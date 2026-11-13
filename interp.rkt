#lang racket/base

;;; interp.rkt — the ELM interpreter (ELM node -> CQL value).
;;;
;;; CQL is compiled to ELM and then RUN by walking the ELM tree — never evaluated from CQL directly.
;;; This is the ELM analog of fhirpath/eval.rkt's fp-eval: a `match` over the prefab ELM structs,
;;; dispatching binary/n-ary/unary operators on their ELM type string. It reuses the FHIRPath System
;;; primitives (decimal, quantity, temporal, equality) and adds CQL's three-valued null handling.
;;; This interpreter is what drives the conformance harness (compile a CQL expression -> ELM -> run ->
;;; compare to the expected value). It runs ELM two ways with one semantics: `eval-elm`, the tree
;;; walker and the reference, and `compile-elm`, a closure compiler that turns each node into a Racket
;;; closure once, so a library evaluated for many patients does no dispatch on its nodes
;;; (`current-elm-evaluator` chooses; tests/evaluator-test.rkt holds them to identical results).
;;;
;;; The algebra the operators rest on lives in interp/, one module per concern, each pure and tested
;;; on its own: base.rkt (the error, the numeric tower, three-valued negation, interval bounds),
;;; temporal.rkt (constructors, components, durations, boundaries, date arithmetic), arith.rkt
;;; (Add … Power over numbers, quantities, uncertainty intervals and moments), compare.rkt (ordering,
;;; equality and equivalence, the null-aware list helpers) and interval.rkt (construction, the
;;; relational operators, membership, set operations, collapse/expand, the timing dispatch). This
;;; module keeps the environment and runtime libraries, the evaluator, the operator CATALOGUE
;;; (`def-op!` entries), the query engine, strings, aggregates, navigation and conversions.

(require racket/match
         racket/contract
         (only-in racket/list take drop last append-map argmax)
         (only-in racket/string string-split)
         "elm/ast.rkt"
         "value.rkt"
         (only-in "error.rkt" cql-error cql-error?)
         (only-in fhirpath/decimal
                  fp-decimal? parse-decimal integer->decimal decimal->exact decimal->string
                  decimal-add decimal-mul decimal-div decimal-negate)
         (only-in fhirpath/value fp-long fp-long? fp-long-v fp-quantity fp-quantity? fp-quantity-value fp-quantity-unit)
         (only-in fhirpath/quantity make-quantity quantity-add)
         (only-in fhirpath/temporal-ops moment moment? parse-fp-date parse-fp-datetime parse-fp-time)
         "representation.rkt"
         (only-in "model/modelinfo.rkt" find-model find-model-by-url model-name model-url find-profile-class model-element-target)
         (only-in "mapping.rkt" parse-mapping map-call map-path map-field map-filter)
         (only-in "code-path.rkt" resolve-code-path code-path-codes)
         ;; the interpreter's algebra, each a module of its own: the numeric base and errors, the
         ;; temporal semantics, arithmetic, comparison and equality, intervals
         "interp/base.rkt" "interp/temporal.rkt" "interp/arith.rkt" "interp/compare.rkt" "interp/interval.rkt")

(provide (contract-out
          [eval-elm    (-> elm-expr? ienv? cql-value?)]  ; run one ELM expression node -> a CQL value (the tree walker)
          [eval-elm/compiled (-> elm-expr? ienv? cql-value?)] ; the same, through the node's compiled closure
          [run-elm     (-> elm-expr? ienv? cql-value?)]  ; the same, by the evaluator current-elm-evaluator names
          [compile-elm (-> elm-expr? procedure?)]        ; ELM node -> (Env -> CqlValue), compiled once per node
          [current-elm-evaluator (parameter/c (or/c 'compiled 'tree))]
          ;; generic-codes : a resource and a code property -> its codes by the generic navigation (the
          ;; reference a retrieve's CodePath is held to, tests/code-path-test.rkt)
          [generic-codes (-> model-node? string? (listof cql-code?))]
          [run-library (-> elm-library? hash?)]     ; def-name(string) -> interpreted CqlValue
          [make-ienv   (->* () (#:defs hash? #:operands hash?) ienv?)]
          [make-rlib   (-> elm-library? hash? (or/c runtime? #f) hash? rlib?)]
          [rlib-value  (-> rlib? string? cql-value?)]
          ;; rlib-eval : one ELM expression in a library's context (its definitions, functions,
          ;; terms and runtime): what an explanation evaluates a definition's parts with
          [rlib-eval   (-> rlib? elm-expr? cql-value?)]
          ;; rlib-call : a library's function applied to values (a measure observation per episode)
          [rlib-call   (-> rlib? string? (listof cql-value?) cql-value?)]
          [runtime     (-> procedure? procedure? (or/c exact-integer? #f) runtime?)]
          [runtime?    (-> any/c boolean?)]
          [rlib?       (-> any/c boolean?)]
          [ienv?       (-> any/c boolean?)]
          [test-equal? (-> any/c any/c boolean?)]))   ; value comparison for the conformance harness

;; ---- interpreter environment ----------------------------------------------
;; defs: name(string) -> promise-of-value ; operands: name(string) -> value ; funcs: name -> elm-function-def ;
;; now: a single evaluation-time moment shared by Now()/Today()/TimeOfDay() so they're self-consistent
(struct ienv (defs operands funcs now lib rt) #:transparent)
(define (make-ienv #:defs [defs (hash)] #:operands [operands (hash)]) (ienv defs operands (hash) (current-moment) #f #f))

;; ---- runtime libraries (execution over data) ---------------------------------
;; A Runtime is what a library runs against: `data` : type-name -> (listof resource jsexpr) (the
;; retrieve source, already scoped to the context patient) and `valueset` : url -> (hash (cons system
;; code) -> #t) | #f (the expansion of a value set; #f = not available, an error when needed).
;; offset: minutes|#f, see current-evaluation-offset. The rest is what depends only on the runtime,
;; whose data and terminology do not change during an evaluation, each computed once:
;;   typed        : type-name -> (listof FhirNode)              a type's resources, typed once
;;   retrieves    : (cons type code-property) -> (hasheq codes-value -> (listof FhirNode))
;;                  a code-filtered retrieve, by the codes value's identity (a ValueSetRef's value is
;;                  the library's term object, shared by every evaluation of it: a missed identity is
;;                  only a recomputation, never a wrong answer)
;;   vocabularies : url -> Vocabulary | #f                       a value set's members (#f: unavailable)
;; A Vocabulary is (vocabulary members codes): members is (hash (cons system code) -> #t); codes is a
;; thunk for the code-only index (code -> #t), built the first time a code without a system is tested.
;; A computation that raises stores nothing, so its error recurs exactly as it would uncached.
(struct runtime (data valueset offset typed retrieves vocabularies)
  #:constructor-name make-runtime-struct #:omit-define-syntaxes)
(struct vocabulary (members codes))
;; runtime : (type -> resources) (url -> members|#f) offset -> Runtime   with empty caches
(define (runtime data valueset offset) (make-runtime-struct data valueset offset (make-hash) (make-hash) (make-hash)))

;; runtime-resources : Runtime model type-name -> (listof ModelNode)   the data's records of a class,
;; read as the model's nodes. The data is asked for the class's name and the model (a source that
;; scopes a patient's records by the model's relationships needs it); a one-argument source is
;; asked for the name alone.
(define (runtime-resources rt m type)
  (hash-ref! (runtime-typed rt) (cons m type)
             (lambda ()
               (define data (runtime-data rt))
               (define qualified (string-append (model-name m) "." type))
               (for/list ([r (in-list (if (procedure-arity-includes? data 2) (data type m) (data type)))])
                 (record->node m r qualified)))))
;; runtime-vocabulary : Runtime url -> Vocabulary | #f
(define (runtime-vocabulary rt url)
  (hash-ref! (runtime-vocabularies rt) url
             (lambda ()
               (define members ((runtime-valueset rt) url))
               (and members
                    (vocabulary members (delay-value (lambda () (for/hash ([k (in-hash-keys members)]) (values (cdr k) #t)))))))))
;; runtime-retrieve : Runtime type code-property codes-value (-> (listof FhirNode)) -> (listof FhirNode)
(define (runtime-retrieve rt type property w compute)
  (define by-codes (hash-ref! (runtime-retrieves rt) (cons type property) make-hasheq))
  (hash-ref! by-codes w compute))

;; An Rlib is a compiled library ready to run: its definitions (memoised promises), functions (by
;; name, overloads in order), parameters, terminology definitions, and the Rlibs of its includes.
;; first-funcs: name -> the first overload (the function table an expression in the library sees),
;; built once instead of per call; overloads: (cons name signature) -> the overload that signature
;; picks (value-independent, so memoised).
(struct rlib (name defs funcs params terms includes rt first-funcs overloads models))

;; make-rlib : elm-library (hash alias -> rlib) runtime (hash name -> value) -> rlib
;; parameter values given by name override the library's defaults (the evaluation's parameters)
;; A LibTables is what make-rlib takes from an ELM library that is the same for every evaluation of
;; it: the functions by name (overloads in declaration order), the first overload of each, the
;; expression definitions, and the static terms (its code systems and value sets, immutable objects
;; in a table make-rlib copies). Built once per library (make-rlib runs per patient; FHIRHelpers alone
;; has dozens of overloads, and rebuilding the table per patient was half of a measure's run time).
(struct libtables (funcs first-funcs defs static-terms models))
(define library-tables-cache (make-ephemeron-hasheq))
;; library-tables : elm-library -> LibTables
(define (library-tables lib)
  (hash-ref! library-tables-cache lib
             (lambda ()
               (define statements (elm-library-statements lib))
               (define fs (filter elm-function-def? statements))
               ;; name -> overloads in declaration order
               (define funcs (for/fold ([h (hash)]) ([s (in-list (reverse fs))])
                               (hash-update h (elm-function-def-name s) (lambda (l) (cons s l)) '())))
               (define static-terms (make-hash))   ; never mutated: make-rlib copies it
               (for ([c (in-list (elm-library-codesystems lib))])
                 (hash-set! static-terms (elm-codesystem-def-name c)
                            (cql-obj "CodeSystem" (hash 'id (elm-codesystem-def-id c) 'version (or (elm-codesystem-def-version c) cql-null)))))
               (for ([v (in-list (elm-library-valuesets lib))])
                 (hash-set! static-terms (elm-valueset-def-name v)
                            (cql-obj "ValueSet" (hash 'id (elm-valueset-def-id v) 'version (or (elm-valueset-def-version v) cql-null)))))
               (libtables funcs (for/hash ([(k v) (in-hash funcs)]) (values k (car v))) (filter elm-def? statements) static-terms
                          (library-models lib)))))
;; library-models : elm-library -> (hash url -> Model)   the data models the library's `using`s name,
;; by the url a Retrieve's dataType carries (`{url}Type`); a model not installed is left out (its
;; retrieve says so when it runs)
(define (library-models lib)
  (for*/hash ([u (in-list (elm-library-usings lib))]
              [m (in-value (find-model (format "~a" (elm-using-local-id u)) (elm-using-version u)))]
              #:when m)
    (values (model-url m) m)))

(define (make-rlib lib includes rt overrides)
  (define now (current-moment))
  (match-define (libtables funcs first-funcs def-statements static-terms models) (library-tables lib))
  (define defs (make-hash)) (define params (make-hash)) (define terms (hash-copy static-terms))
  (define rl (rlib (elm-library-id lib) defs funcs params terms includes rt first-funcs (make-hash) models))
  (define run (elm-runner))   ; the evaluator, chosen once for the library
  (define (env) (ienv defs (hash) first-funcs now rl rt))
  (for ([s (in-list def-statements)])
    (hash-set! defs (elm-def-name s) (delay-value (lambda () (run (elm-def-expression s) (env))))))
  (for ([p (in-list (elm-library-parameters lib))])
    (define n (elm-parameter-def-name p))
    (hash-set! params n (delay-value (lambda ()
                                       (cond [(hash-has-key? overrides n) (hash-ref overrides n)]
                                             [(elm-parameter-def-default p) (run (elm-parameter-def-default p) (env))]
                                             [else cql-null])))))
  (for ([c (in-list (elm-library-codes lib))])
    (hash-set! terms (elm-code-def-name c)
               (delay-value (lambda ()
                              (define cs (run (elm-code-def-codesystem* c) (env)))
                              (cql-code (elm-code-def-id c) (obj-field cs 'id) (obj-field cs 'version) (elm-code-def-display c))))))
  (for ([c (in-list (elm-library-concepts lib))])
    (hash-set! terms (elm-concept-def-name c)
               (delay-value (lambda ()
                              (cql-concept (for/list ([r (in-list (elm-concept-def-codes c))])
                                             (run (elm-term-ref "CodeRef" (elm-term-ref-name r) (elm-term-ref-library r)) (env)))
                                           (elm-concept-def-display c))))))
  rl)
(define (elm-code-def-codesystem* c)
  (define r (elm-code-def-codesystem c))
  (elm-term-ref "CodeSystemRef" (elm-term-ref-name r) (elm-term-ref-library r)))
(define (obj-field o k) (let ([v (hash-ref (cql-obj-fields o) k cql-null)]) (if (cql-null? v) #f v)))

;; in-library : rlib (-> value) -> value   evaluate at the library's runtime's timezone offset (the
;; evaluation request's), else the one already in force
(define (in-library rl thunk)
  (parameterize ([current-evaluation-offset (or (and (rlib-rt rl) (runtime-offset (rlib-rt rl))) (current-evaluation-offset))])
    (thunk)))
;; rlib-env : rlib -> Env   a fresh environment in the library: its definitions and functions, no operands
(define (rlib-env rl) (ienv (rlib-defs rl) (hash) (rlib-first-funcs rl) (current-moment) rl (rlib-rt rl)))

;; rlib-value : rlib name -> value   a definition's value
(define (rlib-value rl name)
  (in-library rl (lambda () (force-value (hash-ref (rlib-defs rl) name (lambda () (fail (format "unknown definition ~a" name))))))))
;; rlib-eval : rlib ELM -> value   one expression in the library's context
(define (rlib-eval rl expr)
  (in-library rl (lambda () ((elm-runner) expr (rlib-env rl)))))
;; rlib-call : rlib name (listof value) -> value   a function of the library applied to values
(define (rlib-call rl name args)
  (in-library rl (lambda () (call-library-function rl name args #f (rlib-env rl) (elm-runner)))))

;; target-lib : env alias|#f -> rlib|#f   the library a qualified reference points into
(define (target-lib env alias)
  (define here (ienv-lib env))
  (cond [(not alias) here]
        [(not here) (fail (format "no library context for ~a" alias))]
        [else (hash-ref (rlib-includes here) alias (lambda () (fail (format "unknown library ~a" alias))))]))
(define (lib-env rl env)
  (ienv (rlib-defs rl) (hash) (rlib-first-funcs rl) (ienv-now env) rl (ienv-rt env)))

;; ---- library driver --------------------------------------------------------
;; run-library : elm-library -> (hash name -> CqlValue)   every expression definition of a library
;; that needs no data, includes or parameters (a `#lang cql` module's `results`)
(define (run-library lib)
  (define statements (elm-library-statements lib))
  (define funcs (for/hash ([s (in-list statements)] #:when (elm-function-def? s))
                  (values (elm-function-def-name s) s)))
  ;; a definition is a memoised thunk, so a reference evaluates it once and may come before it
  (define defs (make-hash))
  (define env (ienv defs (hash) funcs (current-moment) #f #f))
  (define run (elm-runner))
  (for ([s (in-list statements)] #:when (elm-def? s))
    (hash-set! defs (elm-def-name s) (delay-value (lambda () (run (elm-def-expression s) env)))))
  (for/hash ([s (in-list statements)] #:when (elm-def? s))
    (values (elm-def-name s) (force-value (hash-ref defs (elm-def-name s))))))

;; a tiny memoizing thunk (avoids racket/promise's contract friction with our value domain)
(define (delay-value th) (let ([done #f] [v #f]) (lambda () (unless done (set! v (th)) (set! done #t)) v)))
(define (force-value p) (if (procedure? p) (p) p))

;; ---- the evaluator ---------------------------------------------------------
;; An ELM-Expr is one of (see cql/elm/ast.rkt): literal | null | quantity | ratio | nary | unary |
;; interval | list | tuple | if | case | property | expression-ref | operand-ref | function-ref |
;; is | as | to. `eval-elm` is the structural-recursion template over that data definition: one match
;; clause per variant, recurring on sub-expressions; operator variants (nary/unary) delegate to the
;; operator registry via apply-op.
;; eval-elm : ELM-Expr Env -> CqlValue
(define (eval-elm node env)
  (match node
    [(elm-literal vt v) (eval-literal vt v)]
    [(elm-null _) cql-null]
    [(elm-quantity v unit) (make-quantity (or (parse-decimal v) (integer->decimal 0)) (or unit "1"))]
    [(elm-ratio n d) (cql-ratio (eval-elm n env) (eval-elm d env))]
    [(elm-date y mo d) (build-date (ci y env) (ci mo env) (ci d env))]
    [(elm-time h mi s ms) (build-time (ci h env) (ci mi env) (ci s env) (ci ms env))]
    [(elm-datetime y mo d h mi s ms tz)
     (build-datetime (ci y env) (ci mo env) (ci d env) (ci h env) (ci mi env) (ci s env) (ci ms env)
                     (or (cnum tz env)
                         ;; no offset written: the evaluation's (when the runtime sets one and there is a time)
                         (let ([o (current-evaluation-offset)]) (and o h (/ o 60)))))]
    [(elm-component prec operand) (component-from prec (eval-elm operand env))]
    [(elm-between kind prec a b) (between-op kind prec (eval-elm a env) (eval-elm b env))]
    [(elm-timing op prec a b) (timing-op-eval op prec (eval-elm a env) (eval-elm b env))]
    [(elm-query sources lets rels where ret ret-distinct sort agg)
     (run-query sources lets rels where ret ret-distinct sort agg env)]
    [(elm-instance class-type elems) (build-instance class-type (for/list ([te (in-list elems)])
                                                                  (cons (elm-tuple-elem-name te)
                                                                        (eval-elm (elm-tuple-elem-value te) env))))]
    [(elm-nary op operands _rt) (apply-op op operands env)]
    [(elm-unary op operand _rt) (apply-op op (list operand) env)]
    [(elm-interval low high lc hc)
     ;; a closed flag may be an expression (a promoted interval reads the original's flags)
     (define (flag x) (if (boolean? x) x (let ([v (eval-elm x env)]) (and (boolean? v) v))))
     (make-interval-value (and low (eval-elm low env)) (and high (eval-elm high env)) (flag lc) (flag hc))]
    [(elm-list elems) (map (lambda (e) (eval-elm e env)) elems)]
    [(elm-tuple elems)
     (cql-tuple (for/hash ([te (in-list elems)])
                  (values (elm-tuple-elem-name te) (eval-elm (elm-tuple-elem-value te) env))))]
    [(elm-if c t e) (eval-if c t e env)]
    [(elm-case comparand items els) (eval-case comparand items els env)]
    [(elm-property #f path scope) (eval-property (lookup-operand scope env) path)]
    [(elm-property src path _scope) (eval-property (eval-elm src env) path)]
    [(elm-alias-ref name) (lookup-operand name env)]
    [(elm-minmax _which _vt boundary) (eval-elm boundary env)]
    [(elm-query-let-ref name) (lookup-operand name env)]
    [(elm-identifier-ref "$this") (current-sort-row)]
    [(elm-identifier-ref name) (eval-property (current-sort-row) name)]
    [(elm-expression-ref name #f) (force-value (hash-ref (ienv-defs env) name (lambda () (fail (format "unknown reference ~a" name)))))]
    [(elm-expression-ref name lib) (rlib-value (target-lib env lib) name)]
    [(elm-parameter-ref name lib)
     (define rl (target-lib env lib))
     (if rl
         (force-value (hash-ref (rlib-params rl) name (lambda () (fail (format "unknown parameter ~a" name)))))
         (fail (format "parameter ~a has no value" name)))]
    [(elm-term-ref kind name lib)
     (define rl (target-lib env lib))
     (if rl
         (force-value (hash-ref (rlib-terms rl) name (lambda () (fail (format "unknown ~a ~a" kind name)))))
         (fail (format "~a ~a needs a library" kind name)))]
    [(elm-code code sys display)
     (define cs (eval-elm (elm-term-ref "CodeSystemRef" (elm-term-ref-name sys) (elm-term-ref-library sys)) env))
     (cql-code code (obj-field cs 'id) (obj-field cs 'version) display)]
    [(elm-concept codes display) (cql-concept (for/list ([c (in-list codes)]) (eval-elm c env)) display)]
    [(? elm-retrieve?) (eval-retrieve node env)]
    [(elm-in-valueset kind code vs _)
     (define c (eval-elm code env))
     (define v (eval-elm vs env))
     (in-valueset-apply kind c v env)]
    [(elm-precision-op op prec operands)
     (precision-apply op prec (for/list ([o (in-list operands)]) (eval-elm o env)) env)]
    [(elm-operand-ref name) (hash-ref (ienv-operands env) name (lambda () (fail (format "unbound operand ~a" name))))]
    [(elm-function-ref name lib operands sig)
     (define args (map (lambda (o) (eval-elm o env)) operands))
     (if (ienv-lib env)
         (call-library-function (target-lib env lib) name args sig env)
         (eval-function name args env))]
    [(elm-is operand type) (eval-is (eval-elm operand env) type)]
    [(elm-as operand type strict) (as-value (eval-elm operand env) type strict)]
    [(elm-to operand to-type) (convert-to (eval-elm operand env) to-type)]
    [_ (fail (format "cannot evaluate ELM node: ~s" node))]))
(define (type-name-of t) (cond [(string? t) t] [(elm-named-type? t) (elm-named-type-name t)] [else #f]))

;; in-valueset-apply : kind CqlValue CqlValue Env -> Boolean|Null   InValueSet / AnyInValueSet
(define (in-valueset-apply kind c v env)
  (cond [(or (cql-null? c) (cql-null? v)) (if (equal? kind "AnyInValueSet") #f cql-null)]
        [(regexp-match? #rx"^Any" kind) (and (list? c) (ormap (lambda (x) (in-vocabulary? x v env)) c))]
        [else (in-vocabulary? c v env)]))

;; precision-apply : op precision (Listof CqlValue) Env -> CqlValue   CalculateAge[At] in a precision
(define (precision-apply op prec vals env)
  (define p (string->symbol (string-downcase prec)))
  (match* (op vals)
    [("CalculateAgeAt" (list b at)) (between-op 'duration p b at)]
    [("CalculateAge" (list b)) (between-op 'duration p b (component-from 'date (ienv-now env)))]
    [(_ _) (fail (format "unsupported ~a" op))]))

;; as-value : CqlValue type-spec strict? -> CqlValue
;; As is a pass-through, EXCEPT `null as List<T>` which becomes the LIST-TYPED null: Length sees
;; it as empty (0) and scalar membership is false, while list-list inclusion stays null
(define (as-value v type strict)
  (cond [(and (cql-null? v) (elm-list-type? type)) cql-null-list]
        ;; a model's value is cast by its class: the member it is, else null (strict: an error)
        [(and (model-node? v) (type-name-of type))
         => (lambda (tn) (cond [(node-is? v tn) v]
                               [(not (model-type-ref? tn)) v]
                               [strict (fail (format "cannot cast ~a as ~a" (model-node-type v) tn))]
                               [else cql-null]))]
        [else v]))

;; ---- the closure compiler --------------------------------------------------
;; A Compiled is (Env -> CqlValue): an ELM node translated ONCE into a Racket closure. Running it
;; again (for every patient, every query row, every function call) does no dispatch on the node:
;; the `match` over the variant, the operator-registry lookup, a literal's parse and a property
;; path's split all happened when it was compiled. `compile-node` is the same structural recursion
;; as `eval-elm`, one clause per variant, and each clause computes what that clause of `eval-elm`
;; computes, in the same order. The tree walker stays the reference: tests/evaluator-test.rkt holds
;; the two to identical values and identical errors over the conformance suites.
;;
;; Two rules keep it exactly the tree walker's semantics:
;; - errors stay where they were. Compiling never raises: whatever the tree walker raises when it
;;   reaches a node (an unknown operator, a bad literal) is raised by the node's closure when it
;;   runs, so an untaken branch still raises nothing;
;; - only what reads nothing at run time is computed ahead: literals and quantities (no clock, no
;;   evaluation offset, no data, no parameters). Everything else is computed when the closure runs.
;; Queries, function bodies and the operators' own algebra are shared with the tree walker (the query
;; engine and the library-function calls take the evaluator as an argument).

;; current-elm-evaluator : 'compiled | 'tree   which evaluator libraries and run-elm use (the
;; environment variable CQL_ELM_EVALUATOR=tree selects the tree walker for a whole process)
(define current-elm-evaluator
  (make-parameter (if (equal? (getenv "CQL_ELM_EVALUATOR") "tree") 'tree 'compiled)))

;; elm-runner : -> (ELM Env -> CqlValue)   the evaluator current-elm-evaluator names
(define (elm-runner) (if (eq? (current-elm-evaluator) 'tree) eval-elm eval-elm/compiled))

;; run-elm : ELM Env -> CqlValue   one node by the current evaluator
(define (run-elm node env) ((elm-runner) node env))

;; compiled-nodes : node -> Compiled. Ephemeron-keyed: a closure refers to its node, so a weak table
;; would keep every entry alive; with ephemerons an entry goes when its ELM tree does.
(define compiled-nodes (make-ephemeron-hasheq))

;; compile-elm : ELM -> Compiled   memoised per node
(define (compile-elm node)
  (or (hash-ref compiled-nodes node #f)
      (let ([c (compile-node node)])
        (hash-set! compiled-nodes node c)
        c)))

;; eval-elm/compiled : ELM Env -> CqlValue
(define (eval-elm/compiled node env) ((compile-elm node) env))

;; constant : (-> CqlValue) -> Compiled   a value that reads nothing at run time, computed now; when
;; computing it raises (a `fail`, which raises a cql-error value, or any exception), the closure
;; computes it again when it runs, so it raises then, and only if it is reached
(define (constant compute)
  (with-handlers ([(lambda (e) (not (exn:break? e))) (lambda (_) (lambda (env) (compute)))])
    (define v (compute))
    (lambda (env) v)))

;; compile-int / compile-num : ELM|#f -> (Env -> integer|real|#f)   `ci` and `cnum`, compiled
(define (compile-int x)
  (if x
      (let ([c (compile-elm x)]) (lambda (env) (let ([v (c env)]) (if (cql-null? v) #f (->int-val v)))))
      (lambda (env) #f)))
(define (compile-num x)
  (if x
      (let ([c (compile-elm x)]) (lambda (env) (let ([v (c env)]) (if (cql-null? v) #f (num->real v)))))
      (lambda (env) #f)))

;; compile-all : (Listof ELM) -> (Env -> (Listof CqlValue))   evaluated left to right, as `map` does
(define (compile-all nodes)
  (define cs (map compile-elm nodes))
  (lambda (env) (for/list ([c (in-list cs)]) (c env))))

;; compile-node : ELM -> Compiled   the template, clause for clause with eval-elm
(define (compile-node node)
  (match node
    [(elm-literal vt v) (constant (lambda () (eval-literal vt v)))]
    [(elm-null _) (lambda (env) cql-null)]
    [(elm-quantity v unit) (constant (lambda () (make-quantity (or (parse-decimal v) (integer->decimal 0)) (or unit "1"))))]
    [(elm-ratio n d)
     (define cn (compile-elm n)) (define cd (compile-elm d))
     (lambda (env) (let* ([a (cn env)] [b (cd env)]) (cql-ratio a b)))]
    [(elm-date y mo d)
     (define cy (compile-int y)) (define cmo (compile-int mo)) (define cd (compile-int d))
     (lambda (env) (build-date (cy env) (cmo env) (cd env)))]
    [(elm-time h mi s ms)
     (define ch (compile-int h)) (define cmi (compile-int mi)) (define cs (compile-int s)) (define cms (compile-int ms))
     (lambda (env) (build-time (ch env) (cmi env) (cs env) (cms env)))]
    [(elm-datetime y mo d h mi s ms tz)
     (define cy (compile-int y)) (define cmo (compile-int mo)) (define cd (compile-int d)) (define ch (compile-int h))
     (define cmi (compile-int mi)) (define cs (compile-int s)) (define cms (compile-int ms)) (define ctz (compile-num tz))
     (define hour? (and h #t))
     (lambda (env)
       (build-datetime (cy env) (cmo env) (cd env) (ch env) (cmi env) (cs env) (cms env)
                       (or (ctz env)
                           ;; no offset written: the evaluation's (read when it runs, never folded)
                           (let ([o (current-evaluation-offset)]) (and o hour? (/ o 60))))))]
    [(elm-component prec operand)
     (define c (compile-elm operand))
     (lambda (env) (component-from prec (c env)))]
    [(elm-between kind prec a b)
     (define ca (compile-elm a)) (define cb (compile-elm b))
     (lambda (env) (let* ([x (ca env)] [y (cb env)]) (between-op kind prec x y)))]
    [(elm-timing op prec a b)
     (define ca (compile-elm a)) (define cb (compile-elm b))
     (lambda (env) (let* ([x (ca env)] [y (cb env)]) (timing-op-eval op prec x y)))]
    [(elm-query sources lets rels where ret ret-distinct sort agg)
     (lambda (env) (run-query sources lets rels where ret ret-distinct sort agg env eval-elm/compiled))]
    [(elm-instance class-type elems)
     (define fields (for/list ([te (in-list elems)]) (cons (elm-tuple-elem-name te) (compile-elm (elm-tuple-elem-value te)))))
     (lambda (env) (build-instance class-type (for/list ([f (in-list fields)]) (cons (car f) ((cdr f) env)))))]
    [(elm-nary op operands _rt) (compile-op op operands)]
    [(elm-unary op operand _rt) (compile-op op (list operand))]
    [(elm-interval low high lc hc)
     (define cl (and low (compile-elm low))) (define ch (and high (compile-elm high)))
     ;; a closed flag may be an expression (a promoted interval reads the original's flags)
     (define (flag x) (if (boolean? x) (lambda (env) x)
                          (let ([c (compile-elm x)]) (lambda (env) (let ([v (c env)]) (and (boolean? v) v))))))
     (define fl (flag lc)) (define fh (flag hc))
     (lambda (env)
       (let* ([l (and cl (cl env))] [h (and ch (ch env))] [lcv (fl env)] [hcv (fh env)])
         (make-interval-value l h lcv hcv)))]
    [(elm-list elems) (compile-all elems)]
    [(elm-tuple elems)
     (define fields (for/list ([te (in-list elems)]) (cons (elm-tuple-elem-name te) (compile-elm (elm-tuple-elem-value te)))))
     (lambda (env) (cql-tuple (for/hash ([f (in-list fields)]) (values (car f) ((cdr f) env)))))]
    [(elm-if c t e)
     (define cc (compile-elm c)) (define ct (compile-elm t)) (define ce (compile-elm e))
     (lambda (env) (if (eq? (cc env) #t) (ct env) (ce env)))]   ; false or null -> else
    [(elm-case comparand items els) (compile-case comparand items els)]
    [(elm-property #f path scope)
     (define acc (property-accessor path))
     (lambda (env) (acc (lookup-operand scope env)))]
    [(elm-property src path _scope)
     (define cs (compile-elm src)) (define acc (property-accessor path))
     (lambda (env) (acc (cs env)))]
    [(elm-alias-ref name) (lambda (env) (lookup-operand name env))]
    [(elm-minmax _which _vt boundary) (compile-elm boundary)]
    [(elm-query-let-ref name) (lambda (env) (lookup-operand name env))]
    [(elm-identifier-ref "$this") (lambda (env) (current-sort-row))]
    [(elm-identifier-ref name) (lambda (env) (eval-property (current-sort-row) name))]
    [(elm-expression-ref name #f)
     (lambda (env) (force-value (hash-ref (ienv-defs env) name (lambda () (fail (format "unknown reference ~a" name))))))]
    [(elm-expression-ref name lib) (lambda (env) (rlib-value (target-lib env lib) name))]
    [(elm-parameter-ref name lib)
     (lambda (env)
       (define rl (target-lib env lib))
       (if rl
           (force-value (hash-ref (rlib-params rl) name (lambda () (fail (format "unknown parameter ~a" name)))))
           (fail (format "parameter ~a has no value" name))))]
    [(elm-term-ref kind name lib)
     (lambda (env)
       (define rl (target-lib env lib))
       (if rl
           (force-value (hash-ref (rlib-terms rl) name (lambda () (fail (format "unknown ~a ~a" kind name)))))
           (fail (format "~a ~a needs a library" kind name))))]
    [(elm-code code sys display)
     (define ccs (compile-elm (elm-term-ref "CodeSystemRef" (elm-term-ref-name sys) (elm-term-ref-library sys))))
     (lambda (env)
       (define cs (ccs env))
       (cql-code code (obj-field cs 'id) (obj-field cs 'version) display))]
    [(elm-concept codes display)
     (define cc (compile-all codes))
     (lambda (env) (cql-concept (cc env) display))]
    [(? elm-retrieve?)
     (define codes (elm-retrieve-codes node))
     (define cw (and codes (compile-elm codes)))
     (lambda (env) (retrieve node (and cw (lambda () (cw env))) env))]
    [(elm-in-valueset kind code vs _)
     (define cc (compile-elm code)) (define cv (compile-elm vs))
     (lambda (env) (let* ([c (cc env)] [v (cv env)]) (in-valueset-apply kind c v env)))]
    [(elm-precision-op op prec operands)
     (define cs (compile-all operands))
     (lambda (env) (precision-apply op prec (cs env) env))]
    [(elm-operand-ref name)
     (lambda (env) (hash-ref (ienv-operands env) name (lambda () (fail (format "unbound operand ~a" name)))))]
    [(elm-function-ref name lib operands sig)
     (define cs (compile-all operands))
     (lambda (env)
       (define args (cs env))
       (if (ienv-lib env)
           (call-library-function (target-lib env lib) name args sig env eval-elm/compiled)
           (eval-function name args env eval-elm/compiled)))]
    [(elm-is operand type)
     (define c (compile-elm operand))
     (lambda (env) (eval-is (c env) type))]
    [(elm-as operand type strict)
     (define c (compile-elm operand))
     (lambda (env) (as-value (c env) type strict))]
    [(elm-to operand to-type)
     (define c (compile-elm operand))
     (lambda (env) (convert-to (c env) to-type))]
    ;; anything else: the tree walker, which raises its own error for a node it cannot evaluate
    [_ (lambda (env) (eval-elm node env))]))

;; compile-op : op-name (Listof ELM) -> Compiled   the registry entry looked up once. An eager
;; operator gets its operands' values (left to right); And/Or/Implies get the first value and a thunk
;; for the second, over the same logic as the tree walker's; any other lazy operator gets its raw
;; operands, as in apply-op. An unknown operator fails when it runs, before its operands, as apply-op.
(define (compile-op name operands)
  (define o (hash-ref OPS name #f))
  (cond
    [(not o) (lambda (env) (fail (format "unsupported operator ~a" name)))]
    [(and (op-lazy? o) (= 2 (length operands)) (lazy-logic name))
     => (lambda (logic)
          (define ca (compile-elm (car operands))) (define cb (compile-elm (cadr operands)))
          (lambda (env) (logic (ca env) (lambda () (cb env)))))]
    [(op-lazy? o) (define impl (op-impl o)) (lambda (env) (impl operands env))]
    [else
     (define impl (op-impl o))
     (match (map compile-elm operands)
       ['() (lambda (env) (impl '() env))]
       [(list a) (lambda (env) (impl (list (a env)) env))]
       [(list a b) (lambda (env) (let* ([x (a env)] [y (b env)]) (impl (list x y) env)))]
       [cs (lambda (env) (impl (for/list ([c (in-list cs)]) (c env)) env))])]))

;; lazy-logic : op-name -> (CqlValue (-> CqlValue) -> Boolean|Null) | #f   the lazy operators whose
;; registered implementation is the three-valued logic over two operands
(define (lazy-logic name)
  (case name [("And") and-values] [("Or") or-values] [("Implies") implies-values] [else #f]))

;; compile-case : ELM|#f (Listof elm-case-item) ELM|#f -> Compiled   eval-case, compiled
(define (compile-case comparand items els)
  (define ccmp (and comparand (compile-elm comparand)))
  (define arms (for/list ([it (in-list items)])
                 (cons (compile-elm (elm-case-item-when it)) (compile-elm (elm-case-item-then it)))))
  (define cels (and els (compile-elm els)))
  (lambda (env)
    (define cmp (and ccmp (ccmp env)))
    (let loop ([as arms])
      (cond
        [(null? as) (if cels (cels env) cql-null)]
        [else (define w ((car (car as)) env))
              (define hit (if ccmp (cql-eq? cmp w) (eq? w #t)))
              (if hit ((cdr (car as)) env) (loop (cdr as)))]))))

;; ---- functions with overloads -----------------------------------------------
;; call-library-function : rlib name args signature env -> value   the overload whose operand types
;; are the call's signature, else the first whose types the argument values fit
;; run evaluates the body (the tree walker, or the compiled evaluator). The overload a signature picks
;; does not depend on the argument values, so it is memoised in the rlib.
(define (call-library-function rl name args sig env [run eval-elm])
  (define cands (hash-ref (rlib-funcs rl) name (lambda () (fail (format "unknown function ~a" name)))))
  (define (types f) (map elm-operand-def-type (elm-function-def-operands f)))
  (define f (or (and (null? (cdr cands)) (car cands))
                (and sig (hash-ref! (rlib-overloads rl) (cons name sig)
                                    (lambda () (for/first ([c (in-list cands)] #:when (equal? (types c) sig)) c))))
                (for/first ([c (in-list cands)]
                            #:when (and (= (length (types c)) (length args)) (andmap value-fits? args (types c))))
                  c)
                (fail (format "no overload of ~a fits the arguments" name))))
  (define body (elm-function-def-expression f))
  (when (eq? body 'external) (fail (format "external function ~a has no implementation" name)))
  (define env* (struct-copy ienv (lib-env rl env)
                            [operands (for/fold ([h (hash)]) ([o (in-list (elm-function-def-operands f))] [a (in-list args)])
                                        (hash-set h (elm-operand-def-name o) a))]))
  (run body env*))

;; value-fits? : value type-spec -> boolean   (null fits anything)
(define (value-fits? v t)
  (cond
    [(cql-null? v) #t]
    [(elm-named-type? t)
     (define n (elm-named-type-name t))
     (cond [(model-type-ref? n) (and (model-node? v) (node-is? v n))]
           [(member (simple-name n) '("Any")) #t]
           [else (value-is-type? v n)])]
    [(elm-list-type? t) (and (list? v) (andmap (lambda (x) (value-fits? x (elm-list-type-element t))) v))]
    [(elm-interval-type? t) (cql-interval? v)]
    [(elm-choice-type? t) (ormap (lambda (c) (value-fits? v c)) (elm-choice-type-types t))]
    [else #t]))

;; ---- retrieve and terminology ------------------------------------------------
;; eval-retrieve : elm-retrieve env -> (listof FhirNode)   the data source's resources of the type,
;; filtered by the code property against the terminology (a value set: membership; codes: ~)
(define (eval-retrieve r env)
  (retrieve r (and (elm-retrieve-codes r) (lambda () (eval-elm (elm-retrieve-codes r) env))) env))

;; retrieve : elm-retrieve (or #f (-> CqlValue)) Env -> (listof FhirNode)   `want` evaluates the
;; retrieve's codes (#f: none). A type's resources and a code-filtered retrieve are computed once per
;; runtime: the same retrieve in many definitions reads and filters once.
(define (retrieve r want env)
  (define rt (or (ienv-rt env) (fail "a retrieve needs a data source")))
  (define-values (m type) (retrieve-class (elm-retrieve-data-type r) env))
  (define all (runtime-resources rt m type))
  (cond
    [(not want) all]
    [else
     (define w (want))
     (define property (elm-retrieve-code-property r))
     (define by-value-set (and (equal? (elm-retrieve-code-comparator r) "~") (cql-obj? w) (obj-field w 'id)
                               (mapped-value-set-reader (elm-retrieve-template-id r) property)))
     (if by-value-set
         ;; the profile maps a ValueSet at the code property (QI-Core's NotDone profiles: the value set
         ;; the action was not done for): `~` a ValueSet holds when the mapped value set is that one
         (runtime-retrieve rt (list m type '~) property w
                           (lambda () (filter (lambda (n) (member (obj-field w 'id) (by-value-set n))) all)))
         (runtime-retrieve rt (cons m type) property w (lambda () (filter (code-filter m type property w env) all))))]))

;; mapped-value-set-reader : template-id property -> (ModelNode -> (listof url))|#f   when the profile
;; (a class of an installed mapping model, by identifier) maps a System.ValueSet alternative at the
;; property, the reader of that alternative's path over the target data — the urls it names (the
;; mapping's function, a ValueSet constructor from its url, is the reading of the url)
(define value-set-readers (make-hash))
(define (mapped-value-set-reader template-id property)
  (hash-ref! value-set-readers (cons template-id property)
             (lambda ()
               (define found (and template-id property (find-profile-class template-id)))
               (define target (and found (model-element-target (car found) (cdr found) property)))
               (define term (and target (for/first ([alt (in-list (parse-mapping target))]
                                                    #:when (equal? (car alt) "System.ValueSet"))
                                          (cdr alt))))
               (define path (let unwrap ([t term]) (match t [(map-call _ a) (unwrap a)] [(map-path 'parent steps) steps] [_ #f])))
               (and path (lambda (n) (filter string? (map scalar-of (walk-mapped-path (list n) path))))))))
;; scalar-of : value -> value   a record written as a bare scalar (FHIR's uri, canonical) as its System
;; value, through its model's scalar element
(define (scalar-of v)
  (define s (and (model-node? v) (not (hash? (model-node-data v)))
                 (representation-scalar-element (model-representation (model-node-model v)))))
  (if s (node-property v s) v))
;; walk-mapped-path : (listof value) steps -> (listof value)   a mapping path over data: a field of
;; each item (lists flattened; a System value's own `value` is itself), a filter keeping the items
;; whose condition paths read the given text
(define (walk-mapped-path vs steps)
  (define (field v f)
    (cond [(model-node? v) (let ([r (node-property v f)]) (cond [(list? r) r] [(cql-null? r) '()] [else (list r)]))]
          [(and (string? v) (equal? f "value")) (list v)]
          [else '()]))
  (for/fold ([vs vs]) ([s (in-list steps)])
    (match s
      [(map-field f) (append-map (lambda (v) (field v f)) vs)]
      [(map-filter conds)
       (filter (lambda (v) (for/and ([c (in-list conds)])
                             (member (cdr c) (map scalar-of (walk-mapped-path (list v) (map map-field (car c)))))))
               vs)])))

;; retrieve-class : data-type Env -> (values Model class-name)   `{url}Type`: the model the library's
;; `using` names with that url (version and all), else any installed model with it
(define (retrieve-class data-type env)
  (define url (let ([x (regexp-match #rx"^{([^}]*)}" data-type)]) (and x (cadr x))))
  (define lib (ienv-lib env))
  (define m (or (and url lib (hash-ref (rlib-models lib) url #f))
                (and url (find-model-by-url url))
                (fail (format "no data model is installed for ~a" data-type))))
  (values m (simple-name data-type)))

;; model-type-ref? : type-name -> boolean   `{url}X` where url is a data model's (not System's)
(define model-urls (make-weak-hash))
(define (model-type-ref? n)
  (define url (let ([x (regexp-match #rx"^{([^}]*)}" n)]) (and x (cadr x))))
  (and url (hash-ref! model-urls url (lambda () (and (find-model-by-url url) #t)))))

;; code-filter : model type code-property codes-value Env -> (ModelNode -> Boolean)   whether a record's
;; codes at the property match the codes value: a value set by membership, a list of codes by `~`.
;; The codes are read along the property's CodePath (code-path.rkt: resolved once against the model)
;; when the model fixes its shape, else by the generic navigation.
(define (code-filter m type property w env)
  (define steps (string-split property "."))
  (define path (resolve-code-path m (string-append (model-name m) "." type) property))
  (define (codes n)
    (define c (and path (code-path-codes path (model-node-data n))))
    (if (list? c) c (node-codes (navigate-path n steps))))
  (define matches?
    (cond [(cql-obj? w) (lambda (have) (ormap (lambda (c) (in-vocabulary? c w env)) have))]
          [(list? w) (lambda (have) (for*/or ([c (in-list have)] [x (in-list w)]) (code-match? c x)))]
          [else (lambda (_have) #f)]))
  (lambda (n) (matches? (codes n))))
(define (code-match? c w)
  (for/or ([x (in-list (node-codes w))])
    (and (equal? (cql-code-code c) (cql-code-code x))
         (or (not (cql-code-system x)) (not (cql-code-system c)) (equal? (cql-code-system c) (cql-code-system x))))))
(define (generic-codes n property) (node-codes (navigate-path n (string-split property "."))))
(define (navigate-path v steps)
  (for/fold ([v v]) ([s (in-list steps)])
    (cond [(list? v) (append-map (lambda (x) (let ([r (navigate-path x (list s))]) (if (list? r) r (list r)))) v)]
          [(model-node? v) (node-property-all v s)]
          [else cql-null])))

;; in-vocabulary? : code-ish ValueSet|CodeSystem-obj env -> boolean   membership by (system, code);
;; a String matches the code in any system. An unavailable value set is an error, never false.
(define (in-vocabulary? c vs env)
  (define rt (or (ienv-rt env) (fail "terminology membership needs a terminology source")))
  (define url (obj-field vs 'id))
  (define v (or (runtime-vocabulary rt url) (fail (format "value set ~a is not available" url))))
  (for/or ([x (in-list (node-codes c))])
    (if (cql-code-system x)
        (hash-ref (vocabulary-members v) (cons (cql-code-system x) (cql-code-code x)) #f)
        ;; a code without a system matches a member with that code in any system
        (hash-ref ((vocabulary-codes v)) (cql-code-code x) #f))))

;; query aliases and lets live in the operand table (OperandRef, AliasRef and QueryLetRef all read it)
(define (lookup-operand name env)
  (hash-ref (ienv-operands env) name (lambda () (fail (format "unbound alias ~a" name)))))

;; the row a ByExpression/ByColumn sort key is evaluated against (IdentifierRef resolves on it)
(define current-sort-row (make-parameter #f))

;; ---- literals --------------------------------------------------------------
(define (eval-literal vt v)
  (case vt
    [("Integer") (string->number v)]
    [("Long") (fp-long (string->number v))]
    [("Decimal") (or (parse-decimal v) (fail (format "bad decimal ~a" v)))]
    [("String") v]
    [("Boolean") (string=? v "true")]
    [("Date") (or (parse-fp-date v) (fail (format "bad date ~a" v)))]
    [("DateTime") (or (parse-fp-datetime v) (fail (format "bad dateTime ~a" v)))]
    [("Time") (or (parse-fp-time v) (fail (format "bad time ~a" v)))]
    [else (fail (format "unsupported literal type ~a" vt))]))

;; ---- temporal constructors / components ------------------------------------
;; ci : (or ELM #f) Env -> (or exact-integer #f)   evaluate a component operand to an integer, or #f
(define (ci x env) (and x (let ([v (eval-elm x env)]) (if (cql-null? v) #f (->int-val v)))))

;; cnum : (or ELM #f) Env -> (or real #f)   evaluate a numeric operand (e.g. a tz-offset in hours)
(define (cnum x env) (and x (let ([v (eval-elm x env)]) (if (cql-null? v) #f (num->real v)))))

;; build-instance : String (Listof (cons Symbol CqlValue)) -> CqlValue
;; the known System class selectors build their carrier; anything else is a structured tuple
(define (build-instance class-type* fields)
  ;; the System namespace is optional: "{urn:hl7-org:elm-types:r1}Code" = "System.Code" = "Code"
  (define class-type (let ([m (regexp-match #rx"^{urn:hl7-org:elm-types:r1}(.*)$" class-type*)]) (if m (cadr m) class-type*)))
  (define (f k) (cond [(assq k fields) => cdr] [else cql-null]))
  (define (fs k) (let ([v (f k)]) (if (string? v) v #f)))
  (case class-type
    [("Code" "System.Code") (cql-code (fs 'code) (fs 'system) (fs 'version) (fs 'display))]
    [("Concept" "System.Concept")
     (cql-concept (let ([c (f 'codes)]) (cond [(list? c) c] [(cql-code? c) (list c)] [else '()])) (fs 'display))]
    [("Quantity" "System.Quantity")
     (let ([v (f 'value)]) (if (num? v) (fp-quantity (->dec v) (or (fs 'unit) "1") #f) cql-null))]
    [else (cql-obj class-type (for/hash ([kv (in-list fields)]) (values (car kv) (cdr kv))))]))

;; singleton from : the sole element of a list (empty -> null, >1 -> error)
(define (singleton-from x)
  (cond [(cql-null? x) cql-null] [(not (list? x)) cql-null]
        [(null? x) cql-null] [(null? (cdr x)) (car x)] [else (fail "List contains more than one element")]))

;; ---- operator registry -----------------------------------------------------
;; The operator catalogue is DATA (the HTDP move): the interpreter core stays a small structural
;; recursion over ELM nodes, and operator application is one O(1) hash lookup — the CQL analog of
;; fhirpath/functions.rkt's fn registry.
;;
;; An Op is (op Boolean Impl):
;;   - eager (lazy? = #f): Impl is (Listof CqlValue) Env -> CqlValue  (operands pre-evaluated)
;;   - lazy  (lazy? = #t): Impl is (Listof ELM-node) Env -> CqlValue  (Impl evaluates what it needs,
;;                          so And/Or/Implies can short-circuit on a decisive operand)
(struct op (lazy? impl) #:transparent)
(define OPS (make-hash))                                  ; op-name(string) -> op
(define (def-op! name lazy? impl) (hash-set! OPS name (op lazy? impl)))
(define (op-count) (hash-count OPS))                      ; coverage introspection

;; apply-op : String (Listof ELM-node) Env -> CqlValue
(define (apply-op name raw-operands env)
  (define o (hash-ref OPS name (lambda () (fail (format "unsupported operator ~a" name)))))
  (if (op-lazy? o)
      ((op-impl o) raw-operands env)
      ((op-impl o) (map (lambda (x) (eval-elm x env)) raw-operands) env)))

;; registration shorthands: unary (one value), nary (the value list), lazy (raw ELM operands + env)
(define (def-unary! name f) (def-op! name #f (lambda (vals _e) (f (car vals)))))
(define (def-nary!  name f) (def-op! name #f (lambda (vals _e) (f vals))))
(define (def-lazy!  name f) (def-op! name #t f))

;; ---- unary operators -------------------------------------------------------
(def-unary! "Negate"
  (lambda (x) (cond [(cql-null? x) cql-null]
                    [(exact-integer? x) (- x)] [(fp-long? x) (fp-long (- (fp-long-v x)))]
                    [(fp-decimal? x) (decimal-negate x)]
                    [(fp-quantity? x) (fp-quantity (decimal-negate (->dec (fp-quantity-value x))) (fp-quantity-unit x) #f)]
                    [else (fail "cannot negate")])))

(def-unary! "Not"     (lambda (x) (three-not x)))
(def-unary! "ToList"  (lambda (x) (if (cql-null? x) '() (list x))))
(def-unary! "Exists"  (lambda (x) (and (list? x) (pair? (drop-nulls x)))))  ; true iff a non-null element exists
(def-unary! "IsNull"  cql-null?)
(def-unary! "IsTrue"  (lambda (x) (eq? x #t)))
(def-unary! "IsFalse" (lambda (x) (eq? x #f)))
(def-unary! "Abs"     (lambda (x) (cond [(cql-null? x) cql-null]
                                        [(fp-quantity? x) (fp-quantity (dec-abs (->dec (fp-quantity-value x))) (fp-quantity-unit x) #f)]
                                        [else (num-map x abs (lambda (d) (dec-abs d)))])))
(def-unary! "Ceiling" (lambda (x) (nn x (lambda () (int-guard (ceil-int x))))))
(def-unary! "Floor"   (lambda (x) (nn x (lambda () (int-guard (floor-int x))))))
(def-unary! "Truncate" (lambda (x) (nn x (lambda () (int-guard (truncate-int x))))))
(def-unary! "Predecessor" (lambda (x) (step-or-fail x -1)))
(def-unary! "Successor"   (lambda (x) (step-or-fail x 1)))
(def-unary! "ToDate"      (lambda (x) (to-temporal x 'date)))
(def-unary! "ToDateTime"  (lambda (x) (to-temporal x 'datetime)))
(def-unary! "ToTime"      (lambda (x) (to-temporal x 'time)))

;; nullary Now/Today/TimeOfDay read the single evaluation-time moment from the env
(def-unary! "Precision" (lambda (x) (precision-of x)))

(def-nary!  "LowBoundary"  (lambda (vs) (boundary (car vs) (cadr vs) #t)))
(def-nary!  "HighBoundary" (lambda (vs) (boundary (car vs) (cadr vs) #f)))
(def-op! "Now"       #f (lambda (_vs env) (or (ienv-now env) cql-null)))
(def-op! "Today"     #f (lambda (_vs env) (component-from 'date (ienv-now env))))
(def-op! "TimeOfDay" #f (lambda (_vs env) (component-from 'time (ienv-now env))))
(def-unary! "Ln"      (lambda (x) (nn x (lambda () (let ([v (num->real x)])
                                                     (cond [(zero? v) (fail "Ln(0) overflows")]   ; -inf
                                                           [(< v 0) cql-null]                     ; NaN
                                                           [else (safe-float->dec (log (exact->inexact v)))]))))))
(def-unary! "Exp"     (lambda (x) (nn x (lambda () (safe-float->dec (exp (exact->inexact (num->real x))))))))
(def-unary! "ToString" (lambda (x) (cond [(cql-null? x) cql-null]
                                         [(moment? x) (moment->plain-string x)]
                                         [else (render x)])))
(def-unary! "ToInteger" (lambda (x) (to-integer x)))
(def-unary! "ToLong"  (lambda (x) (if (cql-null? x) cql-null (fp-long (->int-val x)))))
(def-unary! "ToDecimal" (lambda (x) (cond [(cql-null? x) cql-null] [(string? x) (or (parse-decimal x) cql-null)]
                                          [(num? x) (->dec x)] [else cql-null])))
(def-unary! "ToBoolean" (lambda (x) (to-boolean x)))
(def-unary! "ToQuantity" (lambda (x) (to-quantity x)))
(def-unary! "Upper"   (lambda (x) (str-map x string-upcase)))
(def-unary! "Lower"   (lambda (x) (str-map x string-downcase)))
(def-unary! "Count"   (lambda (x) (if (list? x) (length (drop-nulls x)) cql-null)))
(def-unary! "Length"  (lambda (x) (cond [(string? x) (string-length x)] [(list? x) (length x)]
                                        [(cql-null-list? x) 0]           ; a list-typed null is empty
                                        [(cql-null? x) cql-null] [else (fail "Length")])))
(def-unary! "First"   (lambda (x) (if (and (list? x) (pair? x)) (car x) cql-null)))
(def-unary! "Last"    (lambda (x) (if (and (list? x) (pair? x)) (last x) cql-null)))
(def-unary! "Distinct" (lambda (x) (if (list? x) (dedup x) cql-null)))
(def-unary! "Sum"     (lambda (x) (aggregate x (lambda (a b) (arith + decimal-add a b)) 0)))
(def-unary! "Min"     (lambda (x) (aggregate-cmp x -1)))
(def-unary! "Max"     (lambda (x) (aggregate-cmp x 1)))
(def-unary! "Avg"     (lambda (x) (avg x)))

;; interval accessors / operators
(def-unary! "Start"   (lambda (x) (iv-start x)))

(def-unary! "End"     (lambda (x) (iv-end x)))
(def-unary! "Width"   (lambda (x) (iv-width x)))
(def-unary! "PointFrom" (lambda (x) (iv-point x)))
(def-unary! "SingletonFrom" (lambda (x) (singleton-from x)))

;; collapse X [per q]: the per-quantity is accepted; intervals merge when they overlap or meet (the
;; per granularity is not applied separately — at the day-level data the measures use, the same)
(def-nary! "Collapse" (lambda (xs) (collapse-intervals (car xs))))

(def-nary!  "Expand"  (lambda (vs) (expand-interval (car vs) (and (pair? (cdr vs)) (cadr vs)))))

;; list operators (Skip, Take and Tail are Slices: the translator writes them so, ELM has no others)
(def-nary!  "Slice"   (lambda (vs) (lst-slice (car vs)
                                              (if (>= (length vs) 2) (cadr vs) cql-null)
                                              (if (>= (length vs) 3) (caddr vs) cql-null))))
(def-unary! "descendents" (lambda (x) cql-null))    ; structural traversal: null/scalars have none
(def-unary! "ToConcept" (lambda (x) (cond [(cql-null? x) cql-null]
                                          [(cql-code? x) (cql-concept (list x) (cql-code-display x))]
                                          [(cql-concept? x) x] [else cql-null])))
(def-unary! "ToCode"    (lambda (x) (if (cql-code? x) x cql-null)))
(def-nary!  "IndexOf" (lambda (vs) (lst-index-of (car vs) (cadr vs))))
(def-unary! "Flatten" (lambda (x) (flatten-list x)))

;; aggregate functions
(def-unary! "AllTrue" (lambda (x) (all-true x)))

(def-unary! "AnyTrue" (lambda (x) (any-true x)))
(def-unary! "Product" (lambda (x) (aggregate x (lambda (a b) (arith * decimal-mul a b)) 1)))
(def-unary! "Mode"    (lambda (x) (mode x)))
(def-unary! "Median"  (lambda (x) (median x)))
(def-unary! "Variance" (lambda (x) (variance x #f)))
(def-unary! "StdDev"  (lambda (x) (stddev x #f)))
(def-unary! "PopulationVariance" (lambda (x) (variance x #t)))
(def-unary! "PopulationStdDev" (lambda (x) (stddev x #t)))
(def-unary! "GeometricMean" (lambda (x) (geometric-mean x)))

;; string functions
(def-nary!  "Split"   (lambda (vs) (str-split (car vs) (cadr vs))))

(def-nary!  "LastPositionOf" (lambda (vs) (last-position-of (car vs) (cadr vs))))
(def-nary!  "Matches" (lambda (vs) (str2 vs (lambda (s p) (regexp-match? (pregexp (string-append "^(?:" p ")$")) s)))))
(def-nary!  "ReplaceMatches"
  (lambda (vs) (if (ormap cql-null? vs) cql-null
                   ;; the CQL replacement string uses \x escapes for literals; unescape, then quote for
                   ;; Racket's replacement syntax (where & and \ are special)
                   (regexp-replace* (pregexp (cadr vs)) (car vs)
                                    (regexp-replace-quote (regexp-replace* #px"\\\\(.)" (caddr vs) "\\1"))))))

;; Message(source, condition, code, severity, message): returns source; an 'Error' severity with a
;; true condition raises (per spec, Error messages halt evaluation).
(def-nary!  "Message"
  (lambda (vs)
    (define cond? (and (>= (length vs) 2) (eq? (cadr vs) #t)))
    (define severity (and (>= (length vs) 4) (list-ref vs 3)))
    (if (and cond? (equal? severity "Error"))
        (fail (format "Message error: ~a" (if (>= (length vs) 5) (list-ref vs 4) "")))
        (car vs))))
(define (nn x th) (if (cql-null? x) cql-null (th)))
(define (num-map x int-f dec-f)
  (cond [(cql-null? x) cql-null] [(int-like? x) (wrap-int (int-f (->int x)) (fp-long? x))]
        [(fp-decimal? x) (dec-f x)] [else (fail "numeric expected")]))
(define (str-map x f) (if (string? x) (f x) cql-null))

;; ---- n-ary / binary operators ----------------------------------------------
;; arithmetic (Add/Subtract also do temporal duration arithmetic when the left operand is a moment)
(def-nary! "Add"      (lambda (vs) (bin vs add-values)))

(def-nary! "Subtract" (lambda (vs) (bin vs sub-values)))
(def-nary! "Multiply" (lambda (vs) (bin vs mul-values)))
(def-nary! "Divide"   (lambda (vs) (bin vs divide)))
(def-nary! "TruncatedDivide" (lambda (vs) (bin vs trunc-div)))
(def-nary! "Modulo"   (lambda (vs) (bin vs modulo-op)))
(def-nary! "Power"    (lambda (vs) (bin vs power)))
(def-nary! "Log"      (lambda (vs) (bin vs (lambda (a b) (let ([av (num->real a)] [bv (num->real b)])
                                                           (if (or (<= av 0) (<= bv 0) (= bv 1)) cql-null
                                                               (safe-float->dec (/ (log (exact->inexact av)) (log (exact->inexact bv))))))))))
(def-nary! "Round"    (lambda (vs) (round-op vs)))
;; ConvertQuantity(q, unit): q expressed in `unit` (UCUM; calendar words read as their UCUM units, as
;; FHIR data write them: 90 'days' -> 90 'd'); null when either is null or the units do not convert
(def-nary! "ConvertQuantity"
  (lambda (vs)
    (match vs
      [(list (? fp-quantity? q) (? string? u))
       (define (ucum x) (let ([d (duration-unit x)]) (if d (hash-ref ucum-of-duration d) x)))
       (define r (quantity-add (make-quantity (parse-decimal "0") (ucum u))
                               (fp-quantity (fp-quantity-value q) (ucum (fp-quantity-unit q)) #f)))
       (if r (fp-quantity (fp-quantity-value r) u #f) cql-null)]
      [_ cql-null])))
(define ucum-of-duration
  (hash "year" "a" "month" "mo" "week" "wk" "day" "d" "hour" "h" "minute" "min" "second" "s" "millisecond" "ms"))

;; string
(def-nary! "Concatenate" (lambda (vs) (concat vs)))

(def-nary! "Combine"  (lambda (vs) (combine vs)))
(def-nary! "Substring" (lambda (vs) (substring-op vs)))
(def-nary! "StartsWith" (lambda (vs) (str2 vs (lambda (s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p))))))
(def-nary! "EndsWith"   (lambda (vs) (str2 vs (lambda (s p) (and (>= (string-length s) (string-length p)) (string=? (substring s (- (string-length s) (string-length p))) p))))))
(def-nary! "PositionOf" (lambda (vs) (position-of vs)))
(def-nary! "Indexer"  (lambda (vs) (indexer vs)))

;; comparison / equality
(def-nary! "Equal"    (lambda (vs) (equal-op vs)))

(def-nary! "Equivalent" (lambda (vs) (equivalent vs)))
(def-nary! "Less"     (lambda (vs) (compare-op vs (lambda (c) (< c 0)))))
(def-nary! "Greater"  (lambda (vs) (compare-op vs (lambda (c) (> c 0)))))
(def-nary! "LessOrEqual" (lambda (vs) (compare-op vs (lambda (c) (<= c 0)))))
(def-nary! "GreaterOrEqual" (lambda (vs) (compare-op vs (lambda (c) (>= c 0)))))

;; logical (three-valued, short-circuit on a decisive operand — also avoids errors in unreached branches)
(def-lazy! "And"      (lambda (ops env) (and-lazy (car ops) (cadr ops) env)))

(def-lazy! "Or"       (lambda (ops env) (or-lazy (car ops) (cadr ops) env)))
(def-nary! "Xor"      (lambda (vs) (three-xor vs)))
(def-lazy! "Implies"  (lambda (ops env) (implies-lazy (car ops) (cadr ops) env)))

;; membership / set
(def-nary! "In"       (lambda (vs) (in-op (car vs) (cadr vs))))

(def-nary! "Contains" (lambda (vs) (in-op (cadr vs) (car vs))))
(def-nary! "Union"    (lambda (vs) (set-op vs 'union)))
(def-nary! "Intersect" (lambda (vs) (set-op vs 'intersect)))
(def-nary! "Except"   (lambda (vs) (set-op vs 'except)))

;; null
(def-nary! "Coalesce" (lambda (vs) (coalesce vs)))

;; ---- strings ---------------------------------------------------------------
(define (concat vals)
  (if (ormap cql-null? vals) cql-null (apply string-append (map ->str vals))))
(define (combine vals)
  ;; Combine(list) or Combine(list, sep) ; an empty (or all-null) list -> null
  (define lst (car vals))
  (define sep (if (and (pair? (cdr vals)) (string? (cadr vals))) (cadr vals) ""))
  (cond [(not (list? lst)) cql-null]
        [(null? (drop-nulls lst)) cql-null]
        [else (join (map ->str (drop-nulls lst)) sep)]))
;; a model's primitive (FHIR's `given`, a list of FHIR.string where no conversion was written) joins
;; as its System value, not its printed form (`Combine(name.given, ' ')` gave `"Ann" "Marie"`)
(define (->str x) (let ([v (scalar-of x)]) (if (string? v) v (render v))))
(define (join xs sep) (cond [(null? xs) ""] [(null? (cdr xs)) (car xs)] [else (string-append (car xs) sep (join (cdr xs) sep))]))
(define (str2 vals f) (bin vals (lambda (a b) (and (string? a) (string? b) (f a b)))))
(define (substring-op vals)
  (define s (car vals))
  (cond [(cql-null? s) cql-null]
        [(cql-null? (cadr vals)) cql-null]
        [else (define start (->int-val (cadr vals)))
              (define len (and (>= (length vals) 3) (not (cql-null? (caddr vals))) (->int-val (caddr vals))))
              (define n (string-length s))
              (cond [(and (= start 0) (= n 0)) ""]
                    [(or (< start 0) (>= start n)) cql-null]
                    [len (substring s start (min n (+ start (max 0 len))))]
                    [else (substring s start)])]))
(define (position-of vals)
  ;; PositionOf(pattern, string) -> 0-based index or -1
  (define pat (car vals)) (define s (cadr vals))
  (if (or (cql-null? pat) (cql-null? s)) cql-null
      (let ([i (find-substring s pat)]) (or i -1))))
(define (find-substring s pat)
  (define n (string-length s)) (define m (string-length pat))
  (let loop ([i 0]) (cond [(> (+ i m) n) #f] [(string=? (substring s i (+ i m)) pat) i] [else (loop (add1 i))])))
(define (indexer vals)
  (define x (car vals)) (define i (cadr vals))
  (cond [(or (cql-null? x) (cql-null-list? x) (cql-null? i)) cql-null]
        [(string? x) (if (and (>= (->int i) 0) (< (->int i) (string-length x))) (string (string-ref x (->int i))) cql-null)]
        [(list? x) (if (and (>= (->int i) 0) (< (->int i) (length x))) (list-ref x (->int i)) cql-null)]
        [else (fail "Indexer")]))

;; and/or/implies are LAZY: they take the raw ELM operands + env and short-circuit on a decisive
;; operand (false for And, true for Or, false-antecedent for Implies), so an error or expensive
;; computation in the unreached branch never fires. Result is CQL three-valued.
;; and-lazy : ELM ELM Env -> Boolean|Null
(define (and-lazy a b env) (and-values (eval-elm a env) (lambda () (eval-elm b env))))
(define (or-lazy a b env) (or-values (eval-elm a env) (lambda () (eval-elm b env))))
;; implies-lazy : ELM ELM Env -> Boolean|Null   (false antecedent => true, without evaluating b)
(define (implies-lazy a b env) (implies-values (eval-elm a env) (lambda () (eval-elm b env))))

;; and-values / or-values / implies-values : CqlValue (-> CqlValue) -> Boolean|Null   the logic itself,
;; given the first operand's value and a thunk for the second (called only when it can decide);
;; shared by the tree walker above and the compiled evaluator
(define (and-values av next)
  (cond [(eq? av #f) #f]
        [else (define bv (next))
              (cond [(eq? bv #f) #f] [(and (eq? av #t) (eq? bv #t)) #t] [else cql-null])]))
(define (or-values av next)
  (cond [(eq? av #t) #t]
        [else (define bv (next))
              (cond [(eq? bv #t) #t] [(and (eq? av #f) (eq? bv #f)) #f] [else cql-null])]))
(define (implies-values av next)
  (cond [(eq? av #f) #t]
        [else (define bv (next))
              (cond [(eq? bv #t) #t] [(and (eq? av #t) (eq? bv #f)) #f] [else cql-null])]))

;; coalesce : (Listof CqlValue) -> CqlValue   first non-null argument; the single-LIST form
;; coalesces the list's elements (Coalesce({null, 'a'}) = 'a')
(define (coalesce vals)
  (define vs (if (and (= 1 (length vals)) (list? (car vals))) (car vals) vals))
  (let loop ([vs vs]) (cond [(null? vs) cql-null] [(cql-null? (car vs)) (loop (cdr vs))] [else (car vs)])))

;; ---- aggregates ------------------------------------------------------------
;; aggregate : CqlValue (CqlValue CqlValue -> CqlValue) Any -> CqlValue   fold over non-null elements
(define (aggregate x f _seed)
  (cond [(not (list? x)) cql-null]
        [else (define nn (drop-nulls x))
              (if (null? nn) cql-null (foldl (lambda (e acc) (f acc e)) (car nn) (cdr nn)))]))

;; aggregate-cmp : CqlValue (or -1 1) -> CqlValue   min (dir=-1) / max (dir=1) over non-null elements
(define (aggregate-cmp x dir)
  (cond [(not (list? x)) cql-null]
        [else (define nn (drop-nulls x))
              (if (null? nn) cql-null
                  (foldl (lambda (e acc) (if (= (sgn (cql-compare e acc)) dir) e acc)) (car nn) (cdr nn)))]))

;; avg : CqlValue -> CqlValue   arithmetic mean (Decimal) over non-null elements
(define (avg x)
  (cond [(not (list? x)) cql-null]
        [else (define nn (drop-nulls x))
              (if (null? nn) cql-null
                  (decimal-div (foldl (lambda (e acc) (decimal-add acc (->dec e))) (integer->decimal 0) nn)
                               (integer->decimal (length nn))))]))

;; ---- list operators --------------------------------------------------------
(define (lst? x) (and (list? x) #t))
;; lst-slice : CqlValue CqlValue CqlValue -> CqlValue
;; Slice(list [, start [, end]]): null start/end default to the whole extent; a NEGATIVE index counts
;; from the end (-2 -> the last two). Out-of-range windows clamp to the empty list.
(define (lst-slice x s e)
  (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null]
        [else (define n (length x))
              (define (norm i dflt) (cond [(cql-null? i) dflt]
                                          [else (define v (->int-val i)) (if (< v 0) (max 0 (+ n v)) v)]))
              (define start (min n (norm s 0)))
              (define end (min n (norm e n)))
              (if (> start end) '() (take (drop x start) (- end start)))]))
(define (lst-index-of x el)
  (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null] [(cql-null? el) cql-null]   ; IndexOf(_, null) = null
        [else (let loop ([i 0] [xs x]) (cond [(null? xs) -1] [(cql-eq? (car xs) el) i] [else (loop (add1 i) (cdr xs))]))]))
(define (flatten-list x)
  (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null]
        [else (append-map (lambda (e) (if (list? e) e (list e))) x)]))

;; ---- more aggregates -------------------------------------------------------
;; all-true / any-true : CqlValue -> Boolean   null elements are ignored (empty/null list: #t / #f).
;; NOTE: must not use findf to look for #f — the found element IS #f, indistinguishable from not-found.
(define (all-true x) (cond [(cql-null? x) #t] [(not (lst? x)) #t] [else (not (for/or ([e (in-list x)]) (eq? e #f)))]))
(define (any-true x) (cond [(cql-null? x) #f] [(not (lst? x)) #f] [else (for/or ([e (in-list x)]) (eq? e #t))]))
(define (median x)
  (cond [(not (lst? x)) cql-null]
        [else (define nn (sort (drop-nulls x) (lambda (a b) (< (cql-compare a b) 0))))
              (define n (length nn))
              (cond [(zero? n) cql-null]
                    [(odd? n) (->dec (list-ref nn (quotient n 2)))]
                    [else (decimal-div (decimal-add (->dec (list-ref nn (sub1 (quotient n 2)))) (->dec (list-ref nn (quotient n 2))))
                                       (integer->decimal 2))])]))
(define (mode x)
  (cond [(not (lst? x)) cql-null]
        [else (define nn (drop-nulls x)) (define counts (make-hash))
              (cond [(null? nn) cql-null]
                    [else (for ([e (in-list nn)]) (hash-update! counts (render e) (lambda (c) (cons (add1 (car c)) e)) (cons 0 e)))
                          (cdr (argmax car (hash-values counts)))])]))
(define (variance x pop?)
  (cond [(not (lst? x)) cql-null]
        [else (define nn (map (lambda (e) (decimal->exact (->dec e))) (drop-nulls x)))
              (define n (length nn))
              (cond [(< n (if pop? 1 2)) cql-null]
                    [else (define mean (/ (apply + nn) n))
                          (define ss (apply + (map (lambda (v) (* (- v mean) (- v mean))) nn)))
                          (rat->dec (/ ss (if pop? n (sub1 n))))])]))
(define (stddev x pop?)
  (define v (variance x pop?))
  (if (cql-null? v) cql-null (dec8 (sqrt (exact->inexact (decimal->exact v))))))
(define (geometric-mean x)
  (cond [(not (lst? x)) cql-null]
        [else (define nn (map (lambda (e) (exact->inexact (decimal->exact (->dec e)))) (drop-nulls x)))
              (if (null? nn) cql-null (rat->dec (expt (apply * nn) (/ 1.0 (length nn)))))]))

;; ---- more string functions -------------------------------------------------
(define (str-split s sep)
  (cond [(cql-null? s) cql-null]
        [(not (string? s)) cql-null]
        [(cql-null? sep) (list s)]                      ; null separator -> the whole string, one element
        [(not (string? sep)) cql-null]
        [else (string-split s sep #:trim? #f)]))
(define (last-position-of pat s)
  (cond [(or (cql-null? pat) (cql-null? s)) cql-null]
        [else (define n (string-length s)) (define m (string-length pat))
              (let loop ([i (- n m)]) (cond [(< i 0) -1] [(string=? (substring s i (+ i m)) pat) i] [else (loop (sub1 i))]))]))

;; ---- query engine ----------------------------------------------------------
;; run-query : sources lets where ret ret-distinct sort agg Env -> CqlValue
;; sources/lets are (Listof (cons string elm)); agg is (list name distinct? starting-elm body-elm)|#f.
;; ev : (ELM Env -> CqlValue) evaluates the query's parts: the tree walker by default, the compiled
;; evaluator from a compiled query (one query engine, so both evaluators share its semantics)
(define (run-query sources lets rels where ret ret-distinct sort agg env [ev eval-elm])
  (define srcs (map (lambda (s) (cons (car s) (ev (cdr s) env))) sources))
  (cond
    [(ormap (lambda (s) (cql-null? (cdr s))) srcs) cql-null]
    [else
     (define aliases (map car srcs))
     (define scalar-single? (and (= 1 (length srcs)) (not (list? (cdr (car srcs))))))
     (define elem-lists (map (lambda (s) (if (list? (cdr s)) (cdr s) (list (cdr s)))) srcs))
     (define rows (query-cartesian aliases elem-lists))
     (define row-envs (map (lambda (row) (apply-lets (row-env env row) lets ev)) rows))
     (define related (filter (lambda (e) (andmap (lambda (r) (relationship-holds? r e ev)) rels)) row-envs))
     (define kept (if where (filter (lambda (e) (eq? (ev where e) #t)) related) related))
     (cond
       [agg (run-aggregate agg aliases kept env ev)]
       [else
        (define results (map (lambda (e) (row-result ret aliases e ev)) kept))
        (define r1 (if (eq? ret-distinct #t) (dedup results) results))
        (define r2 (if sort (sort-by-items r1 sort env ev) r1))
        (if scalar-single? (if (pair? r2) (car r2) cql-null) r2)])]))

;; relationship-holds? : elm-relationship Env -> boolean   with: some related row satisfies such-that ;
;; without: none does
(define (relationship-holds? r env [ev eval-elm])
  (define src (ev (elm-relationship-expression r) env))
  (define items (cond [(cql-null? src) '()] [(list? src) src] [else (list src)]))
  (define any? (for/or ([v (in-list items)])
                 (eq? #t (ev (elm-relationship-such-that r)
                             (row-env env (hash (elm-relationship-alias r) v))))))
  (if (eq? (elm-relationship-kind r) 'with) any? (not any?)))

;; sort-by-items : (Listof CqlValue) (Listof elm-sort-by) Env -> (Listof CqlValue)
;; a single ByDirection sorts the values themselves; ByColumn/ByExpression sort by a key per row
;; (stable, keys compared in order)
(define (sort-by-items xs items env [ev eval-elm])
  (match items
    [(list (elm-sort-by 'direction dir _)) (sort-values xs (string->symbol dir))]
    [_
     (define (key-of b x)
       (parameterize ([current-sort-row x])
         (match b
           [(elm-sort-by 'column _ path) (eval-property x path)]
           [(elm-sort-by 'expression _ e) (ev e env)]
           [(elm-sort-by 'direction _ _) x])))
     (sort xs (lambda (a b)
                (let loop ([bs items])
                  (cond [(null? bs) #f]
                        [else
                         (define c (total-compare (key-of (car bs) a) (key-of (car bs) b)))
                         (define c* (if (member (elm-sort-by-direction (car bs)) '("desc" "descending")) (- c) c))
                         (cond [(< c* 0) #t] [(> c* 0) #f] [else (loop (cdr bs))])]))))]))
(define (query-cartesian aliases lists)
  (if (null? aliases) (list (hash))
      (append-map (lambda (v) (map (lambda (r) (hash-set r (car aliases) v))
                                   (query-cartesian (cdr aliases) (cdr lists))))
                  (car lists))))
(define (row-env base row)
  (struct-copy ienv base [operands (for/fold ([h (ienv-operands base)]) ([(k v) (in-hash row)]) (hash-set h k v))]))
(define (apply-lets env lets [ev eval-elm])
  (for/fold ([e env]) ([l (in-list lets)])
    (struct-copy ienv e [operands (hash-set (ienv-operands e) (car l) (ev (cdr l) e))])))
(define (row-result ret aliases e [ev eval-elm])
  (cond [ret (ev ret e)]
        [(= 1 (length aliases)) (hash-ref (ienv-operands e) (car aliases))]
        [else (cql-tuple (for/hash ([a (in-list aliases)]) (values (string->symbol a) (hash-ref (ienv-operands e) a))))]))

;; sort-values : (Listof CqlValue) ('asc|'ascending|'desc|'descending) -> (Listof CqlValue)
;; Sorting needs a TOTAL order, so undecidable pairs (mixed-precision moments) fall back to a
;; deterministic tiebreak: component-wise with an absent component sorting FIRST (so @2012-10-05
;; precedes @2012-10-05T10, per the reference implementation).
(define (sort-values xs dir)
  (sort xs (lambda (a b) (define c (total-compare a b)) (if (memq dir '(desc descending)) (> c 0) (< c 0)))))
(define (run-aggregate agg aliases kept env [ev eval-elm])
  (define name (car agg)) (define adist (cadr agg)) (define start-elm (caddr agg)) (define body (cadddr agg))
  (define rows (if adist (dedup-rows kept aliases) kept))
  (define start (if start-elm (ev start-elm env) cql-null))
  (foldl (lambda (e acc) (ev body (struct-copy ienv e [operands (hash-set (ienv-operands e) name acc)])))
         start rows))

;; dedup row-envs by their alias-value tuple (for `aggregate distinct`)
(define (dedup-rows envs aliases)
  (define seen (make-hash)) (define out '())
  (for ([e (in-list envs)])
    (define key (map (lambda (a) (render (hash-ref (ienv-operands e) a))) aliases))
    (unless (hash-ref seen key #f) (hash-set! seen key #t) (set! out (cons e out))))
  (reverse out))

;; ---- control flow ----------------------------------------------------------
(define (eval-if c t e env)
  (define cv (eval-elm c env))
  (if (eq? cv #t) (eval-elm t env) (eval-elm e env)))   ; false or null -> else
(define (eval-case comparand items els env)
  (define cmp (and comparand (eval-elm comparand env)))
  (let loop ([its items])
    (cond
      [(null? its) (if els (eval-elm els env) cql-null)]
      [else (define it (car its))
            (define w (eval-elm (elm-case-item-when it) env))
            (define hit (if comparand (cql-eq? cmp w) (eq? w #t)))
            (if hit (eval-elm (elm-case-item-then it) env) (loop (cdr its)))])))

;; ---- navigation / functions / types ---------------------------------------
(define (eval-property v path) ((property-accessor path) v))

;; property-accessor : path -> (CqlValue -> CqlValue)   a dotted path (`birthDate.value`) navigates
;; step by step; the path is split once, here, not on every access (the compiled evaluator keeps
;; the accessor; eval-property builds it per call)
(define (property-accessor path)
  (if (regexp-match? #rx"[.]" path)
      (let ([steps (string-split path ".")])
        (lambda (v) (for/fold ([x v]) ([p (in-list steps)]) (property-step x p))))
      (lambda (v) (property-step v path))))

;; property-step : CqlValue step -> CqlValue   one step of a property path
(define (property-step v path)
  (cond [(cql-null? v) cql-null]
        [(model-node? v) (node-property v path)]
        [(cql-tuple? v) (hash-ref (cql-tuple-fields v) (string->symbol path) cql-null)]
        [(cql-code? v) (or (case path [("code") (cql-code-code v)] [("system") (cql-code-system v)]
                                      [("version") (cql-code-version v)] [("display") (cql-code-display v)] [else #f])
                           cql-null)]
        [(cql-concept? v) (case path [("codes") (cql-concept-codes v)] [("display") (or (cql-concept-display v) cql-null)] [else cql-null])]
        [(cql-interval? v) (case path [("low") (or (cql-interval-low v) cql-null)] [("high") (or (cql-interval-high v) cql-null)]
                             [("lowClosed") (cql-interval-low-closed? v)] [("highClosed") (cql-interval-high-closed? v)] [else cql-null])]
        ;; System.Quantity's value and unit, System.Ratio's numerator and denominator
        [(fp-quantity? v) (case path [("value") (->dec (fp-quantity-value v))] [("unit") (fp-quantity-unit v)] [else cql-null])]
        [(cql-ratio? v) (case path [("numerator") (cql-ratio-numerator v)] [("denominator") (cql-ratio-denominator v)] [else cql-null])]
        [else cql-null]))          ; slice-1: property on non-tuples is undecidable -> null
(define (eval-function name args env [run eval-elm])
  (define f (hash-ref (ienv-funcs env) name (lambda () (fail (format "unknown function ~a" name)))))
  (define ops (elm-function-def-operands f))
  (define env* (struct-copy ienv env
                            [operands (for/fold ([h (hash)]) ([o (in-list ops)] [a (in-list args)])
                                        (hash-set h (elm-operand-def-name o) a))]))
  (define body (elm-function-def-expression f))
  (when (eq? body (quote external)) (fail (format "external function ~a has no implementation" name)))
  (run body env*))
;; eval-is : value type -> boolean   `is`: a named type by the value's class; Interval<T> when the
;; value is an interval whose bounds (those not null) are T; List<T> when every element is T; a
;; choice when any member is
(define (eval-is v type*)
  (define type (if (string? type*) (elm-named-type type*) type*))
  (cond [(cql-null? v) #f]
        [(and (model-node? v) (elm-named-type? type)) (node-is? v (elm-named-type-name type))]
        [(elm-named-type? type) (value-is-type? v (elm-named-type-name type))]
        [(elm-interval-type? type)
         (and (cql-interval? v)
              (for/and ([b (list (cql-interval-low v) (cql-interval-high v))] #:unless (or (not b) (cql-null? b)))
                (eval-is b (elm-interval-type-point type))))]
        [(elm-list-type? type) (and (list? v) (andmap (lambda (x) (or (cql-null? x) (eval-is x (elm-list-type-element type)))) v))]
        [(elm-choice-type? type) (ormap (lambda (c) (eval-is v c)) (elm-choice-type-types type))]
        [else #f]))
(define simple-names (make-weak-hash))   ; qualified name -> simple name (a pure function, memoised)
(define (simple-name qn)
  (hash-ref! simple-names qn (lambda () (let ([m (regexp-match #rx"}([^}]+)$" qn)]) (if m (cadr m) qn)))))
(define (value-is-type? v qn)
  (define t (simple-name qn))
  (case t
    [("Integer") (exact-integer? v)] [("Long") (fp-long? v)] [("Decimal") (fp-decimal? v)]
    [("String") (string? v)] [("Boolean") (boolean? v)] [("Quantity") (fp-quantity? v)]
    [("Date" "DateTime" "Time") (moment? v)]
    [("Code") (cql-code? v)] [("Concept") (cql-concept? v)]
    ;; class hierarchy: ValueSet and CodeSystem are Vocabularies
    [("Vocabulary") (and (cql-obj? v) (member (obj-class-simple v) '("ValueSet" "CodeSystem" "Vocabulary")) #t)]
    [("ValueSet" "CodeSystem") (and (cql-obj? v) (string=? (obj-class-simple v) t))]
    [else #f]))
(define (obj-class-simple o) (simple-name (regexp-replace #px"^System\\." (cql-obj-class o) "")))
(define (convert-to v to-type)
  (if (cql-null? v) cql-null
      (case to-type
        [("String") (render v)] [("Integer") (to-integer v)] [("Long") (fp-long (->int-val v))]
        [("Decimal") (->dec v)] [("Boolean") (to-boolean v)] [("Quantity") (to-quantity v)]
        [("Date") (to-temporal (if (moment? v) v (render v)) 'date)]
        [("DateTime") (to-temporal (if (moment? v) v (render v)) 'datetime)]
        [("Time") (to-temporal (if (moment? v) v (render v)) 'time)]
        [else cql-null])))
(define (to-integer x)
  (cond [(cql-null? x) cql-null] [(exact-integer? x) x] [(fp-long? x) (fp-long-v x)]
        [(string? x) (or (string->number x) cql-null)] [(fp-decimal? x) (truncate-int x)] [else cql-null]))
(define (to-boolean x)
  (cond [(cql-null? x) cql-null] [(boolean? x) x]
        [(string? x) (cond [(member (string-downcase x) '("true" "t" "yes" "y" "1")) #t]
                           [(member (string-downcase x) '("false" "f" "no" "n" "0")) #f] [else cql-null])]
        [else cql-null]))
(define (to-quantity x)
  (cond [(cql-null? x) cql-null] [(fp-quantity? x) x] [(num? x) (make-quantity (->dec x) "1")]
        [(string? x) (let ([m (regexp-match #px"^([0-9.]+)\\s*'([^']*)'$" x)])
                       (if m (make-quantity (or (parse-decimal (cadr m)) (integer->decimal 0)) (caddr m)) cql-null))]
        [else cql-null]))
(module+ test
  (require rackunit
           (only-in "parser/parse.rkt" parse-cql-expr)
           (only-in "compile.rkt" compile-expr)
           (only-in fhir/result/result ok-value))
  ;; end-to-end: parse -> compile -> eval a bare expression
  (define (E s) (eval-elm (ok-value (compile-expr (ok-value (parse-cql-expr s)))) (make-ienv)))
  (check-equal? (E "1 + 2") 3)
  (check-equal? (E "2 * 3 + 1") 7)
  (check-equal? (E "10 - 4 * 2") 2)
  (check-equal? (decimal->string (E "3.0 + 0.14")) "3.14")
  (check-equal? (E "7 div 2") 3)
  (check-equal? (E "7 mod 3") 1)
  (check-equal? (E "2 ^ 10") 1024)
  (check-equal? (E "'a' + 'b'") "ab")
  (check-equal? (E "'a' & 'b'") "ab")
  (check-true (cql-null? (E "'a' + null")))     ; + is null-propagating
  (check-equal? (E "'x' & null") "x")            ; & treats null as ''
  (check-equal? (E "1 = 1") #t)
  (check-equal? (E "1 = 2") #f)
  (check-equal? (E "1 != 2") #t)
  (check-equal? (E "3 > 2") #t)
  (check-equal? (E "2 >= 2") #t)
  (check-equal? (E "'abc' < 'abd'") #t)
  (check-equal? (E "true and false") #f)
  (check-true (cql-null? (E "true and null")))
  (check-equal? (E "true or null") #t)
  (check-equal? (E "not true") #f)
  (check-equal? (E "-5") -5)
  (check-equal? (E "Abs(-3)") 3)
  (check-equal? (E "if 1 < 2 then 'yes' else 'no'") "yes")
  (check-equal? (E "case when false then 1 when true then 2 else 3 end") 2)
  (check-equal? (E "{1, 2, 3}") (list 1 2 3))
  (check-equal? (E "Count({1, 2, 3})") 3)
  (check-equal? (E "First({4, 5, 6})") 4)
  (check-equal? (E "5 in {1, 5, 9}") #t)
  (check-equal? (E "5 in Interval[1, 10]") #t)
  (check-equal? (E "10 in Interval[1, 10)") #f)
  (check-equal? (E "{1, 2} union {2, 3}") (list 1 2 3))
  (check-equal? (E "{1, 2, 3} intersect {2, 3, 4}") (list 2 3))
  (check-equal? (E "Coalesce(null, null, 7)") 7)
  (check-equal? (E "ToString(42)") "42")
  (check-equal? (E "ToInteger('15')") 15)
  (check-equal? (E "Upper('abc')") "ABC")
  (check-equal? (E "5 is Integer") #t)
  (check-equal? (E "convert 5 to String") "5")
  ;; library run with cross references
  (require (only-in "parser/parse.rkt" parse-cql) (only-in "compile.rkt" compile-library))
  (define results (run-library (ok-value (compile-library (ok-value (parse-cql "define A: 3 + 4\ndefine B: A * 2"))))))
  (check-equal? (hash-ref results "A") 7)
  (check-equal? (hash-ref results "B") 14)
  ;; in `sort by`, a fluent function without a target applies to the row ($this)
  (define sorted (run-library (ok-value (compile-library (ok-value (parse-cql "define fluent function Neg(x Integer): -x\ndefine S: ({3, 1, 2}) v sort by Neg() asc"))))))
  (check-equal? (hash-ref sorted "S") '(3 2 1))

  ;; ---- the closure compiler ----
  ;; the compiled evaluator gives the tree walker's value (tests/evaluator-test.rkt does this for
  ;; every expression of the HL7 suite and every definition of the two library corpora)
  (define (node s) (ok-value (compile-expr (ok-value (parse-cql-expr s)))))
  (for ([s (in-list '("1 + 2" "3.0 + 0.14" "'a' & null" "true and null" "false and (1 / 0 = 1)" "null or true"
                      "if 1 < 2 then 'yes' else 'no'" "case 2 when 1 then 'a' when 2 then 'b' else 'c' end"
                      "{1, 2} union {2, 3}" "5 in Interval[1, 10]" "Tuple { a: 1, b: 'x' }.b"
                      "(from ({1, 2, 3}) X where X > 1 return X * 10)" "({3, 1, 2}) v sort desc"
                      "@2012-01-01T10:00 + 1 day" "Interval[@2012-01-01, @2012-12-31] overlaps Interval[@2012-06-01, @2013-01-01]"))])
    (define n (node s)) (define env (make-ienv))
    (check-equal? (eval-elm/compiled n env) (eval-elm n env) s))
  ;; compiling never raises: an error is raised when (and only if) the node runs, as the tree walker
  ;; does — an untaken branch with a bad literal or an unknown operator evaluates fine
  (define bad-literal (elm-literal "Decimal" "not-a-number"))
  (define unknown-op (elm-unary "NoSuchOperator" (elm-literal "Integer" "1") #f))
  (check-equal? (eval-elm/compiled (elm-if (elm-literal "Boolean" "true") (elm-literal "Integer" "1") bad-literal) (make-ienv)) 1)
  (check-equal? (eval-elm/compiled (elm-if (elm-literal "Boolean" "false") unknown-op (elm-literal "Integer" "2")) (make-ienv)) 2)
  (check-exn cql-error? (lambda () (eval-elm/compiled bad-literal (make-ienv))))
  (check-exn cql-error? (lambda () (eval-elm/compiled unknown-op (make-ienv))))
  ;; a node is compiled once
  (define n1 (node "1 + 2"))
  (check-eq? (compile-elm n1) (compile-elm n1))
  ;; run-library and libraries take the evaluator current-elm-evaluator names; both agree
  (define lib2 (ok-value (compile-library (ok-value (parse-cql "define A: 3 + 4\ndefine B: A * 2\ndefine C: (from ({1,2}) X return X + B)")))))
  (check-equal? (parameterize ([current-elm-evaluator 'tree]) (run-library lib2))
                (parameterize ([current-elm-evaluator 'compiled]) (run-library lib2)))

  ;; ---- per-runtime memo ----
  ;; a retrieve reads its type's resources once per runtime, however many definitions retrieve it
  (define reads 0)
  (define rt (runtime (lambda (type) (set! reads (add1 reads)) (list (hasheq 'resourceType type 'id "1"))) (lambda (u) #f) #f))
  (define lib3 (ok-value (compile-library (ok-value (parse-cql "using FHIR version '4.0.1'\ncontext Patient\ndefine A: [Condition]\ndefine B: [Condition]")))))
  (define rl3 (make-rlib lib3 (hash) rt (hash)))
  (check-equal? (length (rlib-value rl3 "A")) 1)
  (check-equal? (length (rlib-value rl3 "B")) 1)
  (check-equal? reads 1 "Condition is read once for the two definitions")
  ;; value set membership: a system-less code matches by code alone, through the memoised index
  (define vs-members (hash (cons "http://s" "a") #t (cons "http://s" "b") #t))
  (define rt2 (runtime (lambda (_t) '()) (lambda (u) (and (equal? u "http://vs") vs-members)) #f))
  (define env2 (struct-copy ienv (make-ienv) [rt rt2]))
  (define vs (cql-obj "ValueSet" (hash 'id "http://vs" 'version cql-null)))
  (check-true (in-vocabulary? (cql-code "a" #f #f #f) vs env2))
  (check-false (in-vocabulary? (cql-code "z" #f #f #f) vs env2))
  (check-true (in-vocabulary? (cql-code "b" "http://s" #f #f) vs env2))
  (check-false (in-vocabulary? (cql-code "b" "http://other" #f #f) vs env2)))

