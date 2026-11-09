#lang racket/base

;;; compile.rkt — CQL AST -> ELM AST (the translator).
;;;
;;; Translation needs TYPES: CQL surface operators are overloaded (`+` is Add on numbers but
;;; Concatenate on strings; `in` is In, InValueSet or AnyInValueSet), function calls resolve among
;;; overloads (FHIRHelpers has one ToString per FHIR code type), and a model type meeting a System
;;; operator is converted implicitly (FHIR.EncounterStatus -> FHIRHelpers.ToString(…)). So `compile`
;;; returns (values elm-node CType) — CType is model/modelinfo.rkt's data definition: a System simple
;;; name ("Integer"), a model name ("FHIR.Encounter"), "Any", or a list/interval/choice/tuple
;;; (list-ctype … / choice-ctype … and their predicates and accessors; never the vectors themselves).
;;;
;;; A library compiles in a library context (`lctx`, below): every name it declares, its data model
;;; (from `using`, see model/modelinfo.rkt), its includes (compiled lazily through
;;; `current-library-resolver`), and memo tables — definitions compile ON DEMAND when first referenced
;;; (their type is needed at the reference), so forward references and included libraries both work.
;;; Errors raise a cql-error caught at the API boundary and returned as (err …).

(require (only-in fhir/suggest did-you-mean closest-names)
         racket/match racket/list racket/string
         "parser/ast.rkt"
         "elm/ast.rkt"
         "model/modelinfo.rkt"
         (only-in "mapping.rkt" parse-mapping map-call map-path map-field map-field? map-field-name map-filter)
         (only-in "error.rkt" cql-error cql-error? cql-error-diagnostic cql-error-phase cql-error-message cql-error-string)
         (only-in fhir/diagnostic diagnostic attached-loc)
         racket/contract
         (only-in fhir/result/result ok err ok? err?)
         (only-in fhirpath/temporal-ops
                  parse-fp-date parse-fp-datetime parse-fp-time
                  moment-year moment-month moment-day moment-hour moment-minute moment-second
                  moment-frac moment-tzmin))

(provide
 current-library-resolver
 (contract-out
  ;; compile-library : cql-library -> (ok elm-library) | (err cql-error)
  [compile-library (-> cql-library? (or/c ok? err?))]
  ;; every translation error (each positioned at its definition), and the library when there are none
  [compile-library/errors (-> cql-library? (values (or/c elm-library? #f) (listof cql-error?)))]
  ;; the same, and the warnings (diagnostics, positioned at their definitions)
  [compile-library/diagnostics (-> cql-library? (values (or/c elm-library? #f) (listof cql-error?) list?))]
  [compile-library/partial (-> cql-library? (values (or/c elm-library? #f) (listof cql-error?)))]
  ;; compile-expr : cql-expression-AST -> (ok elm-expr) | (err cql-error)  (bare-expression entry)
  [compile-expr    (-> any/c (or/c ok? err?))]))

;; current-library-resolver : (parameter (symbol (or string #f) -> (or cql-library #f)))
;; finds an included library's source AST by name and version; #f = unknown (references into it still
;; compile, untyped)
(define current-library-resolver (make-parameter (lambda (name version) #f)))

;; ---- library context -------------------------------------------------------
;; An lctx is everything one library declares, by name:
;;   model      : model|#f              the `using` data model (FHIR 4.0.1), #f for System-only
;;   defs       : sym -> cql-def | 'context   (a `context X` adds the implicit definition X)
;;   contexts   : sym -> string         each define's context ("Patient", "Unfiltered")
;;   funcs      : sym -> (listof cql-func)    overloads in declaration order
;;   params     : sym -> cql-parameter
;;   terms      : sym -> kind           "ValueSetRef" | "CodeSystemRef" | "CodeRef" | "ConceptRef"
;;   includes   : sym(alias) -> (cons cql-include (-> lctx|#f))   the included library, lazily
;;   memo       : sym -> (cons elm CType) | 'pending       compiled definitions
;;   fmemo      : cql-func -> CType                         inferred function result types
(struct lctx (name model defs contexts funcs params terms includes memo fmemo))
(define current-lctx (make-parameter #f))
(define (L) (current-lctx))

;; make-lctx : cql-library -> lctx
(define (make-lctx lib)
  (define defs (cql-library-defs lib))
  (define usings (filter cql-using? defs))
  (define model (for/or ([u (in-list usings)])
                  (find-model (symbol->string (cql-using-model u)) (cql-using-version u))))
  (define-values (dmap cmap)
    (for/fold ([dm (hash)] [cm (hash)] [ctx "Unfiltered"] #:result (values dm cm)) ([d (in-list defs)])
      (match d
        [(cql-context name)
         (define cname (symbol->string name))
         (values (if (and model (model-context-type model cname)) (hash-set dm name 'context) dm)
                 (hash-set cm name cname) cname)]
        [(? cql-def?) (values (hash-set dm (cql-def-name d) d) (hash-set cm (cql-def-name d) ctx) ctx)]
        [(? cql-func?) (values dm (hash-set cm (cql-func-name d) ctx) ctx)]
        [_ (values dm cm ctx)])))
  (define funcs (for/fold ([h (hash)]) ([d (in-list defs)] #:when (cql-func? d))
                  (hash-update h (cql-func-name d) (lambda (l) (append l (list d))) '())))
  (define params (for/hash ([d (in-list defs)] #:when (cql-parameter? d)) (values (cql-parameter-name d) d)))
  (define terms (for/hash ([d (in-list defs)]
                           #:when (or (cql-valueset? d) (cql-codesystem? d) (cql-code-def? d) (cql-concept-def? d)))
                  (match d
                    [(? cql-valueset?) (values (cql-valueset-name d) "ValueSetRef")]
                    [(? cql-codesystem?) (values (cql-codesystem-name d) "CodeSystemRef")]
                    [(? cql-code-def?) (values (cql-code-def-name d) "CodeRef")]
                    [(? cql-concept-def?) (values (cql-concept-def-name d) "ConceptRef")])))
  (define includes
    (for/hash ([d (in-list defs)] #:when (cql-include? d))
      (define alias (or (cql-include-alias d) (cql-include-lib d)))
      (define cell #f)
      (values alias
              (cons d (lambda ()
                        (unless cell
                          (set! cell 'none)
                          (define src ((current-library-resolver) (cql-include-lib d) (cql-include-version d)))
                          (when src (set! cell (make-lctx src))))
                        (and (lctx? cell) cell))))))
  (lctx (cql-library-id lib) model dmap cmap funcs params terms includes (make-hasheq) (make-hasheq)))

(define empty-lctx (lctx #f #f (hash) (hash) (hash) (hash) (hash) (hash) (make-hasheq) (make-hasheq)))

;; the include alias under which a library NAMED `libname` is visible here (FHIRHelpers), or #f
(define (include-alias-for libname)
  (for/or ([(alias inc) (in-hash (lctx-includes (L)))])
    (and (eq? (cql-include-lib (car inc)) libname) (symbol->string alias))))
(define (included alias) (let ([i (hash-ref (lctx-includes (L)) alias #f)]) (and i ((cdr i)))))

;; ---- local scope -----------------------------------------------------------
;; A cenv maps names bound INSIDE an expression to (cons kind CType): kind 'operand (function
;; operand), 'alias (query source), 'let (query let), 'acc (aggregate accumulator), 'row ($this, the
;; row of a `sort by`, as a fluent function's implicit target). `row` is the
;; element type a `sort by` expression is evaluated against (#f elsewhere).
;; `this` is the alias ("$this") whose properties bare names reach inside a FHIRPath-style
;; .where(…)/.select(…), #f elsewhere.
(struct cenv (locals row this) #:transparent)
(define empty-cenv (cenv (hash) #f #f))
(define (cenv-bind e kind pairs)
  (cenv (for/fold ([h (cenv-locals e)]) ([p (in-list pairs)]) (hash-set h (car p) (cons kind (cdr p))))
        (cenv-row e) (cenv-this e)))
(define (cenv-add-operands e names) (cenv-bind e 'operand (map (lambda (n) (cons n "Any")) names)))
(define (local-ref e name) (hash-ref (cenv-locals e) name #f))

;; ---- public API ------------------------------------------------------------
;; compile-library : cql-library -> (ok elm-library) | (err cql-error)
(define (compile-library lib)
  (define-values (elm errors) (compile-library/errors lib))
  (if (null? errors) (ok elm) (err (car errors))))

;; compile-library/errors : cql-library -> (values elm-library|#f (listof cql-error))
;; every translation error, not just the first: a definition that fails is skipped and the rest
;; still compile (a reference to it is untyped, so it does not cascade); errors in source order,
;; each positioned at its definition
(define (compile-library/errors lib)
  (define collected (box '()))
  (with-handlers ([cql-error? (lambda (e) (values #f (list e)))])
    (define lx (make-lctx lib))
    (parameterize ([current-lctx lx] [compile-memo (make-hasheq)] [current-errors collected])
      (define elm (assemble-library lib lx))
      (define errors (remove-duplicates (reverse (unbox collected))
                                        (lambda (a b) (and (cql-error-diagnostic a) (equal? (cql-error-diagnostic a) (cql-error-diagnostic b))))))
      (values (and (null? errors) elm) errors))))
;; compile-library/diagnostics : cql-library -> (values elm-library|#f (listof cql-error) (listof diagnostic))
;; compile-library/errors, and the WARNINGS: what translates but likely does not mean what it says
;; (a choice read through one member, see choice-to), each positioned at its definition
(define (compile-library/diagnostics lib)
  (define warnings (box '()))
  (define-values (elm errors) (parameterize ([current-warnings warnings]) (compile-library/errors lib)))
  (values elm errors (remove-duplicates (reverse (unbox warnings)))))
;; current-warnings : (parameter (box (listof diagnostic)) | #f) ; current-definition : (parameter
;; (cons label def) | #f), the definition being compiled (warn! positions and names it)
(define current-warnings (make-parameter #f))
(define current-definition (make-parameter #f))
;; warn! : string -> void   a warning about the current definition, when someone collects them
(define (warn! msg)
  (define b (current-warnings))
  (define d (current-definition))
  (when b
    (set-box! b (cons (diagnostic 'cql 'compile 'warning
                                  (if d (format "in ~a: ~a" (car d) msg) msg)
                                  (and d (attached-loc (cdr d))))
                      (unbox b)))))
;; current-errors : (parameter (box (listof cql-error)) | #f)   where accumulating compiles collect
(define current-errors (make-parameter #f))
;; collecting : thunk default -> value   run one statement's compile; with an accumulating compile, an
;; error is recorded and `default` stands in (a procedure default is given the error)
(define (collecting thunk default)
  (define box* (current-errors))
  (if box*
      (with-handlers ([cql-error? (lambda (e) (set-box! box* (cons e (unbox box*))) (if (procedure? default) (default e) default))])
        (thunk))
      (thunk)))
;; current-partial? : (parameter boolean)   a partial compile keeps a failed definition as one that
;; raises its translation error when evaluated
(define current-partial? (make-parameter #f))
(define (raising-definition name e)
  (elm-def name "Unfiltered" #f
           (elm-nary "Message" (list (elm-null #f) (elm-literal "Boolean" "true") (elm-literal "String" "translation")
                                     (elm-literal "String" "Error") (elm-literal "String" (cql-error-string e)))
                     #f)))

;; compile-library/partial : cql-library -> (values elm-library|#f (listof cql-error))   every
;; statement translated; a failing definition becomes one that raises its error when evaluated (the
;; reference translator's error recovery leaves a Null there), so the rest of the library runs
(define (compile-library/partial lib)
  (parameterize ([current-partial? #t])
    (define collected (box '()))
    (with-handlers ([cql-error? (lambda (e) (values #f (list e)))])
      (define lx (make-lctx lib))
      (parameterize ([current-lctx lx] [compile-memo (make-hasheq)] [current-errors collected])
        (values (assemble-library lib lx) (reverse (unbox collected)))))))

;; compile-expr : cql-ast -> (ok elm-node) | (err cql-error)  — a bare expression (harness/CLI)
(define (compile-expr ast)
  (with-handlers ([cql-error? (lambda (e) (err e))])
    (parameterize ([current-lctx empty-lctx] [compile-memo (make-hasheq)])
      (define-values (node _t) (compile ast empty-cenv))
      (ok node))))

;; assemble-library : cql-library lctx -> elm-library   every section, statements in source order
(define (assemble-library lib lx)
  (define defs (cql-library-defs lib))
  (define model (lctx-model lx))
  (define (of pred f) (for/list ([d (in-list defs)] #:when (pred d)) (f d)))
  (define usings
    (cons (elm-using "System" "urn:hl7-org:elm-types:r1" #f)
          (of cql-using?
              (lambda (d)
                (define m (find-model (symbol->string (cql-using-model d)) (cql-using-version d)))
                (elm-using (if (cql-using-alias d) (symbol->string (cql-using-alias d)) (symbol->string (cql-using-model d)))
                           (if m (or (model-target-url m) (model-url m)) (symbol->string (cql-using-model d)))
                           (cql-using-version d))))))
  (define includes
    (of cql-include?
        (lambda (d) (elm-include (symbol->string (or (cql-include-alias d) (cql-include-lib d)))
                                 (symbol->string (cql-include-lib d)) (cql-include-version d)))))
  (define params
    (of cql-parameter?
        (lambda (d) (elm-parameter-def (symbol->string (cql-parameter-name d)) (cql-parameter-access d)
                                       (and (cql-parameter-default d) (compile1 (cql-parameter-default d) empty-cenv))
                                       (and (cql-parameter-type d) (compile-type (cql-parameter-type d)))))))
  (define codesystems
    (of cql-codesystem? (lambda (d) (elm-codesystem-def (symbol->string (cql-codesystem-name d)) (cql-codesystem-id d)
                                                        (cql-codesystem-version d) (cql-codesystem-access d)))))
  ;; checked-ref : def label kind ref -> elm-term-ref   positioned at the terminology definition; an
  ;; accumulating compile records the error and goes on
  (define (checked-ref d label kind r)
    (collecting (lambda () (in-definition d label (lambda () ((term-ref-of kind) (known-term kind r)))))
                ((term-ref-of kind) r)))
  (define valuesets
    (of cql-valueset? (lambda (d) (elm-valueset-def (symbol->string (cql-valueset-name d)) (cql-valueset-id d)
                                                    (cql-valueset-version d) (cql-valueset-access d)
                                                    (map (lambda (r) (checked-ref d (format "valueset \"~a\"" (cql-valueset-name d)) "CodeSystemRef" r))
                                                         (cql-valueset-codesystems d))))))
  (define codes
    (of cql-code-def? (lambda (d) (elm-code-def (symbol->string (cql-code-def-name d)) (cql-code-def-id d)
                                                (cql-code-def-display d) (cql-code-def-access d)
                                                (checked-ref d (format "code \"~a\"" (cql-code-def-name d)) "CodeSystemRef" (cql-code-def-system d))))))
  (define concepts
    (of cql-concept-def? (lambda (d) (elm-concept-def (symbol->string (cql-concept-def-name d)) (cql-concept-def-display d)
                                                      (cql-concept-def-access d)
                                                      (map (lambda (r) (checked-ref d (format "concept \"~a\"" (cql-concept-def-name d)) "CodeRef" r))
                                                           (cql-concept-def-codes d))))))
  (define contexts (of cql-context? (lambda (d) (elm-context-def (symbol->string (cql-context-name d))))))
  (define statements
    (append*
     (for/list ([d (in-list defs)])
       (match d
         [(cql-context name) (if (eq? (hash-ref (lctx-defs lx) name #f) 'context)
                                 (list (elm-def (symbol->string name) (symbol->string name) #f (car (def-info lx name))))
                                 '())]
         [(? cql-def?)
          (collecting
           (lambda ()
             (list (elm-def (symbol->string (cql-def-name d)) (hash-ref (lctx-contexts lx) (cql-def-name d) "Unfiltered")
                            (cql-def-access d) (car (def-info lx (cql-def-name d))))))
           (lambda (e) (if (current-partial?) (list (raising-definition (symbol->string (cql-def-name d)) e)) '())))]
         [(? cql-func?) (collecting (lambda () (list (compile-function-def d lx))) '())]
         [_ '()]))))
  (elm-library (and (cql-library-id lib) (symbol->string (cql-library-id lib))) (cql-library-version lib)
               usings includes params codesystems valuesets codes concepts contexts statements))

;; term-ref-of : kind -> (term-ref -> elm-term-ref)   a local name or Library.name
;; known-term : kind ref -> ref   a terminology reference must name a definition of its kind in this
;; library (`code "A1c": '4548-4' from "LONIC"` used to translate, then fail per patient when run);
;; a reference through an include alias is resolved by the include
(define (known-term kind r)
  (match r
    [(cql-ref n)
     (define terms (lctx-terms (L)))
     (unless (equal? (hash-ref terms n #f) kind)
       (fail (format "no ~a named \"~a\"~a" (case kind [("CodeSystemRef") "codesystem"] [("CodeRef") "code"] [("ValueSetRef") "valueset"] [else "concept"])
                     n (did-you-mean (symbol->string n) (for/list ([(k v) (in-hash terms)] #:when (equal? v kind)) (symbol->string k))))))
     r]
    [_ r]))

(define ((term-ref-of kind) r)
  (match r
    [(cql-ref n) (elm-term-ref kind (symbol->string n) #f)]
    [(cql-property (cql-ref lib) n) (elm-term-ref kind (symbol->string n) (symbol->string lib))]))

;; def-info : lctx sym -> (cons elm CType)   compile a definition once; a cycle sees "Any"
(define (def-info lx name)
  (define memo (lctx-memo lx))
  (match (hash-ref memo name #f)
    [(? pair? r) r]
    ['pending (cons #f "Any")]
    [#f
     (hash-set! memo name 'pending)
     (define r
       (parameterize ([current-lctx lx])
         (match (hash-ref (lctx-defs lx) name)
           ['context (context-definition (symbol->string name))]
           [d (with-handlers ([cql-error? (lambda (e)
                                             ;; a failed definition reads as untyped Null to its referrers
                                             (hash-set! memo name (cons (elm-null #f) "Any"))
                                             (raise e))])
                (in-definition d (format "define \"~a\"" name)
                               (lambda () (let-values ([(n t) (compile (cql-def-body d) empty-cenv)]) (cons n t)))))])))
     (hash-set! memo name r)
     r]))

;; in-definition : def label thunk -> value   a translation error inside this definition is raised
;; positioned at it (the parser attached its location) and names it — innermost definition first
(define (in-definition d label thunk)
  (with-handlers ([(lambda (e) (and (cql-error? e) (not (cql-error-diagnostic e))))
                   (lambda (e)
                     (define msg (format "in ~a: ~a" label (cql-error-message e)))
                     (raise (cql-error (cql-error-phase e) msg
                                       (diagnostic 'cql 'compile 'error msg (attached-loc d)))))])
    (parameterize ([current-definition (cons label d)]) (thunk))))

;; context-definition : string -> (cons elm CType)   `context Patient` = SingletonFrom([Patient])
(define (context-definition cname)
  (define m (lctx-model (L)))
  (define t (model-context-type m cname))
  (cons (elm-unary "SingletonFrom" (retrieve-node t #f #f #f) #f) t))

;; compile-function-def : cql-func lctx -> elm-function-def
(define (compile-function-def d lx)
  (define operands (for/list ([o (in-list (cql-func-operands d))])
                     (cons (cql-operand-name o) (type->ctype (cql-operand-type o)))))
  (define body (cql-func-body d))
  (define node (if (eq? body 'external) 'external
                   (in-definition d (format "function \"~a\"" (cql-func-name d))
                                  (lambda () (compile1 body (cenv-bind empty-cenv 'operand operands))))))
  (elm-function-def (symbol->string (cql-func-name d)) (hash-ref (lctx-contexts lx) (cql-func-name d) "Unfiltered")
                    (cql-func-access d)
                    (for/list ([o (in-list (cql-func-operands d))])
                      (elm-operand-def (symbol->string (cql-operand-name o)) (compile-type (cql-operand-type o))))
                    (and (cql-func-result-type d) (compile-type (cql-func-result-type d)))
                    node (cql-func-fluent? d)))

;; func-result-type : lctx cql-func -> CType   declared, else inferred from the body
(define (func-result-type lx f)
  (parameterize ([current-lctx lx])
    (cond
      [(cql-func-result-type f) (type->ctype (cql-func-result-type f))]
      [(eq? (cql-func-body f) 'external) "Any"]
      [else
       (define memo (lctx-fmemo lx))
       (match (hash-ref memo f #f)
         ['pending "Any"]
         [(? values t) t]
         [#f (hash-set! memo f 'pending)
             (define ops (for/list ([o (in-list (cql-func-operands f))])
                           (cons (cql-operand-name o) (type->ctype (cql-operand-type o)))))
             (define-values (_n t) (compile (cql-func-body f) (cenv-bind empty-cenv 'operand ops)))
             (hash-set! memo f t)
             t])])))

;; ---- expression compilation: cql-ast env -> (values elm-node CType) --------
(define (fail msg) (raise (cql-error 'translate msg)))

;; compile-memo : (parameter (or hasheq #f))   ast -> (list env elm CType). Overload resolution
;; types a call's arguments before compiling them as operands; without the memo, nested calls would
;; recompile their arguments exponentially often. Keyed by the AST node and the (eq?) scope.
(define compile-memo (make-parameter #f))
(define (compile ast env)
  (define memo (compile-memo))
  (define hit (and memo (hash-ref memo ast #f)))
  (if (and hit (eq? (car hit) env) (eq? (cadddr hit) (L)))
      (values (cadr hit) (caddr hit))
      (let-values ([(n t) (compile* ast env)])
        (when memo (hash-set! memo ast (list env n t (L))))
        (values n t))))
(define (compile* ast env)
  (match ast
    [(cql-lit v) (compile-literal v)]
    [(cql-ref name) (compile-ref name env)]
    [(cql-unop op operand) (compile-unop op operand env)]
    [(cql-binop op l r) (compile-binop op l r env)]
    [(cql-if c t e) (compile-if c t e env)]
    [(cql-case comparand items els) (compile-case comparand items els env)]
    [(cql-list elems) (compile-list elems env)]
    [(cql-interval low high lc hc) (compile-interval low high lc hc env)]
    [(cql-tuple elems) (compile-tuple elems env)]
    [(cql-property target name) (compile-property target name env)]
    [(cql-call target name args) (compile-call target name args env)]
    [(cql-typeop op operand type) (compile-typeop op operand type env)]
    [(cql-cast operand type) (compile-cast operand type env)]
    [(cql-convert operand type) (compile-convert operand type env)]
    [(cql-convert-unit operand unit)
     (define-values (o _t) (sys operand env))
     (values (elm-nary "ConvertQuantity" (list o (elm-literal "String" unit)) "Quantity") "Quantity")]
    [(cql-index target idx) (compile-index target idx env)]
    [(cql-component prec operand) (compile-component prec operand env)]
    [(cql-between kind prec a b) (compile-between kind prec a b env)]
    [(cql-timing op prec a b) (compile-timing op prec a b env)]
    [(cql-timing-offset op prec q qual a b) (compile-timing-offset op prec q qual a b env)]
    [(cql-within properly? q a b) (compile-within properly? q a b env)]
    [(? cql-query?) (compile-query ast env)]
    [(? cql-retrieve?) (compile-retrieve ast env)]
    [(cql-code-selector code system display)
     (values (elm-code code ((term-ref-of "CodeSystemRef") (known-term "CodeSystemRef" system)) display) "Code")]
    [(cql-concept-selector codes display)
     (values (elm-concept (map (lambda (c) (compile1 c env)) codes) display) "Concept")]
    [(cql-minmax which type)
     (define t (or (type-simple type) "Any"))
     (values (elm-minmax which (system-type-name t) (minmax-elm which t)) t)]
    [(cql-instance type elems)
     ;; each element value is coerced to the class's element type (decimal { value: 100 } -> ToDecimal)
     (define t (named->ctype (symbol->string type)))
     (values (elm-instance (ctype->elm-name t)
                           (for/list ([te (in-list elems)])
                             (define-values (vn vt) (compile (cql-tuple-elem-value te) env))
                             (define want (property-type t (symbol->string (cql-tuple-elem-name te))))
                             (elm-tuple-elem (cql-tuple-elem-name te) (if (and want (conversion-cost vt want)) (coerce vn vt want) vn))))
             t)]
    [_ (fail (format "unsupported expression: ~s" ast))]))

(define (compile1 e env) (let-values ([(n _t) (compile e env)]) n))

;; minimum/maximum of a System type -> the boundary value as a canonical ELM literal/constructor
(define (minmax-elm which t)
  ;; DateTime boundaries are UTC (Z)
  (define (D y mo d h mi s ms) (elm-datetime (int-lit y) (int-lit mo) (int-lit d) (int-lit h) (int-lit mi) (int-lit s) (int-lit ms)
                                             (elm-literal "Decimal" "0.0")))
  (case t
    [("Integer") (elm-literal "Integer" (if (eq? which 'min) "-2147483648" "2147483647"))]
    [("Long") (elm-literal "Long" (if (eq? which 'min) "-9223372036854775808" "9223372036854775807"))]
    [("Decimal") (elm-literal "Decimal" (if (eq? which 'min) "-99999999999999999999.99999999" "99999999999999999999.99999999"))]
    [("Quantity") (elm-quantity (if (eq? which 'min) "-99999999999999999999.99999999" "99999999999999999999.99999999") "1")]
    [("DateTime") (if (eq? which 'min) (D 1 1 1 0 0 0 0) (D 9999 12 31 23 59 59 999))]
    [("Date") (if (eq? which 'min) (elm-date (int-lit 1) (int-lit 1) (int-lit 1)) (elm-date (int-lit 9999) (int-lit 12) (int-lit 31)))]
    [("Time") (if (eq? which 'min) (elm-time (int-lit 0) (int-lit 0) (int-lit 0) (int-lit 0)) (elm-time (int-lit 23) (int-lit 59) (int-lit 59) (int-lit 999)))]
    [else (fail (format "minimum/maximum not supported for type ~a" t))]))

;; singularize a units word (years -> year) and validate it's a temporal precision
(define duration-units '(year month week day hour minute second millisecond))
(define (units->prec u)
  (define s (symbol->string u))
  (string->symbol (if (regexp-match? #rx"s$" s) (substring s 0 (sub1 (string-length s))) s)))
(define (compile-between kind units a b env)
  (define prec (units->prec units))
  (unless (memq prec duration-units) (fail (format "'~a' is not a valid duration unit" units)))
  (define-values (an _at) (sys a env))
  (define-values (bn _bt) (sys b env))
  (values (elm-between kind prec an bn) "Integer"))

;; <precision> from operand — validate the precision word, emit a canonical ELM component node
(define component-precisions '(year month day hour minute second millisecond timezoneoffset timezone date time))
(define (compile-component prec operand env)
  (unless (memq prec component-precisions) (fail (format "'~a from' is not a valid component extraction" prec)))
  (define-values (o _t) (sys operand env))
  (values (elm-component prec o)
          (case prec [(timezoneoffset timezone) "Decimal"] [(date) "Date"] [(time) "Time"] [else "Integer"])))

;; ---- literals --------------------------------------------------------------
(define (compile-literal v)
  (cond
    ;; an Integer literal must fit 32 bits; a Decimal literal is limited to 28 total / 8 fractional
    ;; digits (the CQL decimal domain) — violations are compile errors (per ValueLiteralsAndSelectors)
    [(exact-integer? v)
     (when (or (< v -2147483648) (> v 2147483647)) (fail (format "Integer literal ~a is out of range" v)))
     (values (elm-literal "Integer" (number->string v)) "Integer")]
    [(dec-lit? v)
     (check-decimal-lexeme (dec-lit-lexeme v))
     (values (elm-literal "Decimal" (dec-lit-lexeme v)) "Decimal")]
    [(long-lit? v) (values (elm-literal "Long" (long-lit-lexeme v)) "Long")]
    [(string? v)   (values (elm-literal "String" v) "String")]
    [(boolean? v)  (values (elm-literal "Boolean" (if v "true" "false")) "Boolean")]
    [(eq? v 'null) (values (elm-null #f) "Any")]
    [(qty-lit? v)  (values (elm-quantity (qty-num->string (qty-lit-number v)) (qty-lit-unit v)) "Quantity")]
    [(ratio-lit? v) (values (elm-ratio (qty->elm (ratio-lit-numerator v)) (qty->elm (ratio-lit-denominator v))) "Ratio")]
    ;; @-literals compile to canonical ELM Date/DateTime/Time constructor nodes (ELM has no temporal
    ;; literal type): parse the lexeme to a moment, then emit component operands.
    [(date-lit? v)     (values (moment->elm (parse-fp-date (date-lit-str v)) 'date) "Date")]
    [(datetime-lit? v) (values (moment->elm (parse-fp-datetime (datetime-lit-str v)) 'dateTime) "DateTime")]
    [(time-lit? v)     (values (moment->elm (parse-fp-time (time-lit-str v)) 'time) "Time")]
    [else (fail (format "unsupported literal: ~s" v))]))

(define (qty-num->string n) (if (dec-lit? n) (dec-lit-lexeme n) (number->string n)))

;; check-decimal-lexeme : string -> void   enforce CQL's decimal domain: <= 28 integer digits and
;; <= 8 fractional digits (a longer lexeme cannot be represented and is a compile error)
(define (check-decimal-lexeme s)
  (define m (regexp-match #px"^[+-]?([0-9]+)(?:\\.([0-9]+))?$" s))
  (when m
    (define ip (cadr m)) (define fp (or (caddr m) ""))
    (when (> (string-length fp) 8) (fail (format "Decimal literal ~a exceeds 8 fractional digits" s)))
    (when (> (string-length ip) 28) (fail (format "Decimal literal ~a exceeds the decimal range" s)))))

;; qty->elm : (qty-lit | number-lit) -> elm-quantity   a ratio part; a bare number gets unit "1"
(define (qty->elm q)
  (if (qty-lit? q)
      (elm-quantity (qty-num->string (qty-lit-number q)) (qty-lit-unit q))
      (elm-quantity (qty-num->string q) "1")))

;; ---- canonical temporal ELM construction -----------------------------------
(define (int-lit n) (elm-literal "Integer" (number->string n)))
(define (opt-int n) (and n (int-lit n)))
(define (frac->ms f) (and f (inexact->exact (round (* (or (string->number (string-append "0." f)) 0) 1000)))))
;; moment->elm : moment kind -> elm-date | elm-datetime | elm-time
(define (moment->elm m kind)
  (case kind
    [(date) (elm-date (opt-int (moment-year m)) (opt-int (moment-month m)) (opt-int (moment-day m)))]
    [(time) (elm-time (opt-int (moment-hour m)) (opt-int (moment-minute m)) (opt-int (moment-second m))
                      (let ([ms (frac->ms (moment-frac m))]) (and ms (int-lit ms))))]
    [(dateTime)
     (elm-datetime (opt-int (moment-year m)) (opt-int (moment-month m)) (opt-int (moment-day m))
                   (opt-int (moment-hour m)) (opt-int (moment-minute m)) (opt-int (moment-second m))
                   (let ([ms (frac->ms (moment-frac m))]) (and ms (int-lit ms)))
                   (let ([tz (moment-tzmin m)])
                     (and tz (elm-literal "Decimal" (number->string (exact->inexact (/ tz 60)))))))]))

;; ---- implicit conversion ---------------------------------------------------
;; A model type meets a System operator through the model's conversion function — but only when the
;; library includes it (FHIRHelpers): inside FHIRHelpers itself, `value.value` is written explicitly.
;; to-system : elm CType -> (values elm CType)
(define (to-system n t)
  (cond
    ;; a list of model values converts element-wise: (X in list return all Convert(X))
    [(and (list-ctype? t) (string? (ctype-element t)) (convertible? (ctype-element t)))
     (define-values (cn ct) (to-system* (elm-alias-ref "X") (ctype-element t)))
     (values (elm-query (list (cons "X" n)) '() '() #f cn 'all #f #f) (list-ctype ct))]
    [else (to-system* n t)]))
(define (convertible? t)
  (define m (lctx-model (L)))
  (define conv (and m (model-conversion m t)))
  (define fn (and conv (regexp-match #rx"^([^.]+)[.](.+)$" (cdr conv))))
  (and fn (include-alias-for (string->symbol (cadr fn))) #t))
(define (to-system* n t)
  (define m (lctx-model (L)))
  (define conv (and m (string? t) (model-conversion m t)))
  (define fn (and conv (regexp-match #rx"^([^.]+)[.](.+)$" (cdr conv))))
  (define alias (and fn (include-alias-for (string->symbol (cadr fn)))))
  (if alias
      (values (elm-function-ref (caddr fn) alias (list n) (let ([s (ctype->elm-type t)]) (and s (list s)))) (car conv))
      (values n t)))
(define (sys e env) (let-values ([(n t) (compile e env)]) (to-system n t)))

;; sys-terminology : cql cenv -> (values elm CType)   the code side of `x in "ValueSet"`: a choice
;; is cast to its first member converting to String, Code or Concept (InValueSet's overload order)
(define (sys-terminology e env)
  (define-values (n t) (compile e env))
  (cond
    [(choice-ctype? t)
     (define member (for*/first ([target (in-list '("String" "Code" "Concept"))]
                                 [c (in-list (choice-members t))]
                                 #:when (let ([k (conversion-cost c target)]) (and k (= k 1))))
                      c))
     (if member (to-system (elm-as n (ctype->elm-name member) #f) member) (values n t))]
    [else (to-system n t)]))

;; sys-against : cql cql cenv -> (values elm CType)   like sys, but a CHOICE operand meeting a
;; concretely typed one is first cast to the member that converts to it — `O.value > 5 'mg'` is
;; ToQuantity(O.value as FHIR.Quantity) (an implicit As, written with asType)
(define (sys-against e other env)
  (define-values (n t) (compile e env))
  (cond
    [(choice-ctype? t)
     (define-values (_on ot) (sys other env))
     (define target (cond [(and (string? ot) (not (equal? ot "Any"))) ot]
                          [(interval-ctype? ot) ot]
                          [else #f]))
     (if target (choice-to n t target) (values n t))]
    ;; a member value against an interval whose points are the choice (`Interval[C.onset,
    ;; C.abatement] includes E.period`): cast to the whole choice, the interval's own point type
    [(and (string? t) (member-of-interval-choice t other env))
     => (lambda (ch) (values (elm-as n (ctype->elm-type ch) #f) ch))]
    [else (to-system n t)]))
(define (member-of-interval-choice t other env)
  (define-values (_on ot) (compile other env))
  (and (interval-ctype? ot) (choice-ctype? (ctype-element ot))
       (member t (choice-members (ctype-element ot))) (ctype-element ot)))

;; choice-to : elm CType(choice) CType -> (values elm CType)   the choice members that convert to
;; `target`: none -> unchanged ; one -> Convert(x as M) ; several -> a Case over `x is M`
(define (choice-to n t target)
  (define fits (for/list ([c (in-list (choice-members t))]
                          #:when (let ([k (conversion-cost c target)]) (and k (<= k 1))))
                 c))
  ;; a member that IS the target is the one read (a mapped Choice<Quantity, Integer, …> compared with
  ;; a Quantity: value as Quantity, not a Case that promotes the Integer too)
  (define exact (filter (lambda (c) (equal? c target)) fits))
  (define members (if (pair? exact) exact fits))
  ;; the choice-timing trap: against an interval the members read are the interval-typed ones; a
  ;; member that is a POINT of the interval (an Observation's effectiveDateTime against a DateTime
  ;; interval) is left out and gives null. The reference translator does the same; say so.
  (when (interval-ctype? target)
    (define points (for/list ([c (in-list (choice-members t))]
                              #:unless (member c members)
                              #:when (let ([k (conversion-cost c (ctype-element target))]) (and k (<= k 1))))
                     c))
    (when (and (pair? points) (pair? members))
      (warn! (format "~a against an interval reads only its ~a member~a: a ~a value gives null. Say which member you mean: (… as ~a)"
                     (ctype->string t) (string-join (map ctype->string members) ", ") (if (null? (cdr members)) "" "s")
                     (ctype->string (car points)) (ctype->string (car points))))))
  ;; a member's type: a name, or a specifier for an interval or list member (Interval<Quantity>)
  (define (spec m) (if (string? m) (ctype->elm-name m) (ctype->elm-type m)))
  (define (conv m) (let-values ([(cn _ct) (to-system (elm-as n (spec m) #f) m)]) cn))
  (match members
    ['() (values n t)]
    [(list m) (to-system (elm-as n (spec m) #f) m)]
    [ms (values (elm-case #f (for/list ([m (in-list ms)]) (elm-case-item (elm-is n (spec m)) (conv m)))
                          (elm-null #f))
                target)]))

;; System implicit promotions, the translator's order: Integer -> Long -> Decimal -> Quantity,
;; Date -> DateTime, Code -> Concept — and an interval of one (Interval<Date> -> Interval<DateTime>).
(define promotions
  (hash (cons "Integer" "Decimal") "ToDecimal" (cons "Integer" "Long") "ToLong" (cons "Long" "Decimal") "ToDecimal"
        (cons "Integer" "Quantity") "ToQuantity" (cons "Decimal" "Quantity") "ToQuantity"
        (cons "Date" "DateTime") "ToDateTime" (cons "Code" "Concept") "ToConcept"))
;; promotion : CType CType -> string|#f   the converting operator from one type to the other
(define (promotion from to)
  (cond [(and (string? from) (string? to)) (hash-ref promotions (cons from to) #f)]
        [(and (interval-ctype? from) (interval-ctype? to)) (promotion (ctype-element from) (ctype-element to))]
        [else #f]))
;; promote : elm CType CType -> elm   (no-op when not applicable). An interval is rebuilt over its
;; converted bounds, the closed flags read from the original — the reference translator's form
;; (FHIRCommon's ToInterval: an Interval<Date> arm beside Interval<DateTime> ones):
;;   Interval[ToDateTime(X.low), ToDateTime(X.high)] with lowClosedExpression X.lowClosed, …
(define (promote n from to)
  (define op (promotion from to))
  (cond [(not op) n]
        [(interval-ctype? from)
         (define (bound p) (elm-unary op (elm-property n p #f) (ctype-element to)))
         (elm-interval (bound "low") (bound "high") (elm-property n "lowClosed" #f) (elm-property n "highClosed" #f))]
        [else (elm-unary op n to)]))
;; unify-pair : elm CType elm CType -> (values elm elm CType)   promote the narrower operand
(define (unify-pair a at b bt)
  (cond [(promotion at bt) (values (promote a at bt) b bt)]
        [(promotion bt at) (values a (promote b bt at) at)]
        [else (values a b at)]))
;; unify-timing : elm CType elm CType -> (values elm elm)   a point or interval operand meets the
;; other's point type: the narrower side is promoted — a Date point against an Interval<DateTime>,
;; an Interval<Date> against a DateTime point (Example's `… 30 days or less before Today()`)
(define (point-of t) (if (interval-ctype? t) (ctype-element t) t))
(define (widen t p) (if (interval-ctype? t) (interval-ctype p) p))
(define (unify-timing a at b bt)
  (define pa (point-of at)) (define pb (point-of bt))
  (cond [(and (string? pa) (string? pb) (promotion pa pb)) (values (promote a at (widen at pb)) b)]
        [(and (string? pa) (string? pb) (promotion pb pa)) (values a (promote b bt (widen bt pa)))]
        [else (values a b)]))

;; ---- references ------------------------------------------------------------
;; compile-ref : sym cenv -> (values elm CType)   local scope, then the library, then the sort row
(define (compile-ref name env)
  (define s (symbol->string name))
  (match (local-ref env name)
    [(cons 'alias t) (values (elm-alias-ref s) t)]
    [(cons 'let t) (values (elm-query-let-ref s) t)]
    [(cons 'row t) (values (elm-identifier-ref s) t)]                 ; $this, the sort row
    [(cons _ t) (values (elm-operand-ref s) t)]
    [#f
     (define lx (L))
     (cond
       [(hash-ref (lctx-defs lx) name #f) (values (elm-expression-ref s #f) (cdr (def-info lx name)))]
       [(hash-ref (lctx-params lx) name #f) => (lambda (p) (values (elm-parameter-ref s #f) (param-type p)))]
       [(hash-ref (lctx-terms lx) name #f) => (lambda (k) (values (elm-term-ref k s #f) (term-type k)))]
       [(and (cenv-row env) (eq? name '$this)) (values (elm-identifier-ref s) (cenv-row env))]
       [(and (cenv-row env) (mapped-element? (cenv-row env) s))
        (map-property #f #f (cenv-row env) s #:value (elm-identifier-ref s))]
       [(and (cenv-row env) (property-type (cenv-row env) s))
        => (lambda (t) (values (elm-identifier-ref s) t))]
       [(and (cenv-this env) (property-type (cdr (local-ref env (string->symbol (cenv-this env)))) s))
        => (lambda (t) (values (elm-property #f s (cenv-this env)) t))]
       [else (fail (format "unknown identifier '~a'~a" name (did-you-mean s (identifier-candidates env))))])]))

;; identifier-candidates : cenv -> (listof string)   the names a reference could mean here: the
;; library's defines, parameters and terminology, and the names in local scope (for "did you mean")
(define (identifier-candidates env)
  (define lx (L))
  (map (lambda (k) (if (symbol? k) (symbol->string k) (format "~a" k)))
       (append (hash-keys (lctx-defs lx)) (hash-keys (lctx-params lx)) (hash-keys (lctx-terms lx))
               (hash-keys (cenv-locals env)))))

(define (term-type kind)
  (case kind [("ValueSetRef") "ValueSet"] [("CodeSystemRef") "CodeSystem"] [("CodeRef") "Code"] [else "Concept"]))
(define (param-type p)
  (cond [(cql-parameter-type p) (type->ctype (cql-parameter-type p))]
        [(cql-parameter-default p) (let-values ([(_n t) (compile (cql-parameter-default p) empty-cenv)]) t)]
        [else "Any"]))

;; a reference INTO an included library: Alias."name" -> (values elm CType) | #f when not a declared name
(define (compile-qualified-ref alias name)
  (define a (symbol->string alias))
  (define s (symbol->string name))
  (define lx (included alias))
  (cond
    [(not lx) (values (elm-expression-ref s a) "Any")]          ; unresolved include: untyped
    [(hash-ref (lctx-defs lx) name #f) (values (elm-expression-ref s a) (cdr (def-info lx name)))]
    [(hash-ref (lctx-params lx) name #f)
     => (lambda (p) (values (elm-parameter-ref s a) (parameterize ([current-lctx lx]) (param-type p))))]
    [(hash-ref (lctx-terms lx) name #f) => (lambda (k) (values (elm-term-ref k s a) (term-type k)))]
    [else
     ;; the nearest names of that library, written as they are referenced: Alias."Name"
     (define near (closest-names s (map (lambda (k) (format "~a" k))
                                        (append (hash-keys (lctx-defs lx)) (hash-keys (lctx-params lx)) (hash-keys (lctx-terms lx))))))
     (fail (format "unknown identifier '~a.~a'~a" alias name
                   (if (null? near) ""
                       (format " (did you mean ~a?)" (string-join (for/list ([n (in-list near)]) (format "`~a.\"~a\"`" alias n)) " or ")))))]))

(define (include-alias? env x)
  (and (cql-ref? x) (not (local-ref env (cql-ref-name x)))
       (not (hash-ref (lctx-defs (L)) (cql-ref-name x) #f))
       (hash-ref (lctx-includes (L)) (cql-ref-name x) #f) #t))

;; ---- unary -----------------------------------------------------------------
(define (compile-unop op operand env)
  ;; a negated integer literal folds to ONE literal (so -2147483648, the 32-bit min, is a valid
  ;; Integer; magnitudes beyond the range promote to Long like positive literals)
  (cond
    [(and (eq? op '-) (cql-lit? operand) (exact-integer? (cql-lit-value operand)))
     (define nv (- (cql-lit-value operand)))
     (when (or (< nv -2147483648) (> nv 2147483647)) (fail (format "Integer literal ~a is out of range" nv)))
     (values (elm-literal "Integer" (number->string nv)) "Integer")]
    [else (compile-unop* op operand env)]))
(define (compile-unop* op operand env)
  (case op
    [(+) (compile operand env)]                                        ; unary plus is identity
    [(-) (let-values ([(o t) (sys operand env)]) (values (elm-unary "Negate" o (simple-rt t)) t))]
    [(not) (let-values ([(o _t) (sys operand env)]) (values (elm-unary "Not" o "Boolean") "Boolean"))]
    [(exists) (let-values ([(o _t) (compile operand env)]) (values (elm-unary "Exists" o "Boolean") "Boolean"))]
    [else (fail (format "unsupported unary operator ~a" op))]))
;; the resultType stamped on an elm node: only System simple names (model/composite types: none)
(define (simple-rt t) (and (string? t) (not (regexp-match? #rx"[.]" t)) (not (equal? t "Any")) t))

;; ---- binary ----------------------------------------------------------------
(define generic-binops '(union intersect except))
;; A union chain groups the way the reference translator's parse does: adjacent operands pair up,
;; the pairs fold left, an odd operand comes last — A u B u C u D u E = ((A u B) u (C u D)) u E.
;; union-chain : cql -> (listof cql)   the operands of a left-deep union chain
(define (union-chain e)
  (match e [(cql-binop 'union l r) (append (union-chain l) (list r))] [_ (list e)]))
(define (regroup-union operands)
  (define pairs (let loop ([xs operands])
                  (match xs
                    [(list a b rest ...) (cons (cql-binop 'union a b) (loop rest))]
                    [(list a) (list a)]
                    ['() '()])))
  (for/fold ([acc (car pairs)]) ([p (in-list (cdr pairs))]) (cql-binop 'union acc p)))

(define (compile-binop op l r env)
  (define chain (and (eq? op 'union) (union-chain (cql-binop 'union l r))))
  (cond
    [(and chain (> (length chain) 3) (not (regrouped? l)))
     (define g (regroup-union chain))
     (compile-binop 'union (mark-regrouped (cql-binop-lhs g)) (cql-binop-rhs g) env)]
    [(and (eq? op 'union) (union-of-different-types l r env)) => (lambda (r) (values (car r) (cadr r)))]
    [(memq op generic-binops)
     (define-values (lo lt) (compile l env))
     (define-values (ro rt) (compile r env))
     (define name (case op [(union) "Union"] [(intersect) "Intersect"] [else "Except"]))
     (values (elm-nary name (list lo ro) (simple-rt (or (composite-or lt rt) "Any"))) (or (composite-or lt rt) lt))]
    [(memq op '(in contains)) (compile-membership op l r env)]
    [else
     (define-values (lo0 lt0) (sys-against l r env))
     (define-values (ro0 rt0) (sys-against r l env))
     (define-values (lo ro t) (if (memq op '(& and or xor implies)) (values lo0 ro0 lt0) (unify-pair lo0 lt0 ro0 rt0)))
     (define lt (if (eq? lo lo0) lt0 t))
     (define rt (if (eq? ro ro0) rt0 t))
     (compile-binop* op lo lt ro rt)]))

(define (compile-binop* op lo lt ro rt)
  (define (bin name res) (values (elm-nary name (list lo ro) (simple-rt res)) res))
  (case op
    [(+) (if (or (equal? lt "String") (equal? rt "String"))
             (bin "Concatenate" "String")                    ; CQL `+` on strings is null-propagating
             (bin "Add" (arith-type lt rt)))]
    ;; CQL `&` treats null as '' — desugar to Concatenate(Coalesce(l,''), Coalesce(r,''))
    [(&) (define empty (elm-literal "String" ""))
         (values (elm-nary "Concatenate"
                           (list (elm-nary "Coalesce" (list lo empty) "String")
                                 (elm-nary "Coalesce" (list ro empty) "String"))
                           "String")
                 "String")]
    [(-) (bin "Subtract" (arith-type lt rt))]
    [(*) (bin "Multiply" (arith-type lt rt))]
    [(/) (bin "Divide" (if (equal? (numeric-or lt rt) "Quantity") "Quantity" "Decimal"))]
    [(div) (bin "TruncatedDivide" (numeric-or lt rt))]
    [(mod) (bin "Modulo" (numeric-or lt rt))]
    [(^) (bin "Power" (numeric-or lt rt))]
    [(=) (bin "Equal" "Boolean")]
    [(!=) (values (elm-unary "Not" (elm-nary "Equal" (list lo ro) "Boolean") "Boolean") "Boolean")]
    [(~) (bin "Equivalent" "Boolean")]
    [(!~) (values (elm-unary "Not" (elm-nary "Equivalent" (list lo ro) "Boolean") "Boolean") "Boolean")]
    [(<) (bin "Less" "Boolean")]
    [(>) (bin "Greater" "Boolean")]
    [(<=) (bin "LessOrEqual" "Boolean")]
    [(>=) (bin "GreaterOrEqual" "Boolean")]
    [(and) (bin "And" "Boolean")]
    [(or) (bin "Or" "Boolean")]
    [(xor) (bin "Xor" "Boolean")]
    [(implies) (bin "Implies" "Boolean")]
    [else (fail (format "unsupported binary operator ~a" op))]))

;; arithmetic on temporals keeps the temporal type (DateTime - 1 day is a DateTime)
(define (arith-type a b)
  (cond [(member a '("Date" "DateTime" "Time")) a]
        [(or (equal? a "Any") (equal? b "Any")) "Any"]
        [else (numeric-or a b)]))

;; membership: `x in ValueSet` -> InValueSet (AnyInValueSet for a list) ; `x in CodeSystem` ->
;; InCodeSystem ; otherwise In/Contains over a list or interval, the point promoted to its element type
(define (compile-membership op l r env)
  (define-values (ro rt) (sys-against r l env))
  (define-values (lo lt)
    (if (member rt '("ValueSet" "CodeSystem"))
        (sys-terminology l env)
        (sys-against l r env)))
  (cond
    [(and (eq? op 'in) (member rt '("ValueSet" "CodeSystem")))
     (define kind (string-append (if (list-ctype? lt) "Any" "") (if (equal? rt "ValueSet") "InValueSet" "InCodeSystem")))
     (values (elm-in-valueset kind lo ro) "Boolean")]   ; (json: code vs codes by kind)
    [(eq? op 'in)
     (define el (ctype-element rt))
     (values (elm-nary "In" (list (if el (promote lo lt el) lo) ro) "Boolean") "Boolean")]
    [else
     (define el (ctype-element lt))
     (values (elm-nary "Contains" (list lo (if el (promote ro rt el) ro)) "Boolean") "Boolean")]))

;; numeric promotion Integer < Long < Decimal < Quantity ; #f/Any when not numeric
(define numeric-rank (hash "Integer" 1 "Long" 2 "Decimal" 3 "Quantity" 4))
(define (numeric-or a b)
  (define ra (hash-ref numeric-rank a #f))
  (define rb (hash-ref numeric-rank b #f))
  (cond [(and ra rb) (if (>= ra rb) a b)]
        [ra a] [rb b] [else "Any"]))
(define (composite-or a b) (cond [(compound-ctype? a) a] [(compound-ctype? b) b] [else #f]))

;; union-of-different-types : cql cql cenv -> (list elm CType) | #f
;; List<A> union List<B> of unrelated model types: both cast to List<Choice<A, B>>, as the reference does
(define (union-of-different-types l r env)
  (define-values (lo lt) (compile l env))
  (define-values (ro rt) (compile r env))
  (define m (lctx-model (L)))
  (define (el t) (and (list-ctype? t) (ctype-element t)))
  (define a (el lt)) (define b (el rt))
  (define members choice-members)
  (cond
    [(and m a b (not (equal? a b)) (or (model-type? a) (compound-ctype? a)) (or (model-type? b) (compound-ctype? b))
          (not (and (string? a) (string? b) (or (model-subtype? m a b) (model-subtype? m b a)))))
     (define ct (list-ctype (choice-ctype (remove-duplicates (append (members a) (members b))))))
     (define spec (ctype->elm-type ct))
     (define (cast n t) (if (equal? t ct) n (elm-as n spec #f)))
     (list (elm-nary "Union" (list (cast lo lt) (cast ro rt)) #f) ct)]
    [else #f]))

;; a regrouped chain's left side must not be regrouped again (it is itself a union chain)
(define regrouped (make-weak-hasheq))
(define (mark-regrouped e) (hash-set! regrouped e #t) e)
(define (regrouped? e) (hash-ref regrouped e #f))

;; ---- timing ----------------------------------------------------------------
;; compile-timing : op prec cql cql cenv -> (values elm "Boolean")
;; point-vs-interval picks the ELM form: `point during I` is In, `I includes point` is Contains
(define (compile-timing op prec a b env)
  (define-values (an0 at0) (sys-against a b env))
  (define-values (bn0 bt0) (sys-against b a env))
  (define-values (an bn) (unify-timing an0 at0 bn0 bt0))
  (define a-point? (not (interval-ctype? at0)))
  (define b-point? (not (interval-ctype? bt0)))
  (define op*
    (case op
      [(included-in during) (if (and a-point? (interval-ctype? bt0)) 'in op)]
      [(properly-included-in) (if (and a-point? (interval-ctype? bt0)) 'proper-in op)]
      [(includes) (if (and b-point? (interval-ctype? at0)) 'contains op)]
      [(properly-includes) (if (and b-point? (interval-ctype? at0)) 'proper-contains op)]
      [else op]))
  (values (elm-timing op* (and prec (units->prec prec)) an bn) "Boolean"))

;; A <quantity> [or less|or more|…] before|after B — the translator's expansion:
;;   or less / less than : In(A, Interval[B - q, B]) and not (B is null)   (bounds per phrase)
;;   or more / more than : SameOrBefore|Before(A, B - q) (after: SameOrAfter|After(A, B + q))
;;   exact               : SameAs(A, B -/+ q)
;; an interval A is reduced to its end (before) or start (after); an interval B to its start
;; (before) or end (after).
(define (compile-timing-offset op prec q qual a b env)
  (define-values (an0 at) (sys-against a b env))
  (define-values (bn0 bt) (sys-against b a env))
  (define before? (memq op '(before same-or-before)))
  (define on? (memq op '(same-or-before same-or-after)))
  (define an (if (interval-ctype? at) (elm-unary (if before? "End" "Start") an0 #f) an0))
  (define bn (if (interval-ctype? bt) (elm-unary (if before? "Start" "End") bn0 #f) bn0))
  (define pa (point-of at)) (define pb (point-of bt))
  (define-values (qn _qt) (compile-literal q))
  (define shifted (elm-nary (if before? "Subtract" "Add") (list bn qn) #f))
  (define p (and prec (units->prec prec)))
  (case qual
    [(or-less less-than)
     (define edge-closed (and (eq? qual 'or-less) #t))
     (define iv0 (if before?
                     (elm-interval shifted bn edge-closed (and on? #t))
                     (elm-interval bn shifted (and on? #t) edge-closed)))
     ;; a DateTime A against a Date B: the interval around B is promoted (ToDateTime over its bounds)
     (define-values (an* iv) (unify-timing an pa iv0 (interval-ctype pb)))
     (values (elm-nary "And" (list (elm-timing 'in p an* iv)
                                   (elm-unary "Not" (elm-unary "IsNull" bn "Boolean") "Boolean"))
                       "Boolean")
             "Boolean")]
    [(or-more more-than)
     (define t (if before? (if (eq? qual 'or-more) 'same-or-before 'before) (if (eq? qual 'or-more) 'same-or-after 'after)))
     (define-values (an* sh) (unify-timing an pa shifted pb))
     (values (elm-timing t p an* sh) "Boolean")]
    [else (define-values (an* sh) (unify-timing an pa shifted pb))
          (values (elm-timing 'same-as p an* sh) "Boolean")]))

;; A [properly] within q of B : In(A, Interval[B - q, B + q]) and not (B is null)
(define (compile-within properly? q a b env)
  (define-values (an0 at) (sys a env))
  (define-values (bn0 bt) (sys b env))
  (define-values (qn _qt) (compile-literal q))
  (define closed (not properly?))
  (values (elm-nary "And"
                    (list (elm-timing 'in #f an0 (elm-interval (elm-nary "Subtract" (list bn0 qn) #f)
                                                              (elm-nary "Add" (list bn0 qn) #f) closed closed))
                          (elm-unary "Not" (elm-unary "IsNull" bn0 "Boolean") "Boolean"))
                    "Boolean")
          "Boolean"))

;; ---- retrieve --------------------------------------------------------------
;; [Type: path comparator terminology] -> Retrieve over the model's class (profile url = templateId,
;; default code path = the class's primaryCodePath; a Code terminology becomes ToList(code), compared ~)
(define (compile-retrieve r env)
  (match-define (cql-retrieve _ctx type path cmp term) r)
  (define m (or (lctx-model (L)) (fail "a retrieve needs a data model (`using`)")))
  (define qname (or (model-resolve-type m (symbol->string type)) (fail (format "unknown model type ~a" type))))
  (define-values (codes ct) (if term (compile term env) (values #f #f)))
  (define code-property (and term (or (and path (symbol->string path))
                                      (class-info-primary-code-path (model-class m qname)))))
  (define comparator (and term (cond [cmp (symbol->string cmp)]
                                     [(member ct '("ValueSet" "CodeSystem")) "in"]
                                     [else "~"])))
  ;; a single (non-list, non-ValueSet) terminology becomes a list
  (define codes* (and term (if (or (list-ctype? ct) (member ct '("ValueSet" "CodeSystem")))
                               codes
                               (elm-unary "ToList" codes #f))))
  (values (if (and (not cmp) (equal? ct "ValueSet") (valueset-alternative? m qname code-property))
              ;; the element maps a ValueSet as well (QI-Core's NotDone profiles: the value set the
              ;; action was not done for): those records match by `~`, the others by `in`
              (elm-nary "Union" (list (retrieve-node qname code-property "in" codes*)
                                      (retrieve-node qname code-property "~" codes*))
                        #f)
              (retrieve-node qname code-property comparator codes*))
          (list-ctype qname)))
;; valueset-alternative? : model class path -> boolean   the element's mapping has a System.ValueSet
;; alternative
(define (valueset-alternative? m qname path)
  (define target (and (mapping-model? m) path (not (regexp-match? #rx"[.]" path)) (model-element-target m qname path)))
  (and target (for/or ([alt (in-list (parse-mapping target))]) (equal? (car alt) "System.ValueSet"))))
(define (retrieve-node qname code-property comparator codes)
  (define m (lctx-model (L)))
  (elm-retrieve (if (mapping-model? m)
                    (format "{~a}~a" (model-target-url m) (physical-name m qname))
                    (format "{~a}~a" (model-url m) (local-name qname)))
                (class-info-identifier (model-class m qname)) code-property comparator codes))
(define (local-name q) (let ([m (regexp-match #rx"[.]([^.]+)$" q)]) (if m (cadr m) q)))

;; ---- mapping models ------------------------------------------------------------------------
;; A library using a mapping model (QI-Core, US Core: a ModelInfo with a targetUrl) is typed by that
;; model's classes, and its ELM reads the target model's data, as the reference translator writes it:
;; a retrieve names the target class (the profile is its templateId), a type in the ELM names the
;; target class, and an element with a `target` mapping (mapping.rkt) is the mapping's ELM — %value
;; the target element read from the source, %parent the source; a filter is a query over $this whose
;; conditions compare the elements' System values (First at each list along a condition's path), and
;; whose remaining steps are one dotted property returned; a single element over a list of matches is
;; SingletonFrom it; a function of a list element maps every item. Types under the mapping are the
;; target model's; the property's type is the mapping model's declared one.
(define (mapping-model? m) (and m (model-target-url m) #t))
(define target-models (make-weak-hasheq))
(define (target-model m)
  (hash-ref! target-models m (lambda () (or (find-model-by-url (model-target-url m))
                                            (fail (format "~a ~a reads ~a, and no model with that url is installed"
                                                          (model-name m) (model-version m) (model-target-url m)))))))
(define (strip-ns q) (let ([x (regexp-match #rx"^[^.]+[.](.*)$" q)]) (if x (cadr x) q)))
;; physical-name : model class -> the target model's class, unqualified ("Observation" for a profile
;; whose target is Observation; "Encounter.Participant" for a nested class)
(define (physical-name m cls)
  (define ci (model-class m cls))
  (or (and ci (class-info-target ci)) (strip-ns cls)))
(define (physical-class m cls) (string-append (model-name (target-model m)) "." (physical-name m cls)))

;; mapped-element? : CType name -> boolean   an element of a mapping model's class with a mapping
(define (mapped-element? t name)
  (define m (lctx-model (L)))
  (and (mapping-model? m) (string? t) (model-class m t) (model-element-target m t name) #t))

;; map-property : source-elm|#f alias|#f class name [#:value elm] -> (values elm CType)
;; #:value is the element as read already (a `sort by` row's IdentifierRef, a list item's
;; `$this.p`), #:parent what %parent reads
(define (map-property src alias cls name #:value [value #f] #:parent [parent #f])
  (define m (lctx-model (L)))
  (define tm (target-model m))
  (define decl (or (property-type cls name) "Any"))
  (define term (cdr (car (parse-mapping (model-element-target m cls name)))))
  (define phys (physical-class m cls))
  (define vnode (cond [value] [alias (elm-property #f name alias)] [else (elm-property src name #f)]))
  (define vtype (or (model-element-type tm phys name) "Any"))
  (define pnode (cond [parent] [alias (elm-alias-ref alias)] [else src]))
  (define-values (n _t many?) (map-term term tm vnode vtype pnode phys decl))
  (values (if (and many? (not (list-ctype? decl))) (elm-unary "SingletonFrom" n #f) n) decl))

(define (elem* t) (if (list-ctype? t) (ctype-element t) t))

;; map-term : Term model value-elm value-type parent-elm parent-type decl -> (values elm CType many?)
(define (map-term t tm vnode vtype pnode ptype decl)
  (match t
    [(map-call name arg)
     (define-values (a at many?) (map-term arg tm vnode vtype pnode ptype decl))
     (define fn (or (regexp-match #rx"^(.*)[.]([^.]+)$" name) (fail (format "a mapping calls ~a, not a library function" name))))
     (define alias (or (include-alias-for (string->symbol (cadr fn)))
                       (fail (format "reading this element needs ~a: include it (the mapping is ~a)" (cadr fn) name))))
     (define (call x xt) (elm-function-ref (caddr fn) alias (list x) (let ([s (ctype->elm-type xt)]) (and s (list s)))))
     (cond [(and many? (list-ctype? decl))
            (values (elm-query (list (cons "$this" a)) '() '() #f (call (elm-alias-ref "$this") (elem* at)) 'all #f #f) decl #t)]
           [many? (values (call (elm-unary "SingletonFrom" a #f) (elem* at)) decl #f)]
           [else (values (call a at) decl #f)])]
    [(map-path root steps)
     (define-values (n0 t0) (if (eq? root 'value) (values vnode vtype) (values pnode ptype)))
     (walk-steps tm n0 t0 (list-ctype? t0) steps)]))

;; walk-steps : model elm CType many? steps -> (values elm CType many?)
(define (walk-steps tm n t many? steps)
  (match steps
    ['() (values n t many?)]
    [(cons (map-field f) rest)
     (define et (or (and (string? (elem* t)) (model-element-type tm (elem* t) f)) "Any"))
     (walk-steps tm (elm-property n f #f) et (or many? (list-ctype? et)) rest)]
    [(cons (map-filter conds) rest)
     (define item (elem* t))
     (define where (for/fold ([w #f]) ([c (in-list conds)])
                     (define e (condition->elm tm item c))
                     (if w (elm-nary "And" (list w e) "Boolean") e)))
     (define-values (fields after) (splitf-at rest map-field?))
     (define rt (for/fold ([x item]) ([f (in-list fields)])
                  (or (and (string? (elem* x)) (model-element-type tm (elem* x) (map-field-name f))) "Any")))
     (define ret (and (pair? fields) (elm-property (elm-alias-ref "$this") (string-join (map map-field-name fields) ".") #f)))
     (walk-steps tm (elm-query (list (cons "$this" n)) '() '() where ret 'all #f #f) (list-ctype (elem* rt)) #t after)]))

;; condition->elm : model item-class (cons path text) -> elm   path's System value = text
(define (condition->elm tm item c)
  (match-define (cons path text) c)
  (define-values (n t)
    (for/fold ([n #f] [t item]) ([p (in-list path)])
      (define et (or (and (string? (elem* t)) (model-element-type tm (elem* t) p)) "Any"))
      (values (if n (elm-property (if (list-ctype? t) (elm-unary "First" n #f) n) p #f) (elm-property #f p "$this")) et)))
  (elm-nary "Equal" (list (target-to-system tm n (elem* t)) (elm-literal "String" text)) "Boolean"))
;; target-to-system : model elm type -> elm   through the target model's conversion (FHIRHelpers.ToString)
(define (target-to-system tm n t)
  (define conv (and (string? t) (model-conversion tm t)))
  (define fn (and conv (regexp-match #rx"^([^.]+)[.](.+)$" (cdr conv))))
  (define alias (and fn (include-alias-for (string->symbol (cadr fn)))))
  (if alias (elm-function-ref (caddr fn) alias (list n) (let ([s (ctype->elm-type t)]) (and s (list s)))) n))

;; ---- queries ---------------------------------------------------------------
;; sources/lets bind aliases with their element types; relationships, where, return and aggregate
;; compile in that scope; `sort by` items compile against the result row (IdentifierRef / ByColumn).
(define (compile-query q env)
  (define srcs (for/list ([s (in-list (cql-query-sources q))])
                 (define-values (n t) (compile (cql-source-expr s) env))
                 (list (cql-source-alias s) n t)))
  (define single? (= 1 (length srcs)))
  (define (elem t) (if (list-ctype? t) (ctype-element t) t))
  (define env1 (cenv-bind env 'alias (for/list ([s (in-list srcs)]) (cons (first s) (elem (third s))))))
  (define-values (env2 let-elms)
    (for/fold ([e env1] [acc '()] #:result (values e (reverse acc))) ([l (in-list (cql-query-lets q))])
      (define-values (n t) (compile (cql-let-def-expr l) e))
      (values (cenv-bind e 'let (list (cons (cql-let-def-name l) t)))
              (cons (cons (symbol->string (cql-let-def-name l)) n) acc))))
  (define rels
    (for/list ([r (in-list (cql-query-rels q))])
      (define src (cql-relationship-source r))
      (define-values (n t) (compile (cql-source-expr src) env2))
      (define envR (cenv-bind env2 'alias (list (cons (cql-source-alias src) (elem t)))))
      (elm-relationship (cql-relationship-kind r) (symbol->string (cql-source-alias src)) n
                        (let-values ([(sn _st) (sys (cql-relationship-such-that r) envR)]) sn))))
  (define where (and (cql-query-where q) (let-values ([(n _t) (sys (cql-query-where q) env2)]) n)))
  (define-values (ret ret-t)
    (if (cql-query-ret q)
        (compile (cql-return-expr (cql-query-ret q)) env2)
        ;; a multi-source query without a return yields a tuple of its aliases (the translator's
        ;; Tuple{QualifyingEncounter: Encounter, SelectedProcedure: Procedure})
        (values #f (if single? (elem (third (car srcs)))
                       (tuple-ctype (for/list ([s (in-list srcs)]) (cons (string->symbol (format "~a" (first s))) (elem (third s)))))))))
  (define ret-distinct (and (cql-query-ret q) (cql-return-distinct? (cql-query-ret q))))
  ;; the accumulator has the starting value's type (`starting (null as List<Interval<Date>>)`), and
  ;; so does the result; with no typed start, the body's
  (define-values (agg agg-t)
    (match (cql-query-agg q)
      [#f (values #f #f)]
      [a
       (define-values (sn st) (if (cql-agg-starting a) (compile (cql-agg-starting a) env2) (values #f "Any")))
       (define envA (cenv-bind env2 'acc (list (cons (cql-agg-name a) st))))
       (define-values (bn bt) (compile (cql-agg-body a) envA))
       (values (list (symbol->string (cql-agg-name a)) (cql-agg-distinct? a) sn bn)
               (if (equal? st "Any") bt st))]))
  (define sort
    (match (cql-query-sort q)
      [#f #f]
      [(? symbol? dir) (list (elm-sort-by 'direction (symbol->string dir) #f))]
      [items
       (define row-env (cenv (cenv-locals env) ret-t #f))
       (for/list ([it (in-list items)])
         (define dir (symbol->string (cql-sort-item-direction it)))
         (match (cql-sort-item-expr it)
           [(cql-ref n) #:when (property-type ret-t (symbol->string n))
            (elm-sort-by 'column dir (symbol->string n))]
           [e (elm-sort-by 'expression dir (let-values ([(n _t) (sys e row-env)]) n))]))]))
  (define scalar? (and single? (not (list-ctype? (third (car srcs))))))
  (values (elm-query (for/list ([s (in-list srcs)]) (cons (symbol->string (first s)) (second s)))
                     let-elms rels where ret ret-distinct sort agg)
          (cond [agg agg-t] [scalar? ret-t] [else (list-ctype ret-t)])))

;; ---- control flow ----------------------------------------------------------
;; an untyped `null` branch takes the type of the other branches, as an explicit As (the reference
;; translator's form): if x is null then null else Interval[…] -> then: As(Null, Interval<DateTime>)
(define (typed-null n t result-type)
  ;; (a Message over null is as untyped as the null itself)
  (define null-ish? (or (elm-null? n)
                        (and (elm-nary? n) (equal? (elm-nary-op n) "Message") (elm-null? (car (elm-nary-operands n))))))
  (define spec (and (equal? t "Any") null-ish? (ctype->elm-type result-type)))
  (cond [(not spec) n]
        [(elm-named-type? spec) (elm-as n (elm-named-type-name spec) #f)]   ; implicit: asType
        [else (elm-as n spec #f)]))
(define (known t) (and (not (equal? t "Any")) t))

;; unify-branches : (listof (cons elm CType)) -> (values (listof elm) CType)
;; branches of different types meet at a System type when the model ones convert to it
;; (if x is null then E.period else Interval[…] -> then: ToInterval(E.period)); untyped nulls take
;; the result type
(define (unify-branches bs)
  (define sys-t (for/first ([b (in-list bs)] #:when (and (known (cdr b)) (not (model-type? (cdr b))))) (cdr b)))
  (define converted
    (for/list ([b (in-list bs)])
      (if (and sys-t (model-type? (cdr b)))
          (let-values ([(n t) (to-system (car b) (cdr b))]) (if (equal? t sys-t) (cons n t) b))
          b)))
  ;; promotable System types meet at the widest one (an Interval<Date> arm beside Interval<DateTime>)
  (define rt (or (for/fold ([rt #f]) ([b (in-list converted)] #:when (known (cdr b)))
                   (cond [(not rt) (cdr b)] [(promotion rt (cdr b)) (cdr b)] [else rt]))
                 "Any"))
  (values (for/list ([b (in-list converted)])
            (define n (if (and (known (cdr b)) (promotion (cdr b) rt)) (promote (car b) (cdr b) rt) (car b)))
            (typed-null n (cdr b) rt))
          rt))
(define (model-type? t) (and (string? t) (regexp-match? #rx"[.]" t)))

(define (compile-if c t e env)
  (define-values (cn _ct) (sys c env))
  (define-values (tn tt) (compile t env))
  (define-values (en et) (compile e env))
  (define-values (ns rt) (unify-branches (list (cons tn tt) (cons en et))))
  (values (elm-if cn (first ns) (second ns)) rt))

(define (compile-case comparand items els env)
  (define cmp (and comparand (let-values ([(n _t) (compile comparand env)]) n)))
  (define arms (for/list ([it (in-list items)])
                 (define-values (wn _wt) (if comparand (compile (cql-case-item-when it) env) (sys (cql-case-item-when it) env)))
                 (define-values (tn tt) (compile (cql-case-item-then it) env))
                 (list wn tn tt)))
  (define-values (en et) (if els (compile els env) (values #f "Any")))
  (define-values (ns rt) (unify-branches (append (for/list ([a (in-list arms)]) (cons (second a) (third a)))
                                                 (if en (list (cons en et)) '()))))
  (values (elm-case cmp (for/list ([a (in-list arms)] [n (in-list ns)]) (elm-case-item (first a) n))
                    (and en (last ns)))
          rt))

;; ---- collections -----------------------------------------------------------
(define (compile-list elems env)
  (define-values (nodes t)
    (for/fold ([acc '()] [t "Any"] #:result (values (reverse acc) t)) ([e (in-list elems)])
      (define-values (n et) (compile e env))
      (values (cons n acc) et)))
  (values (elm-list nodes) (list-ctype t)))

(define (compile-interval low high lc hc env)
  (define-values (ln0 lt0) (sys low env))
  (define-values (hn0 ht0) (sys high env))
  (define pt (or (known lt0) (known ht0) "Any"))
  (define ln (typed-null ln0 lt0 pt))
  (define hn (typed-null hn0 ht0 pt))
  (define lt pt)
  (values (elm-interval ln hn lc hc) (interval-ctype lt)))

;; a tuple carries its elements' types (`return {id: …, requestStatusReason: GetStatusReason(D)}`
;; is a Tuple{id: String, requestStatusReason: …}): a property on it is typed, and a union of
;; queries returning tuples beside ones returning resources is cast to the choice of both
(define (compile-tuple elems env)
  (define compiled (for/list ([te (in-list elems)])
                     (define-values (vn t) (compile (cql-tuple-elem-value te) env))
                     (list (cql-tuple-elem-name te) vn t)))
  (values (elm-tuple (for/list ([c (in-list compiled)]) (elm-tuple-elem (first c) (second c))))
          (tuple-ctype (for/list ([c (in-list compiled)]) (cons (string->symbol (format "~a" (first c))) (third c))))))

;; ---- navigation / calls / indexing ----------------------------------------
;; X.name : an include alias names a definition there ; a query alias reads with `scope` ; otherwise a
;; Property over the source, typed from the model (or a System structured type)
(define (compile-property target name env)
  (cond
    [(include-alias? env target) (compile-qualified-ref (cql-ref-name target) name)]
    [(and (cql-ref? target) (match (local-ref env (cql-ref-name target)) [(cons 'alias _) #t] [_ #f]))
     (define t (cdr (local-ref env (cql-ref-name target))))
     (if (mapped-element? t (symbol->string name))
         (map-property #f (symbol->string (cql-ref-name target)) t (symbol->string name))
         (values (elm-property #f (symbol->string name) (symbol->string (cql-ref-name target)))
                 (or (property-type t (symbol->string name)) "Any")))]
    [else
     (define-values (tn tt) (compile target env))
     (define pt (or (property-type tt (symbol->string name)) "Any"))
     (cond
       [(mapped-element? tt (symbol->string name)) (map-property tn #f tt (symbol->string name))]
       ;; a property of a LIST of model values flattens, as the reference translator writes it:
       ;; ($this in list where $this.p is not null return all $this.p)
       [(and (list-ctype? tt) (string? (ctype-element tt)) (regexp-match? #rx"[.]" (ctype-element tt)))
        (define p (elm-property (elm-alias-ref "$this") (symbol->string name) #f))
        ;; (an item's mapped element is returned through its mapping: FHIRHelpers.ToValue($this.bounds))
        (define ret (if (mapped-element? (ctype-element tt) (symbol->string name))
                        (let-values ([(n _t) (map-property #f #f (ctype-element tt) (symbol->string name)
                                                           #:value p #:parent (elm-alias-ref "$this"))])
                          n)
                        p))
        (define q (elm-query (list (cons "$this" tn)) '() '()
                             (elm-unary "Not" (elm-unary "IsNull" p "Boolean") "Boolean") ret 'all #f #f))
        (define el (property-type (ctype-element tt) (symbol->string name)))
        (if (list-ctype? el)
            (values (elm-unary "Flatten" q #f) el)
            (values q pt))]
       [else (values (elm-property tn (symbol->string name) #f) pt)])]))

;; property-type : CType string -> CType|#f
(define system-properties
  (hash "Code" (hash "code" "String" "system" "String" "version" "String" "display" "String")
        "Concept" (hash "codes" (list-ctype "Code") "display" "String")
        "Quantity" (hash "value" "Decimal" "unit" "String")
        "Ratio" (hash "numerator" "Quantity" "denominator" "Quantity")))
(define (property-type t name)
  (match t
    [(? string? s)
     (define m (lctx-model (L)))
     (cond [(hash-ref system-properties s #f) => (lambda (h) (hash-ref h name #f))]
           [(and m (regexp-match? #rx"[.]" s)) (model-element-type m s name)]
           [(equal? s "Any") "Any"]
           [else #f])]
    [(? list-ctype?) (let ([p (property-type (ctype-element t) name)]) (and p (if (list-ctype? p) p (list-ctype p))))]
    [(? interval-ctype?) (and (member name '("low" "high")) (ctype-element t))]
    [(? tuple-ctype?) (let ([e (assq (string->symbol name) (tuple-fields t))]) (and e (cdr e)))]
    [(? choice-ctype?) "Any"]
    [_ #f]))

(define (compile-index target idx env)
  (define-values (tn tt) (compile target env))
  (define-values (in _it) (compile idx env))
  (values (elm-nary "Indexer" (list tn in) (simple-rt (elem-type tt))) (elem-type tt)))

(define (elem-type t) (or (and (list-ctype? t) (ctype-element t)) "Any"))

;; system operators reachable as function-call syntax: name -> (arity-kind . result-type)
;; arity-kind: 'unary (1 arg -> elm-unary) | 'nary (n args -> elm-nary)
(define system-ops
  (hash 'ToString    '(unary . "String")   'ToInteger '(unary . "Integer") 'ToLong '(unary . "Long")
        'ToDecimal   '(unary . "Decimal")  'ToBoolean '(unary . "Boolean") 'ToQuantity '(unary . "Quantity")
        'ToDate      '(unary . "Date")     'ToDateTime '(unary . "DateTime") 'ToTime '(unary . "Time")
        'Abs         '(unary . #f)         'Ceiling '(unary . "Integer") 'Floor '(unary . "Integer")
        'Truncate    '(unary . "Integer")  'Ln '(unary . "Decimal") 'Exp '(unary . "Decimal")
        'IsNull      '(unary . "Boolean")  'IsTrue '(unary . "Boolean") 'IsFalse '(unary . "Boolean")
        'Count       '(unary . "Integer")  'Sum '(unary . #f) 'Min '(unary . #f) 'Max '(unary . #f)
        'Avg         '(unary . "Decimal")  'First '(unary . #f) 'Last '(unary . #f)
        'Length      '(unary . "Integer")  'Upper '(unary . "String") 'Lower '(unary . "String")
        'Exists      '(unary . "Boolean")  'Not '(unary . "Boolean") 'Distinct '(unary . #f)
        'Log         '(nary . "Decimal")   'Power '(nary . #f) 'Round '(nary . "Decimal")
        'Coalesce    '(nary . #f)       'Combine '(nary . "String") 'Concatenate '(nary . "String")
        'StartsWith  '(nary . "Boolean")   'EndsWith '(nary . "Boolean") 'Substring '(nary . "String")
        'PositionOf  '(nary . "Integer")   'Indexer '(nary . #f)
        ;; interval operators
        'Start '(unary . #f) 'End '(unary . #f) 'Width '(unary . #f) 'PointFrom '(unary . #f)
        'SingletonFrom '(unary . #f) 'Collapse '(nary . #f) 'Expand '(nary . #f)
        ;; list operators
        'Take '(nary . #f) 'Skip '(nary . #f) 'Tail '(unary . #f) 'Slice '(nary . #f)
        'IndexOf '(nary . "Integer") 'Flatten '(unary . #f)
        ;; aggregate functions
        'AllTrue '(unary . "Boolean") 'AnyTrue '(unary . "Boolean") 'Product '(unary . #f) 'Mode '(unary . #f)
        'Median '(unary . "Decimal") 'StdDev '(unary . "Decimal") 'Variance '(unary . "Decimal")
        'PopulationStdDev '(unary . "Decimal") 'PopulationVariance '(unary . "Decimal") 'GeometricMean '(unary . "Decimal")
        ;; string functions
        'Split '(nary . #f) 'LastPositionOf '(nary . "Integer") 'Matches '(nary . "Boolean")
        'ReplaceMatches '(nary . "String") 'Message '(nary . #f)
        'Predecessor '(unary . #f) 'Successor '(unary . #f)
        'ToDate '(unary . "Date") 'ToDateTime '(unary . "DateTime") 'ToTime '(unary . "Time")
        'Today '(nary . "Date") 'Now '(nary . "DateTime") 'TimeOfDay '(nary . "Time")
        'Precision '(unary . "Integer") 'LowBoundary '(nary . #f) 'HighBoundary '(nary . #f)
        'descendents '(unary . #f) 'ToConcept '(unary . "Concept") 'ToCode '(unary . "Code")))

;; temporal constructors take positional args -> canonical ELM Date/DateTime/Time nodes
(define (compile-constructor name args env)
  (define nts (for/list ([a (in-list args)]) (let-values ([(n t) (compile a env)]) (cons n t))))
  ;; DateTime's timezone offset is a Decimal: an Integer one is promoted (ToDecimal), as the reference does
  (define ns (for/list ([nt (in-list nts)] [i (in-naturals)])
               (if (and (eq? name 'DateTime) (= i 7)) (promote (car nt) (cdr nt) "Decimal") (car nt))))
  (define (arg i) (and (< i (length ns)) (list-ref ns i)))
  (case name
    [(Date) (values (elm-date (arg 0) (arg 1) (arg 2)) "Date")]
    [(Time) (values (elm-time (arg 0) (arg 1) (arg 2) (arg 3)) "Time")]
    [(DateTime) (values (elm-datetime (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6) (arg 7)) "DateTime")]))

;; a bare calendar-unit word used as a per-step (expand … per day) means "1 <unit>"
(define calendar-units '(year years month months week weeks day days hour hours minute minutes second seconds millisecond milliseconds))
(define (normalize-per-arg a)
  (if (and (cql-ref? a) (memq (cql-ref-name a) calendar-units))
      (cql-lit (qty-lit 1 (symbol->string (cql-ref-name a))))
      a))


;; system operators over Any (lists, nulls): their operands are NOT converted to System types
(define generic-ops '(IsNull Exists Count First Last SingletonFrom Flatten Distinct Coalesce Message Indexer
                      Tail Take Skip Slice IndexOf Length Combine))

;; age functions: AgeInYears() / AgeInYearsAt(x) / CalculateAgeInYearsAt(b, x) -> CalculateAge[At]
(define age-rx #px"^(Calculate)?AgeIn(Years|Months|Weeks|Days|Hours|Minutes|Seconds)(At)?$")
;; age-function-names : (listof string)   every name age-rx accepts (for "did you mean")
(define age-function-names
  (for*/list ([calc '("" "Calculate")] [p '("Years" "Months" "Weeks" "Days" "Hours" "Minutes" "Seconds")] [at '("" "At")])
    (string-append calc "AgeIn" p at)))

(define (compile-call target name args env)
  (define lx (L))
  (cond
    ;; temporal constructors (only in call position, no target)
    [(and (not target) (memq name '(Date DateTime Time))) (compile-constructor name args env)]
    ;; expand's per-argument may be a bare unit keyword (normalize, then compile as usual)
    [(and (not target) (memq name '(Expand Collapse)) (= 2 (length args)) (not (equal? args (map normalize-per-arg args))))
     (compile-call target name (map normalize-per-arg args) env)]
    ;; Library.f(args) into an include
    [(include-alias? env target) (call-function (cql-ref-name target) name args env)]
    ;; a local function (or overload set)
    [(and (not target) (resolve-overload (hash-ref (lctx-funcs lx) name '()) (arg-types args env) lx))
     (call-function #f name args env)]
    ;; x.f(args) where f is a fluent function, local or included
    [(and target (fluent-home name (cons target args) env))
     => (lambda (home) (call-function (if (eq? home 'local) #f home) name (cons target args) env))]
    ;; inside `sort by`, f(args) may be a fluent function over the row: f($this, args), as the
    ;; reference translator reads `sort by start of "Interval"() asc`
    [(and (not target) (cenv-row env)
          (let ([env* (cenv-bind env 'row (list (cons '$this (cenv-row env))))] [this (cql-ref '$this)])
            (let ([home (fluent-home name (cons this args) env*)])
              (and home (list home env* this)))))
     => (match-lambda [(list home env* this) (call-function (if (eq? home 'local) #f home) name (cons this args) env*)])]
    [(and (not target) (regexp-match age-rx (symbol->string name))) => (lambda (m) (compile-age m args env))]
    ;; FHIRPath-style methods on a collection: x.first(), x.where(cond), x.single(), …
    [(and target (hash-ref fhirpath-methods name #f)) => (lambda (op) (compile-method op target args env))]
    [else (compile-system-call target name args env)]))

;; FHIRPath-compatible method invocation, as the translator maps it
(define fhirpath-methods
  (hasheq 'first "First" 'last "Last" 'single "SingletonFrom" 'exists "Exists" 'count "Count"
          'distinct "Distinct" 'where 'where 'select 'select 'empty 'empty
          'skip 'skip 'take 'take 'tail 'tail))
;; compile-method : op cql (listof cql) cenv -> (values elm CType)
(define (compile-method op target args env)
  (define-values (tn tt) (compile target env))
  (define el (if (list-ctype? tt) (ctype-element tt) tt))
  (define (this-env) (struct-copy cenv (cenv-bind env 'alias (list (cons '$this el))) [this "$this"]))
  (match op
    ['where
     (define-values (c _ct) (sys (car args) (this-env)))
     (values (elm-query (list (cons "$this" tn)) '() '() c #f #f #f #f) (list-ctype el))]
    ['select
     (define-values (r rt) (compile (car args) (this-env)))
     (values (elm-query (list (cons "$this" tn)) '() '() #f r #f #f #f) (list-ctype rt))]
    ['empty (values (elm-unary "Not" (elm-unary "Exists" tn "Boolean") "Boolean") "Boolean")]
    ;; skip(n) / take(n) / tail() are Slice(source, start, end)
    ['skip (values (elm-nary "Slice" (list tn (compile1 (car args) env) (elm-null #f)) #f) tt)]
    ['take (values (elm-nary "Slice" (list tn (elm-literal "Integer" "0") (compile1 (car args) env)) #f) tt)]
    ['tail (values (elm-nary "Slice" (list tn (elm-literal "Integer" "1") (elm-null #f)) #f) tt)]
    [(or "First" "Last" "SingletonFrom") (values (elm-unary op tn #f) el)]
    ["Count" (values (elm-unary op tn "Integer") "Integer")]
    ["Exists" (values (elm-unary op tn "Boolean") "Boolean")]
    [_ (values (elm-unary op tn #f) tt)]))

;; arg-types : (listof cql) cenv -> (listof CType)
(define (arg-types args env) (for/list ([a (in-list args)]) (let-values ([(_n t) (compile a env)]) t)))

;; fluent-home : sym args cenv -> 'local | alias-sym | #f   where a fluent function `name` lives
(define (fluent-home name args env)
  ;; only type the arguments when some fluent function of that name exists (typing a FHIRPath-style
  ;; .where(use = 'x') argument outside its $this scope would fail)
  (define (named? lx) (ormap cql-func-fluent? (hash-ref (lctx-funcs lx) name '())))
  (and (or (named? (L)) (for/or ([(alias _i) (in-hash (lctx-includes (L)))]) (let ([lx (included alias)]) (and lx (named? lx)))))
       (fluent-home* name args env)))
(define (fluent-home* name args env)
  (define ts (arg-types args env))
  (define (has? lx) (resolve-overload (filter cql-func-fluent? (hash-ref (lctx-funcs lx) name '())) ts lx))
  (cond [(has? (L)) 'local]
        [else (for/or ([(alias _inc) (in-hash (lctx-includes (L)))])
                (define lx (included alias))
                (and lx (has? lx) alias))]))

;; call-function : alias|#f sym args cenv -> (values elm CType)   FunctionRef with overload resolution
;; and argument conversion to the chosen overload's operand types
(define (call-function alias name args env)
  (define lx (if alias (included alias) (L)))
  (define compiled (for/list ([a (in-list args)]) (let-values ([(n t) (compile a env)]) (cons n t))))
  (define cands (if lx (hash-ref (lctx-funcs lx) name '()) '()))
  (define f (and lx (resolve-overload cands (map cdr compiled) lx)))
  (cond
    [f
     (define formals (parameterize ([current-lctx lx])
                       (map (lambda (o) (type->ctype (cql-operand-type o))) (cql-func-operands f))))
     (values (elm-function-ref (symbol->string name) (and alias (symbol->string alias))
                               (for/list ([c (in-list compiled)] [ft (in-list formals)]) (coerce (car c) (cdr c) ft))
                               (parameterize ([current-lctx lx])
                                 (map (lambda (o) (compile-type (cql-operand-type o))) (cql-func-operands f))))
             (func-result-type lx f))]
    [(and alias (not lx))                                  ; unresolved include: untyped reference
     (values (elm-function-ref (symbol->string name) (symbol->string alias) (map car compiled) #f) "Any")]
    ;; a name the library does not define at all: say so, with the nearest of its functions
    [(and lx (null? cands))
     (fail (format "unknown function ~a~a~a" (if alias (format "~a." alias) "") name
                   (did-you-mean (symbol->string name) (map (lambda (k) (format "~a" k)) (hash-keys (lctx-funcs lx))))))]
    [else (fail (format "no overload of ~a~a matches ~s" (if alias (format "~a." alias) "") name (map cdr compiled)))]))

;; ---- overloads and conversions ----------------------------------------------
;; conversion-cost : CType CType -> number|#f   0 exact/subtype, 1 one implicit conversion, 2 two
(define (conversion-cost from to)
  (define m (lctx-model (L)))
  (cond
    [(equal? from to) 0]
    [(or (equal? to "Any") (equal? from "Any")) 0]
    [(and (string? from) (string? to) m (model-subtype? m from to)) 0]
    [(choice-ctype? to)
     ;; a choice argument fits a choice parameter when all its members do (0), or some do (1: the
     ;; translator accepts the overlap)
     (cond [(choice-ctype? from)
            (let ([ks (map (lambda (f) (conversion-cost f to)) (choice-members from))])
              (cond [(andmap values ks) 0] [(ormap values ks) 1] [else #f]))]
           ;; an interval or list argument fits a choice with that very member (QI-Core's period,
           ;; an Interval<DateTime>, into toInterval's Choice<…, Interval<DateTime>, …>)
           [(compound-ctype? from) (and (for/or ([c (in-list (choice-members to))]) (and (compound-ctype? c) (eqv? 0 (conversion-cost from c)))) 0)]
           [(for/or ([c (in-list (choice-members to))]) (eqv? 0 (conversion-cost from c))) 0]
           ;; a System value promotes to a member (a Date into earliest's Choice<DateTime, …>)
           [(promoted-member from to) 1]
           [else #f])]
    ;; a choice argument to a parameter of one of its member types: an implicit As (cost 1); to a
    ;; type a member CONVERTS to: As then the conversion (cost 2) — ToConcept(m as CodeableConcept)
    [(and (choice-ctype? from) (string? to) (for/or ([c (in-list (choice-members from))]) (eqv? 0 (conversion-cost c to))))
     1]
    [(and (choice-ctype? from) (string? to) (for/or ([c (in-list (choice-members from))]) (eqv? 1 (conversion-cost c to))))
     2]
    [(or (and (list-ctype? from) (list-ctype? to)) (and (interval-ctype? from) (interval-ctype? to)))
     (conversion-cost (ctype-element from) (ctype-element to))]
    [(and (string? from) (string? to) (hash-ref promotions (cons from to) #f)) 1]
    [(and (string? from) m (model-conversion m from))
     => (lambda (c) (let ([k (conversion-cost (car c) to)]) (and k (< k 2) (+ 1 k))))]
    [(and (equal? to "Tuple") (tuple-ctype? from)) 0]
    [else #f]))

;; exact-overlap : CType(choice) CType(choice) -> (listof CType)|#f   the members of `from` that are
;; members of `to`, in order, when every other member fits not at all and they are System types
(define (exact-overlap from to)
  (define ks (for/list ([c (in-list (choice-members from))]) (cons c (conversion-cost c to))))
  (define (system? t) (if (string? t) (not (regexp-match? #rx"[.]" t)) (and (ctype-element t) (system? (ctype-element t)))))
  (and (andmap (lambda (k) (memv (cdr k) '(0 #f))) ks)
       (let ([ms (for/list ([k (in-list ks)] #:when (eqv? (cdr k) 0)) (car k))])
         (and (pair? ms) (andmap system? ms) ms))))

;; promoted-member : CType CType -> CType|#f   the member of choice `to` a System `from` promotes to
(define (promoted-member from to)
  (and (string? from)
       (for/first ([c (in-list (choice-members to))] #:when (and (string? c) (hash-ref promotions (cons from c) #f))) c)))

;; coerce : elm CType CType -> elm   apply the implicit conversions conversion-cost counted
(define (coerce n from to)
  (define m (lctx-model (L)))
  (cond
    ;; a value passed to a choice-typed parameter is cast to the whole choice
    ;; a choice argument only partly inside a choice parameter is cast to the parameter's choice
    ;; …when its members either ARE parameter members or do not fit at all (System types under a
    ;; mapping: FHIRHelpers.ToValue's Choice<DateTime, Interval<DateTime>, Timing, DateTime> into
    ;; Choice<DateTime, Quantity, …>), a Case over the members that are: when x is M then x as M as
    ;; the parameter's choice, else null — the reference translator's form
    [(and (choice-ctype? to) (choice-ctype? from) (eqv? (conversion-cost from to) 1)
          (exact-overlap from to))
     => (lambda (ms)
          (define (spec m) (if (string? m) (ctype->elm-name m) (ctype->elm-type m)))
          (define whole (ctype->elm-type to))
          (elm-case #f (for/list ([m (in-list ms)])
                         (elm-case-item (elm-is n (spec m)) (elm-as (elm-as n (spec m) #f) whole #f)))
                    (elm-null #f)))]
    [(and (choice-ctype? to) (choice-ctype? from) (eqv? (conversion-cost from to) 1))
     (let ([spec (ctype->elm-type to)]) (if spec (elm-as n spec #f) n))]
    ;; a value promoted to a member: ToDateTime(As(x, Date)), as the reference writes it
    [(and (choice-ctype? to) (string? from) (eqv? (conversion-cost from to) 1) (promoted-member from to))
     => (lambda (member) (promote (elm-as n (ctype->elm-name from) #f) from member))]
    ;; (a member type is widened; a choice argument already fits as it is)
    [(and (choice-ctype? to) (or (string? from) (and (compound-ctype? from) (not (choice-ctype? from)))) (not (equal? from "Any")) (conversion-cost from to))
     (let ([spec (ctype->elm-type to)]) (if spec (elm-as n spec #f) n))]
    [(eqv? (conversion-cost from to) 0) n]
    [(promotion from to) (promote n from to)]
    [(and (choice-ctype? from) (string? to) (for/or ([c (in-list (choice-members from))]) (eqv? 0 (conversion-cost c to))))
     (elm-as n (ctype->elm-name to) #f)]
    [(and (choice-ctype? from) (string? to))
     (let-values ([(cn _ct) (choice-to n from to)]) cn)]
    [(and (string? from) m (model-conversion m from))
     (define-values (n2 t2) (to-system n from))
     (if (eq? n2 n) n (coerce n2 t2 to))]
    [else n]))

;; resolve-overload : (listof cql-func) (listof CType) lctx -> cql-func|#f   cheapest applicable
(define (resolve-overload cands ts lx)
  (define scored
    (parameterize ([current-lctx lx])
      (for*/list ([f (in-list cands)]
                  #:when (= (length (cql-func-operands f)) (length ts))
                  [cost (in-value (for/fold ([acc 0]) ([o (in-list (cql-func-operands f))] [t (in-list ts)])
                                    (define k (and acc (conversion-cost t (type->ctype (cql-operand-type o)))))
                                    (and k (+ acc k))))]
                  #:when cost)
        (cons cost f))))
  (and (pair? scored) (cdr (argmin car scored))))

;; AgeIn<P>[At](x) / CalculateAgeIn<P>At(b, x) -> CalculateAge[At] over the model's birth date
(define (compile-age m args env)
  (define prec (let ([p (caddr m)]) (substring p 0 (sub1 (string-length p)))))
  (define at? (and (cadddr m) #t))
  (define calc? (and (cadr m) #t))
  (define-values (operands types)
    (for/lists (ns ts) ([a (in-list (if calc? args (if at? args '())))])
      (sys a env)))
  (define birth
    (and (not calc?)
         (let* ([lx (L)] [model (or (lctx-model lx) (fail "age functions need a data model"))])
           (if (mapped-element? (model-patient-class model) (model-birth-date-path model))
               (let-values ([(n _t) (map-property (elm-expression-ref "Patient" #f) #f (model-patient-class model) (model-birth-date-path model))])
                 (cons n "Date"))
               (cons (elm-property (elm-expression-ref "Patient" #f) (model-birth-date-path model) #f) "Date")))))
  (define all (if birth (cons (car birth) operands) operands))
  (define all-t (if birth (cons (cdr birth) types) types))
  ;; with a DateTime "as of", the birth date is promoted to DateTime
  (define target (if (and (pair? all-t) (member "DateTime" all-t)) "DateTime" #f))
  (define converted (for/list ([n (in-list all)] [t (in-list all-t)]) (if target (promote n t target) n)))
  (values (elm-precision-op (if (or at? calc?) "CalculateAgeAt" "CalculateAge") prec converted) "Integer"))

;; system functions (the operator registry's names), operands converted to System types
(define (compile-system-call target name args env)
  ;; method-style x.f(args) is sugar for f(x, args)
  (define all-args (if target (cons target args) args))
  ;; FHIRPath-style lowercase names reach the System function: x.toDateTime() = ToDateTime(x)
  (define spec (or (hash-ref system-ops name #f) (hash-ref system-ops (capitalize name) #f)))
  (set! name (if (hash-ref system-ops name #f) name (capitalize name)))
  (cond
    [(not spec) (fail (format "unknown function '~a'~a" name
                              (did-you-mean (format "~a" name)
                                            (map (lambda (k) (format "~a" k))
                                                 (append (hash-keys system-ops) (hash-keys (lctx-funcs (L))) age-function-names)))))]
    [else
     (define-values (nodes0 types0)
       (for/lists (ns ts) ([a (in-list all-args)])
         (cond [(memq name generic-ops) (compile a env)]
               ;; Boolean tests over a choice pick the member converting to Boolean
               [(memq name '(IsTrue IsFalse))
                (let-values ([(n t) (compile a env)])
                  (if (choice-ctype? t) (choice-to n t "Boolean") (to-system n t)))]
               [else (sys a env)])))
     ;; Coalesce's arguments meet at one type, like if/case branches: a FHIR value among System ones
     ;; is converted (Coalesce(start of P, R.authoredOn) -> …, ToDateTime(R.authoredOn))
     (define-values (nodes types)
       (if (eq? name 'Coalesce)
           (let ([sys-t (for/first ([t (in-list types0)] #:when (and (known t) (not (model-type? t)))) t)])
             (for/lists (ns ts) ([n (in-list nodes0)] [t (in-list types0)])
               (if (and sys-t (model-type? t))
                   (let-values ([(cn ct) (to-system n t)]) (if (equal? ct sys-t) (values cn ct) (values n t)))
                   (values n t))))
           (values nodes0 types0)))
     (define rt (cdr spec))
     (define t (or rt (derived-result-type name types)))
     (case (car spec)
       [(unary)
        (unless (= 1 (length nodes)) (fail (format "~a expects 1 argument" name)))
        (values (elm-unary (symbol->string name) (car nodes) rt) t)]
       [(nary)
        (values (elm-nary (symbol->string name) nodes rt) t)])]))

(define (capitalize sym)
  (define s (symbol->string sym))
  (if (string=? s "") sym (string->symbol (string-append (string-upcase (substring s 0 1)) (substring s 1)))))

;; derived-result-type : sym (listof CType) -> CType   for system operators typed by their arguments
(define (derived-result-type name ts)
  (define t0 (if (pair? ts) (car ts) "Any"))
  (define (el t) (or (ctype-element t) "Any"))
  (case name
    [(Start End PointFrom Width Min Max Sum First Last SingletonFrom Median Mode Product Indexer) (el t0)]
    [(Abs Predecessor Successor Distinct Tail Take Skip Slice Expand Collapse LowBoundary HighBoundary Message) t0]
    [(Flatten) (let ([e (ctype-element t0)]) (if (list-ctype? e) e t0))]
    [(Coalesce) (or (for/first ([t (in-list ts)] #:unless (equal? t "Any")) t) "Any")]
    [else "Any"]))

;; ---- type ops --------------------------------------------------------------
(define (compile-typeop op operand type env)
  (define-values (o _t) (compile operand env))
  (case op
    [(is) (values (elm-is o (compile-type type)) "Boolean")]
    [(as) (values (elm-as o (compile-type type) #f) (type->ctype type))]
    [else (fail (format "unsupported type operator ~a" op))]))

(define (compile-cast operand type env)
  (define-values (o _t) (compile operand env))
  (values (elm-as o (compile-type type) #t) (type->ctype type)))

(define (compile-convert operand type env)
  (define-values (o _t) (sys operand env))
  (define simple (type-simple type))
  ;; convert to a System type -> a To<Type> operator when known, else a strict As
  (if (and simple (member simple '("String" "Integer" "Long" "Decimal" "Quantity" "Boolean" "Date" "DateTime" "Time")))
      (values (elm-to o simple) simple)
      (values (elm-as o (compile-type type) #t) (type->ctype type))))

;; a type-spec's simple System name (for result-type inference), or #f for composite/qualified
(define (type-simple type)
  (match type
    [(named-type name) (let ([s (symbol->string name)]) (if (regexp-match? #rx"[.]" s) #f s))]
    [_ #f]))

;; ---- types -----------------------------------------------------------------
;; named->ctype : string -> CType   "System.Code" -> "Code" ; "Encounter"/"FHIR.Encounter" -> "FHIR.Encounter"
(define system-simple-types
  '("Integer" "Long" "Decimal" "Quantity" "Ratio" "String" "Boolean" "Date" "DateTime" "Time"
    "Code" "Concept" "Any" "Vocabulary" "ValueSet" "CodeSystem"))
(define (named->ctype s)
  (define m (lctx-model (L)))
  ;; an unqualified name means the model's type when the model has one (Quantity, Ratio, Period in a
  ;; FHIR library), else the System type
  (cond [(regexp-match #rx"^System[.](.*)$" s) => cadr]
        [(and m (model-resolve-type m s))]
        [else s]))
;; type->ctype : type-spec -> CType
(define (type->ctype t)
  (match t
    [(named-type name) (named->ctype (symbol->string name))]
    [(interval-type el) (interval-ctype (type->ctype el))]
    [(list-type el) (list-ctype (type->ctype el))]
    [(choice-type ts) (choice-ctype (map type->ctype ts))]
    [(tuple-type els) (tuple-ctype (map (lambda (e) (cons (car e) (type->ctype (cdr e)))) els))]
    [_ "Any"]))
;; ctype->elm-name : CType(string) -> "{urn:hl7-org:elm-types:r1}Integer" | "{http://hl7.org/fhir}Period"
(define (ctype->elm-name s)
  (define m (lctx-model (L)))
  (cond [(and (mapping-model? m) (regexp-match? (pregexp (string-append "^" (regexp-quote (model-name m)) "[.]")) s))
         (format "{~a}~a" (model-target-url m) (physical-name m s))]
        [(and (mapping-model? m) (regexp-match? (pregexp (string-append "^" (regexp-quote (model-name (target-model m))) "[.]")) s))
         (format "{~a}~a" (model-target-url m) (strip-ns s))]
        [(and m (regexp-match? (pregexp (string-append "^" (regexp-quote (model-name m)) "[.]")) s))
         (format "{~a}~a" (model-url m) (local-name s))]
        [(regexp-match? #rx"[.{]" s) s]
        [else (system-type-name s)]))

;; ctype->elm-type : CType -> elm type-spec | #f (Any)
(define (ctype->elm-type t)
  (match t
    ["Any" #f]
    [(? string? s) (elm-named-type (ctype->elm-name s))]
    [(? list-ctype?) (let ([e (ctype->elm-type (ctype-element t))]) (and e (elm-list-type e)))]
    [(? interval-ctype?) (let ([e (ctype->elm-type (ctype-element t))]) (and e (elm-interval-type e)))]
    [(? choice-ctype?) (let ([es (map ctype->elm-type (choice-members t))]) (and (andmap values es) (elm-choice-type es)))]
    [(? tuple-ctype?) (let ([es (map (lambda (e) (cons (symbol->string (car e)) (ctype->elm-type (cdr e)))) (tuple-fields t))])
                           (and (andmap cdr es) (elm-tuple-type es)))]
    [_ #f]))

(define (compile-type type)
  (match type
    [(named-type name) (elm-named-type (ctype->elm-name (named->ctype (symbol->string name))))]
    [(interval-type el) (elm-interval-type (compile-type el))]
    [(list-type el) (elm-list-type (compile-type el))]
    [(choice-type ts) (elm-choice-type (map compile-type ts))]
    [(tuple-type els) (elm-tuple-type (map (lambda (e) (cons (symbol->string (car e)) (compile-type (cdr e)))) els))]
    [_ (fail (format "unsupported type specifier: ~s" type))]))

(module+ test
  (require rackunit
           (only-in "parser/parse.rkt" parse-cql-expr parse-cql)
           (only-in fhir/result/result ok? ok-value err?))
  (define (C s) (ok-value (compile-expr (ok-value (parse-cql-expr s)))))
  ;; + on integers -> Add ; + on strings -> Concatenate
  (check-equal? (elm-nary-op (C "1 + 2")) "Add")
  (check-equal? (elm-nary-result-type (C "1 + 2")) "Integer")
  (check-equal? (elm-nary-result-type (C "1 + 2.0")) "Decimal")
  (check-equal? (elm-nary-op (C "'a' + 'b'")) "Concatenate")
  (check-equal? (elm-nary-op (C "'a' & 'b'")) "Concatenate")
  ;; comparison / equality
  (check-equal? (elm-nary-op (C "1 < 2")) "Less")
  (check-equal? (elm-nary-op (C "1 = 2")) "Equal")
  ;; != wraps Equal in Not
  (check-equal? (elm-unary-op (C "1 != 2")) "Not")
  (check-equal? (elm-nary-op (elm-unary-operand (C "1 != 2"))) "Equal")
  ;; logical
  (check-equal? (elm-nary-op (C "true and false")) "And")
  (check-equal? (elm-unary-op (C "not true")) "Not")
  ;; unary minus -> Negate
  (check-equal? (C "-3") (elm-literal "Integer" "-3"))   ; a negated integer literal folds to one literal
  (check-equal? (elm-unary-op (C "-(3 + 1)")) "Negate")  ; but Negate on a non-literal stays Negate
  ;; membership / set ops
  (check-equal? (elm-nary-op (C "5 in Interval[1, 10]")) "In")
  (check-equal? (elm-nary-op (C "{1} union {2}")) "Union")
  ;; list / interval / tuple
  (check-true (elm-list? (C "{1, 2, 3}")))
  (check-true (elm-interval? (C "Interval[1, 10]")))
  (check-true (elm-tuple? (C "Tuple{a: 1}")))
  ;; if / case
  (check-true (elm-if? (C "if true then 1 else 2")))
  (check-true (elm-case? (C "case when true then 1 else 2 end")))
  ;; type ops
  (check-true (elm-is? (C "5 is Integer")))
  (check-equal? (elm-named-type-name (elm-is-type (C "5 is Integer"))) "{urn:hl7-org:elm-types:r1}Integer")
  (check-true (elm-to? (C "convert 5 to String")))
  (check-equal? (elm-to-to-type (C "convert 5 to String")) "String")
  ;; system function
  (check-equal? (elm-unary-op (C "ToString(5)")) "ToString")
  (check-equal? (elm-nary-op (C "Coalesce(1, 2)")) "Coalesce")
  ;; unknown identifier / unknown function surface as (err …)
  (check-true (err? (compile-expr (ok-value (parse-cql-expr "unknownRef")))))
  (check-true (err? (compile-expr (ok-value (parse-cql-expr "Bogus(1)")))))
  ;; a whole library compiles, with a default System using + forward reference resolution
  (define lib (ok-value (compile-library (ok-value (parse-cql "define A: 1 + 1\ndefine B: A * 2")))))
  (check-equal? (length (elm-library-statements lib)) 2)
  (check-true (elm-expression-ref? (car (elm-nary-operands (elm-def-expression (cadr (elm-library-statements lib)))))))
  ;; every translation error, each at its definition; a reference to a failed one does not cascade
  (let-values ([(elm errs) (compile-library/errors
                            (ok-value (parse-cql "library T\ndefine A: 1\ndefine X: A + Foo\ndefine Y: X + 1\ndefine Z: Baz\n")))])
    (check-false elm)
    (check-equal? (length errs) 2)
    (check-equal? (map (lambda (e) (cql-error-message e)) errs)
                  '("in define \"X\": unknown identifier 'Foo'" "in define \"Z\": unknown identifier 'Baz'"))))
