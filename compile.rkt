#lang racket/base

;;; compile.rkt — CQL AST -> ELM AST (the translator).
;;;
;;; Translation needs TYPES: CQL surface operators are overloaded (`+` is Add on numbers but
;;; Concatenate on strings; `in` is In, InValueSet or AnyInValueSet), function calls resolve among
;;; overloads (FHIRHelpers has one ToString per FHIR code type), and a model type meeting a System
;;; operator is converted implicitly (FHIR.EncounterStatus -> FHIRHelpers.ToString(…)). So `compile`
;;; returns (values elm-node CType) — CType is defined in model/modelinfo.rkt: a System simple name
;;; ("Integer"), a model name ("FHIR.Encounter"), "Any", or a list/interval/choice/tuple vector.
;;;
;;; A library compiles in a library context (`lctx`, below): every name it declares, its data model
;;; (from `using`, see model/modelinfo.rkt), its includes (compiled lazily through
;;; `current-library-resolver`), and memo tables — definitions compile ON DEMAND when first referenced
;;; (their type is needed at the reference), so forward references and included libraries both work.
;;; Errors raise a cql-error caught at the API boundary and returned as (err …).

(require racket/match racket/list racket/string
         "parser/ast.rkt"
         "elm/ast.rkt"
         "model/modelinfo.rkt"
         (only-in "error.rkt" cql-error cql-error? cql-error-diagnostic cql-error-phase cql-error-message)
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
;; operand), 'alias (query source), 'let (query let), 'acc (aggregate accumulator). `row` is the
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
  (with-handlers ([cql-error? (lambda (e) (err e))])
    (define lx (make-lctx lib))
    (parameterize ([current-lctx lx] [compile-memo (make-hasheq)])
      (ok (assemble-library lib lx)))))

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
                           (if m (model-url m) (symbol->string (cql-using-model d)))
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
  (define valuesets
    (of cql-valueset? (lambda (d) (elm-valueset-def (symbol->string (cql-valueset-name d)) (cql-valueset-id d)
                                                    (cql-valueset-version d) (cql-valueset-access d)
                                                    (map (term-ref-of "CodeSystemRef") (cql-valueset-codesystems d))))))
  (define codes
    (of cql-code-def? (lambda (d) (elm-code-def (symbol->string (cql-code-def-name d)) (cql-code-def-id d)
                                                (cql-code-def-display d) (cql-code-def-access d)
                                                ((term-ref-of "CodeSystemRef") (cql-code-def-system d))))))
  (define concepts
    (of cql-concept-def? (lambda (d) (elm-concept-def (symbol->string (cql-concept-def-name d)) (cql-concept-def-display d)
                                                      (cql-concept-def-access d)
                                                      (map (term-ref-of "CodeRef") (cql-concept-def-codes d))))))
  (define contexts (of cql-context? (lambda (d) (elm-context-def (symbol->string (cql-context-name d))))))
  (define statements
    (append*
     (for/list ([d (in-list defs)])
       (match d
         [(cql-context name) (if (eq? (hash-ref (lctx-defs lx) name #f) 'context)
                                 (list (elm-def (symbol->string name) (symbol->string name) #f (car (def-info lx name))))
                                 '())]
         [(? cql-def?)
          (list (elm-def (symbol->string (cql-def-name d)) (hash-ref (lctx-contexts lx) (cql-def-name d) "Unfiltered")
                         (cql-def-access d) (car (def-info lx (cql-def-name d)))))]
         [(? cql-func?) (list (compile-function-def d lx))]
         [_ '()]))))
  (elm-library (and (cql-library-id lib) (symbol->string (cql-library-id lib))) (cql-library-version lib)
               usings includes params codesystems valuesets codes concepts contexts statements))

;; term-ref-of : kind -> (term-ref -> elm-term-ref)   a local name or Library.name
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
           [d (in-definition d (format "define \"~a\"" name)
                             (lambda () (let-values ([(n t) (compile (cql-def-body d) empty-cenv)]) (cons n t))))])))
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
    (thunk)))

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
    [(cql-index target idx) (compile-index target idx env)]
    [(cql-component prec operand) (compile-component prec operand env)]
    [(cql-between kind prec a b) (compile-between kind prec a b env)]
    [(cql-timing op prec a b) (compile-timing op prec a b env)]
    [(cql-timing-offset op prec q qual a b) (compile-timing-offset op prec q qual a b env)]
    [(cql-within properly? q a b) (compile-within properly? q a b env)]
    [(? cql-query?) (compile-query ast env)]
    [(? cql-retrieve?) (compile-retrieve ast env)]
    [(cql-code-selector code system display)
     (values (elm-code code ((term-ref-of "CodeSystemRef") system) display) "Code")]
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
    [(and (list-type? t) (string? (vector-ref t 1)) (convertible? (vector-ref t 1)))
     (define-values (cn ct) (to-system* (elm-alias-ref "X") (vector-ref t 1)))
     (values (elm-query (list (cons "X" n)) '() '() #f cn 'all #f #f) (vector 'list ct))]
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
    [(and (vector? t) (eq? (vector-ref t 0) 'choice))
     (define member (for*/first ([target (in-list '("String" "Code" "Concept"))]
                                 [c (in-list (vector-ref t 1))]
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
    [(and (vector? t) (eq? (vector-ref t 0) 'choice))
     (define-values (_on ot) (sys other env))
     (define target (cond [(and (string? ot) (not (equal? ot "Any"))) ot]
                          [(interval-type? ot) ot]
                          [else #f]))
     (if target (choice-to n t target) (values n t))]
    [else (to-system n t)]))

;; choice-to : elm CType(choice) CType -> (values elm CType)   the choice members that convert to
;; `target`: none -> unchanged ; one -> Convert(x as M) ; several -> a Case over `x is M`
(define (choice-to n t target)
  (define members (for/list ([c (in-list (vector-ref t 1))]
                             #:when (let ([k (conversion-cost c target)]) (and k (<= k 1))))
                    c))
  (define (conv m) (let-values ([(cn _ct) (to-system (elm-as n (ctype->elm-name m) #f) m)]) cn))
  (match members
    ['() (values n t)]
    [(list m) (to-system (elm-as n (ctype->elm-name m) #f) m)]
    [ms (values (elm-case #f (for/list ([m (in-list ms)]) (elm-case-item (elm-is n (ctype->elm-name m)) (conv m)))
                          (elm-null #f))
                target)]))

;; System implicit promotions, the translator's order: Integer -> Long -> Decimal -> Quantity,
;; Date -> DateTime, Code -> Concept. promote : elm CType CType -> elm (no-op when not applicable)
(define promotions
  (hash (cons "Integer" "Decimal") "ToDecimal" (cons "Integer" "Long") "ToLong" (cons "Long" "Decimal") "ToDecimal"
        (cons "Integer" "Quantity") "ToQuantity" (cons "Decimal" "Quantity") "ToQuantity"
        (cons "Date" "DateTime") "ToDateTime" (cons "Code" "Concept") "ToConcept"))
(define (promote n from to)
  (define op (hash-ref promotions (cons from to) #f))
  (if op (elm-unary op n to) n))
;; unify-pair : elm CType elm CType -> (values elm elm CType)   promote the narrower operand
(define (unify-pair a at b bt)
  (cond [(hash-ref promotions (cons at bt) #f) (values (promote a at bt) b bt)]
        [(hash-ref promotions (cons bt at) #f) (values a (promote b bt at) at)]
        [else (values a b at)]))
(define (point-of t) (if (and (vector? t) (memq (vector-ref t 0) '(interval list))) (vector-ref t 1) #f))
(define (interval-type? t) (and (vector? t) (eq? (vector-ref t 0) 'interval)))
(define (list-type? t) (and (vector? t) (eq? (vector-ref t 0) 'list)))

;; ---- references ------------------------------------------------------------
;; compile-ref : sym cenv -> (values elm CType)   local scope, then the library, then the sort row
(define (compile-ref name env)
  (define s (symbol->string name))
  (match (local-ref env name)
    [(cons 'alias t) (values (elm-alias-ref s) t)]
    [(cons 'let t) (values (elm-query-let-ref s) t)]
    [(cons _ t) (values (elm-operand-ref s) t)]
    [#f
     (define lx (L))
     (cond
       [(hash-ref (lctx-defs lx) name #f) (values (elm-expression-ref s #f) (cdr (def-info lx name)))]
       [(hash-ref (lctx-params lx) name #f) => (lambda (p) (values (elm-parameter-ref s #f) (param-type p)))]
       [(hash-ref (lctx-terms lx) name #f) => (lambda (k) (values (elm-term-ref k s #f) (term-type k)))]
       [(and (cenv-row env) (property-type (cenv-row env) s))
        => (lambda (t) (values (elm-identifier-ref s) t))]
       [(and (cenv-this env) (property-type (cdr (local-ref env (string->symbol (cenv-this env)))) s))
        => (lambda (t) (values (elm-property #f s (cenv-this env)) t))]
       [else (fail (format "unknown identifier '~a'" name))])]))

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
    [else (fail (format "unknown identifier '~a.~a'" alias name))]))

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
     (define kind (string-append (if (list-type? lt) "Any" "") (if (equal? rt "ValueSet") "InValueSet" "InCodeSystem")))
     (values (elm-in-valueset kind lo ro) "Boolean")]   ; (json: code vs codes by kind)
    [(eq? op 'in)
     (define el (point-of rt))
     (values (elm-nary "In" (list (if el (promote lo lt el) lo) ro) "Boolean") "Boolean")]
    [else
     (define el (point-of lt))
     (values (elm-nary "Contains" (list lo (if el (promote ro rt el) ro)) "Boolean") "Boolean")]))

;; numeric promotion Integer < Long < Decimal < Quantity ; #f/Any when not numeric
(define numeric-rank (hash "Integer" 1 "Long" 2 "Decimal" 3 "Quantity" 4))
(define (numeric-or a b)
  (define ra (hash-ref numeric-rank a #f))
  (define rb (hash-ref numeric-rank b #f))
  (cond [(and ra rb) (if (>= ra rb) a b)]
        [ra a] [rb b] [else "Any"]))
(define (composite-or a b) (cond [(vector? a) a] [(vector? b) b] [else #f]))

;; union-of-different-types : cql cql cenv -> (list elm CType) | #f
;; List<A> union List<B> of unrelated model types: both cast to List<Choice<A, B>>, as the reference does
(define (union-of-different-types l r env)
  (define-values (lo lt) (compile l env))
  (define-values (ro rt) (compile r env))
  (define m (lctx-model (L)))
  (define (el t) (and (list-type? t) (vector-ref t 1)))
  (define a (el lt)) (define b (el rt))
  (define (members t) (if (and (vector? t) (eq? (vector-ref t 0) 'choice)) (vector-ref t 1) (list t)))
  (cond
    [(and m a b (not (equal? a b)) (or (model-type? a) (vector? a)) (or (model-type? b) (vector? b))
          (not (and (string? a) (string? b) (or (model-subtype? m a b) (model-subtype? m b a)))))
     (define ct (vector 'list (vector 'choice (remove-duplicates (append (members a) (members b))))))
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
  (define-values (an bn _t)
    (if (or (vector? at0) (vector? bt0)) (values an0 bn0 at0) (unify-pair an0 at0 bn0 bt0)))
  (define a-point? (not (interval-type? at0)))
  (define b-point? (not (interval-type? bt0)))
  (define op*
    (case op
      [(included-in during) (if (and a-point? (interval-type? bt0)) 'in op)]
      [(properly-included-in) (if (and a-point? (interval-type? bt0)) 'proper-in op)]
      [(includes) (if (and b-point? (interval-type? at0)) 'contains op)]
      [(properly-includes) (if (and b-point? (interval-type? at0)) 'proper-contains op)]
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
  (define an (if (interval-type? at) (elm-unary (if before? "End" "Start") an0 #f) an0))
  (define bn (if (interval-type? bt) (elm-unary (if before? "Start" "End") bn0 #f) bn0))
  (define-values (qn _qt) (compile-literal q))
  (define shifted (elm-nary (if before? "Subtract" "Add") (list bn qn) #f))
  (define p (and prec (units->prec prec)))
  (case qual
    [(or-less less-than)
     (define edge-closed (and (eq? qual 'or-less) #t))
     (define iv (if before?
                    (elm-interval shifted bn edge-closed (and on? #t))
                    (elm-interval bn shifted (and on? #t) edge-closed)))
     (values (elm-nary "And" (list (elm-timing 'in p an iv)
                                   (elm-unary "Not" (elm-unary "IsNull" bn "Boolean") "Boolean"))
                       "Boolean")
             "Boolean")]
    [(or-more more-than)
     (define t (if before? (if (eq? qual 'or-more) 'same-or-before 'before) (if (eq? qual 'or-more) 'same-or-after 'after)))
     (values (elm-timing t p an shifted) "Boolean")]
    [else (values (elm-timing 'same-as p an shifted) "Boolean")]))

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
  (values (retrieve-node qname
                         (and term (or (and path (symbol->string path))
                                       (class-info-primary-code-path (model-class m qname))))
                         (and term (cond [cmp (symbol->string cmp)]
                                         [(member ct '("ValueSet" "CodeSystem")) "in"]
                                         [else "~"]))
                         ;; a single (non-list, non-ValueSet) terminology becomes a list
                         (and term (if (or (list-type? ct) (member ct '("ValueSet" "CodeSystem")))
                                       codes
                                       (elm-unary "ToList" codes #f))))
          (vector 'list qname)))
(define (retrieve-node qname code-property comparator codes)
  (define m (lctx-model (L)))
  (elm-retrieve (format "{~a}~a" (model-url m) (local-name qname))
                (class-info-identifier (model-class m qname)) code-property comparator codes))
(define (local-name q) (let ([m (regexp-match #rx"[.]([^.]+)$" q)]) (if m (cadr m) q)))

;; ---- queries ---------------------------------------------------------------
;; sources/lets bind aliases with their element types; relationships, where, return and aggregate
;; compile in that scope; `sort by` items compile against the result row (IdentifierRef / ByColumn).
(define (compile-query q env)
  (define srcs (for/list ([s (in-list (cql-query-sources q))])
                 (define-values (n t) (compile (cql-source-expr s) env))
                 (list (cql-source-alias s) n t)))
  (define single? (= 1 (length srcs)))
  (define (elem t) (if (list-type? t) (vector-ref t 1) t))
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
        (values #f (if single? (elem (third (car srcs))) "Tuple"))))
  (define ret-distinct (and (cql-query-ret q) (cql-return-distinct? (cql-query-ret q))))
  (define agg
    (and (cql-query-agg q)
         (let* ([a (cql-query-agg q)] [envA (cenv-bind env2 'acc (list (cons (cql-agg-name a) "Any")))])
           (list (symbol->string (cql-agg-name a)) (cql-agg-distinct? a)
                 (and (cql-agg-starting a) (compile1 (cql-agg-starting a) env2))
                 (compile1 (cql-agg-body a) envA)))))
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
  (define scalar? (and single? (not (list-type? (third (car srcs))))))
  (values (elm-query (for/list ([s (in-list srcs)]) (cons (symbol->string (first s)) (second s)))
                     let-elms rels where ret ret-distinct sort agg)
          (cond [agg "Any"] [scalar? ret-t] [else (vector 'list ret-t)])))

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
  (define rt (or (for/first ([b (in-list converted)] #:when (known (cdr b))) (cdr b)) "Any"))
  (values (for/list ([b (in-list converted)]) (typed-null (car b) (cdr b) rt)) rt))
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
  (values (elm-list nodes) (vector 'list t)))

(define (compile-interval low high lc hc env)
  (define-values (ln0 lt0) (sys low env))
  (define-values (hn0 ht0) (sys high env))
  (define pt (or (known lt0) (known ht0) "Any"))
  (define ln (typed-null ln0 lt0 pt))
  (define hn (typed-null hn0 ht0 pt))
  (define lt pt)
  (values (elm-interval ln hn lc hc) (vector 'interval lt)))

(define (compile-tuple elems env)
  (values (elm-tuple (for/list ([te (in-list elems)])
                       (define-values (vn _t) (compile (cql-tuple-elem-value te) env))
                       (elm-tuple-elem (cql-tuple-elem-name te) vn)))
          "Tuple"))

;; ---- navigation / calls / indexing ----------------------------------------
;; X.name : an include alias names a definition there ; a query alias reads with `scope` ; otherwise a
;; Property over the source, typed from the model (or a System structured type)
(define (compile-property target name env)
  (cond
    [(include-alias? env target) (compile-qualified-ref (cql-ref-name target) name)]
    [(and (cql-ref? target) (match (local-ref env (cql-ref-name target)) [(cons 'alias _) #t] [_ #f]))
     (define t (cdr (local-ref env (cql-ref-name target))))
     (values (elm-property #f (symbol->string name) (symbol->string (cql-ref-name target)))
             (or (property-type t (symbol->string name)) "Any"))]
    [else
     (define-values (tn tt) (compile target env))
     (define pt (or (property-type tt (symbol->string name)) "Any"))
     (cond
       ;; a property of a LIST of model values flattens, as the reference translator writes it:
       ;; ($this in list where $this.p is not null return all $this.p)
       [(and (list-type? tt) (string? (vector-ref tt 1)) (regexp-match? #rx"[.]" (vector-ref tt 1)))
        (define p (elm-property (elm-alias-ref "$this") (symbol->string name) #f))
        (define q (elm-query (list (cons "$this" tn)) '() '()
                             (elm-unary "Not" (elm-unary "IsNull" p "Boolean") "Boolean") p 'all #f #f))
        (define el (property-type (vector-ref tt 1) (symbol->string name)))
        (if (list-type? el)
            (values (elm-unary "Flatten" q #f) el)
            (values q pt))]
       [else (values (elm-property tn (symbol->string name) #f) pt)])]))

;; property-type : CType string -> CType|#f
(define system-properties
  (hash "Code" (hash "code" "String" "system" "String" "version" "String" "display" "String")
        "Concept" (hash "codes" (vector 'list "Code") "display" "String")
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
    [(vector 'list el) (let ([p (property-type el name)]) (and p (if (list-type? p) p (vector 'list p))))]
    [(vector 'interval el) (and (member name '("low" "high")) el)]
    [(vector 'tuple els) (let ([e (assq (string->symbol name) els)]) (and e (cdr e)))]
    [(vector 'choice _) "Any"]
    [_ #f]))

(define (compile-index target idx env)
  (define-values (tn tt) (compile target env))
  (define-values (in _it) (compile idx env))
  (values (elm-nary "Indexer" (list tn in) (simple-rt (elem-type tt))) (elem-type tt)))

(define (elem-type t) (if (and (vector? t) (eq? (vector-ref t 0) 'list)) (vector-ref t 1) "Any"))

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
  (define ns (for/list ([a (in-list args)]) (let-values ([(n _t) (compile a env)]) n)))
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
  (define el (if (list-type? tt) (vector-ref tt 1) tt))
  (define (this-env) (struct-copy cenv (cenv-bind env 'alias (list (cons '$this el))) [this "$this"]))
  (match op
    ['where
     (define-values (c _ct) (sys (car args) (this-env)))
     (values (elm-query (list (cons "$this" tn)) '() '() c #f #f #f #f) (vector 'list el))]
    ['select
     (define-values (r rt) (compile (car args) (this-env)))
     (values (elm-query (list (cons "$this" tn)) '() '() #f r #f #f #f) (vector 'list rt))]
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
    [else (fail (format "no overload of ~a~a matches ~s" (if alias (format "~a." alias) "") name (map cdr compiled)))]))

;; ---- overloads and conversions ----------------------------------------------
;; conversion-cost : CType CType -> number|#f   0 exact/subtype, 1 one implicit conversion, 2 two
(define (conversion-cost from to)
  (define m (lctx-model (L)))
  (cond
    [(equal? from to) 0]
    [(or (equal? to "Any") (equal? from "Any")) 0]
    [(and (string? from) (string? to) m (model-subtype? m from to)) 0]
    [(and (vector? to) (eq? (vector-ref to 0) 'choice))
     ;; a choice argument fits a choice parameter when all its members do (0), or some do (1: the
     ;; translator accepts the overlap)
     (cond [(vector? from)
            (if (eq? (vector-ref from 0) 'choice)
                (let ([ks (map (lambda (f) (conversion-cost f to)) (vector-ref from 1))])
                  (cond [(andmap values ks) 0] [(ormap values ks) 1] [else #f]))
                #f)]
           [else (for/or ([c (in-list (vector-ref to 1))]) (let ([k (conversion-cost from c)]) (and k (= k 0) 0)))])]
;; a choice argument to a parameter of one of its member types: an implicit As (cost 1); to a
    ;; type a member CONVERTS to: As then the conversion (cost 2) — ToConcept(m as CodeableConcept)
    [(and (vector? from) (eq? (vector-ref from 0) 'choice) (string? to)
          (for/or ([c (in-list (vector-ref from 1))]) (eqv? 0 (conversion-cost c to))))
     1]
    [(and (vector? from) (eq? (vector-ref from 0) 'choice) (string? to)
          (for/or ([c (in-list (vector-ref from 1))]) (eqv? 1 (conversion-cost c to))))
     2]
    [(and (vector? from) (vector? to) (eq? (vector-ref from 0) (vector-ref to 0)) (memq (vector-ref from 0) '(list interval)))
     (conversion-cost (vector-ref from 1) (vector-ref to 1))]
    [(and (string? from) (string? to) (hash-ref promotions (cons from to) #f)) 1]
    [(and (string? from) m (model-conversion m from))
     => (lambda (c) (let ([k (conversion-cost (car c) to)]) (and k (< k 2) (+ 1 k))))]
    [(and (equal? to "Tuple") (vector? from) (eq? (vector-ref from 0) 'tuple)) 0]
    [else #f]))

;; coerce : elm CType CType -> elm   apply the implicit conversions conversion-cost counted
(define (coerce n from to)
  (define m (lctx-model (L)))
  (cond
    ;; a value passed to a choice-typed parameter is cast to the whole choice
    ;; a choice argument only partly inside a choice parameter is cast to the parameter's choice
    [(and (vector? to) (eq? (vector-ref to 0) 'choice) (vector? from) (eq? (vector-ref from 0) 'choice)
          (eqv? (conversion-cost from to) 1))
     (let ([spec (ctype->elm-type to)]) (if spec (elm-as n spec #f) n))]
    ;; (a member type is widened; a choice argument already fits as it is)
    [(and (vector? to) (eq? (vector-ref to 0) 'choice) (string? from) (not (equal? from "Any")) (conversion-cost from to))
     (let ([spec (ctype->elm-type to)]) (if spec (elm-as n spec #f) n))]
    [(eqv? (conversion-cost from to) 0) n]
    [(and (string? from) (string? to) (hash-ref promotions (cons from to) #f)) (promote n from to)]
    [(and (vector? from) (eq? (vector-ref from 0) 'choice) (string? to)
          (for/or ([c (in-list (vector-ref from 1))]) (eqv? 0 (conversion-cost c to))))
     (elm-as n (ctype->elm-name to) #f)]
    [(and (vector? from) (eq? (vector-ref from 0) 'choice) (string? to))
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
           (cons (elm-property (elm-expression-ref "Patient" #f) (model-birth-date-path model) #f) "Date"))))
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
    [(not spec) (fail (format "unknown function '~a'" name))]
    [else
     (define-values (nodes0 types0)
       (for/lists (ns ts) ([a (in-list all-args)])
         (cond [(memq name generic-ops) (compile a env)]
               ;; Boolean tests over a choice pick the member converting to Boolean
               [(memq name '(IsTrue IsFalse))
                (let-values ([(n t) (compile a env)])
                  (if (and (vector? t) (eq? (vector-ref t 0) 'choice)) (choice-to n t "Boolean") (to-system n t)))]
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
  (define (el t) (or (point-of t) "Any"))
  (case name
    [(Start End PointFrom Width Min Max Sum First Last SingletonFrom Median Mode Product Indexer) (el t0)]
    [(Abs Predecessor Successor Distinct Tail Take Skip Slice Expand Collapse LowBoundary HighBoundary Message) t0]
    [(Flatten) (let ([e (point-of t0)]) (if (list-type? e) e t0))]
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
    [(interval-type el) (vector 'interval (type->ctype el))]
    [(list-type el) (vector 'list (type->ctype el))]
    [(choice-type ts) (vector 'choice (map type->ctype ts))]
    [(tuple-type els) (vector 'tuple (map (lambda (e) (cons (car e) (type->ctype (cdr e)))) els))]
    [_ "Any"]))
;; ctype->elm-name : CType(string) -> "{urn:hl7-org:elm-types:r1}Integer" | "{http://hl7.org/fhir}Period"
(define (ctype->elm-name s)
  (define m (lctx-model (L)))
  (cond [(and m (regexp-match? (pregexp (string-append "^" (regexp-quote (model-name m)) "[.]")) s))
         (format "{~a}~a" (model-url m) (local-name s))]
        [(regexp-match? #rx"[.{]" s) s]
        [else (system-type-name s)]))

;; ctype->elm-type : CType -> elm type-spec | #f (Any)
(define (ctype->elm-type t)
  (match t
    ["Any" #f]
    [(? string? s) (elm-named-type (ctype->elm-name s))]
    [(vector 'list el) (let ([e (ctype->elm-type el)]) (and e (elm-list-type e)))]
    [(vector 'interval el) (let ([e (ctype->elm-type el)]) (and e (elm-interval-type e)))]
    [(vector 'choice ts) (let ([es (map ctype->elm-type ts)]) (and (andmap values es) (elm-choice-type es)))]
    [(vector 'tuple els) (let ([es (map (lambda (e) (cons (symbol->string (car e)) (ctype->elm-type (cdr e)))) els)])
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
  (check-true (elm-expression-ref? (car (elm-nary-operands (elm-def-expression (cadr (elm-library-statements lib))))))))
