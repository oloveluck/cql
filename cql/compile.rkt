#lang racket/base

;;; compile.rkt — CQL AST -> ELM AST (the translator).
;;;
;;; CQL surface operators are overloaded (`+` is Add on numbers but Concatenate on strings; `=` is
;;; Equal; membership picks In/Contains), so translation needs a light System-type inference. We infer
;;; bottom-up: `compile` returns (values elm-node type), where `type` is a System type descriptor
;;; (a simple-name string like "Integer", or a `(vector 'list T)` / `(vector 'interval T)` composite,
;;; or "Any"). The inferred simple-name is stamped onto ELM nodes as resultType so the emitted ELM is
;;; self-describing. Errors raise a cql-error caught at the API boundary and returned as (err …).
;;;
;;; Slice-1 simplification: temporal literals (@date/@time) compile to an ELM Literal carrying the
;;; lexeme (canonical ELM Date/DateTime/Time *constructor* nodes are deferred) — this round-trips
;;; through our interpreter, which is what the closed-loop conformance harness checks.

(require racket/match
         "parser/ast.rkt"
         "elm/ast.rkt"
         (only-in "error.rkt" cql-error cql-error?)
         racket/contract
         (only-in fhir/result/result ok err ok? err?)
         (only-in fhir/fhirpath/temporal-ops
                  parse-fp-date parse-fp-datetime parse-fp-time
                  moment-year moment-month moment-day moment-hour moment-minute moment-second
                  moment-frac moment-tzmin))

(provide
 (contract-out
  ;; compile-library : cql-library -> (ok elm-library) | (err cql-error)
  [compile-library (-> cql-library? (or/c ok? err?))]
  ;; compile-expr : cql-expression-AST -> (ok elm-expr) | (err cql-error)  (bare-expression entry)
  [compile-expr    (-> any/c (or/c ok? err?))]))

;; ---- compile scope ---------------------------------------------------------
;; what names are in scope, so a bare identifier resolves to the right ELM ref kind
(struct cenv (defs funcs operands params) #:transparent)
(define empty-cenv (cenv (hash) (hash) (hash) (hash)))
(define (cenv-add-operands e names)
  (cenv (cenv-defs e) (cenv-funcs e)
        (for/fold ([h (cenv-operands e)]) ([n (in-list names)]) (hash-set h n #t))
        (cenv-params e)))

;; ---- public API ------------------------------------------------------------
;; compile-library : cql-library -> (ok elm-library) | (err cql-error)
(define (compile-library lib)
  (with-handlers ([cql-error? (lambda (e) (err e))])
    (define defs (cql-library-defs lib))
    ;; gather top-level names first so forward references resolve
    (define def-names (for/hash ([d (in-list defs)] #:when (cql-def? d)) (values (cql-def-name d) #t)))
    (define func-names (for/hash ([d (in-list defs)] #:when (cql-func? d)) (values (cql-func-name d) #t)))
    (define param-names (for/hash ([d (in-list defs)] #:when (cql-parameter? d)) (values (cql-parameter-name d) #t)))
    (define base (cenv def-names func-names (hash) param-names))
    (define usings (cons (elm-using "System" "urn:hl7-org:elm-types:r1" "1.0.0")
                         (for/list ([d (in-list defs)] #:when (cql-using? d))
                           (elm-using (or (cql-using-alias d) (symbol->string (cql-using-model d)))
                                      (symbol->string (cql-using-model d)) (cql-using-version d)))))
    (define statements
      (for/list ([d (in-list defs)] #:when (or (cql-def? d) (cql-func? d)))
        (compile-statement d base)))
    (ok (elm-library (and (cql-library-id lib) (symbol->string (cql-library-id lib)))
                     (cql-library-version lib) usings statements))))

;; compile-expr : cql-ast -> (ok elm-node) | (err cql-error)  — a bare expression (harness/CLI)
(define (compile-expr ast)
  (with-handlers ([cql-error? (lambda (e) (err e))])
    (define-values (node _t) (compile ast empty-cenv))
    (ok node)))

(define (compile-statement d env)
  (cond
    [(cql-def? d)
     (define-values (node _t) (compile (cql-def-body d) env))
     (elm-def (symbol->string (cql-def-name d)) "Unfiltered" (cql-def-access d) node)]
    [(cql-func? d)
     (define opnames (map cql-operand-name (cql-func-operands d)))
     (define env* (cenv-add-operands env opnames))
     (define body (cql-func-body d))
     (define node (if (eq? body 'external) (elm-null #f)
                      (let-values ([(n _t) (compile body env*)]) n)))
     (elm-function-def (symbol->string (cql-func-name d)) "Unfiltered" (cql-func-access d)
                       (map (lambda (o) (elm-operand-def (symbol->string (cql-operand-name o))
                                                         (compile-type (cql-operand-type o))))
                            (cql-func-operands d))
                       (and (cql-func-result-type d) (compile-type (cql-func-result-type d)))
                       node)]))

;; ---- expression compilation: cql-ast env -> (values elm-node type) ---------
(define (fail msg) (raise (cql-error 'translate msg)))

(define (compile ast env)
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
    [(cql-timing op prec a b)
     (let-values ([(an _a) (compile a env)] [(bn _b) (compile b env)])
       (values (elm-timing op (and prec (units->prec prec)) an bn) "Boolean"))]
    [(cql-query _s _l _w _r _sort _agg) (compile-query ast env)]
    [(cql-minmax which type) (values (minmax-elm which (type-simple type)) (or (type-simple type) "Any"))]
    [(cql-instance type elems)
     (values (elm-instance (symbol->string type)
                           (for/list ([te (in-list elems)])
                             (elm-tuple-elem (cql-tuple-elem-name te) (compile1 (cql-tuple-elem-value te) env))))
             (symbol->string type))]
    [_ (fail (format "unsupported expression: ~s" ast))]))

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

(define (compile1 e env) (let-values ([(n _t) (compile e env)]) n))

;; a query: bind source aliases (and lets, and the aggregate accumulator) as operands so the body's
;; references compile to OperandRefs the interpreter fills per row.
(define (compile-query q env)
  (define srcs (cql-query-sources q))
  (define aliases (map cql-source-alias srcs))
  (define src-elms (for/list ([s (in-list srcs)]) (cons (symbol->string (cql-source-alias s)) (compile1 (cql-source-expr s) env))))
  (define env1 (cenv-add-operands env aliases))
  (define lets (cql-query-lets q))
  (define env2 (cenv-add-operands env1 (map cql-let-def-name lets)))
  (define let-elms (for/list ([l (in-list lets)]) (cons (symbol->string (cql-let-def-name l)) (compile1 (cql-let-def-expr l) env2))))
  (define where (and (cql-query-where q) (compile1 (cql-query-where q) env2)))
  (define ret (and (cql-query-ret q) (compile1 (cql-return-expr (cql-query-ret q)) env2)))
  (define ret-distinct (and (cql-query-ret q) (cql-return-distinct? (cql-query-ret q))))
  (define agg
    (and (cql-query-agg q)
         (let* ([a (cql-query-agg q)] [envA (cenv-add-operands env2 (list (cql-agg-name a)))])
           (list (symbol->string (cql-agg-name a)) (cql-agg-distinct? a)
                 (and (cql-agg-starting a) (compile1 (cql-agg-starting a) env2))
                 (compile1 (cql-agg-body a) envA)))))
  (values (elm-query src-elms let-elms where ret ret-distinct (cql-query-sort q) agg) "Any"))

;; singularize a units word (years -> year) and validate it's a temporal precision
(define duration-units '(year month week day hour minute second millisecond))
(define (units->prec u)
  (define s (symbol->string u))
  (string->symbol (if (regexp-match? #rx"s$" s) (substring s 0 (sub1 (string-length s))) s)))
(define (compile-between kind units a b env)
  (define prec (units->prec units))
  (unless (memq prec duration-units) (fail (format "'~a' is not a valid duration unit" units)))
  (define-values (an _at) (compile a env))
  (define-values (bn _bt) (compile b env))
  (values (elm-between kind prec an bn) "Integer"))

;; <precision> from operand — validate the precision word, emit a canonical ELM component node
(define component-precisions '(year month day hour minute second millisecond timezoneoffset timezone date time))
(define (compile-component prec operand env)
  (unless (memq prec component-precisions) (fail (format "'~a from' is not a valid component extraction" prec)))
  (define-values (o _t) (compile operand env))
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

;; ---- references ------------------------------------------------------------
(define (compile-ref name env)
  (cond
    [(hash-ref (cenv-operands env) name #f) (values (elm-operand-ref (symbol->string name)) "Any")]
    [(hash-ref (cenv-params env) name #f) (values (elm-parameter-ref (symbol->string name) #f) "Any")]
    [(hash-ref (cenv-defs env) name #f) (values (elm-expression-ref (symbol->string name) #f) "Any")]
    ;; a lone Boolean-ish literal keyword shouldn't reach here; unknown identifiers are errors
    [else (fail (format "unknown identifier '~a'" name))]))

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
  (define-values (o t) (compile operand env))
  (case op
    [(+) (values o t)]                                        ; unary plus is identity
    [(-) (values (elm-unary "Negate" o t) t)]
    [(not) (values (elm-unary "Not" o "Boolean") "Boolean")]
    [(exists) (values (elm-unary "Exists" o "Boolean") "Boolean")]
    [else (fail (format "unsupported unary operator ~a" op))]))

;; ---- binary ----------------------------------------------------------------
(define (compile-binop op l r env)
  (define-values (lo lt) (compile l env))
  (define-values (ro rt) (compile r env))
  (define (bin name res) (values (elm-nary name (list lo ro) res) res))
  (case op
    [(+) (if (or (equal? lt "String") (equal? rt "String"))
             (bin "Concatenate" "String")                    ; CQL `+` on strings is null-propagating
             (bin "Add" (numeric-or lt rt)))]
    ;; CQL `&` treats null as '' — desugar to Concatenate(Coalesce(l,''), Coalesce(r,''))
    [(&) (define e "") (define empty (elm-literal "String" e))
         (values (elm-nary "Concatenate"
                           (list (elm-nary "Coalesce" (list lo empty) "String")
                                 (elm-nary "Coalesce" (list ro empty) "String"))
                           "String")
                 "String")]
    [(-) (bin "Subtract" (numeric-or lt rt))]
    [(*) (bin "Multiply" (numeric-or lt rt))]
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
    [(in) (bin "In" "Boolean")]
    [(contains) (bin "Contains" "Boolean")]
    [(union) (bin "Union" (or (composite-or lt rt) "Any"))]
    [(intersect) (bin "Intersect" (or (composite-or lt rt) "Any"))]
    [(except) (bin "Except" (or (composite-or lt rt) "Any"))]
    [else (fail (format "unsupported binary operator ~a" op))]))

;; numeric promotion Integer < Long < Decimal < Quantity ; #f/Any when not numeric
(define numeric-rank (hash "Integer" 1 "Long" 2 "Decimal" 3 "Quantity" 4))
(define (numeric-or a b)
  (define ra (hash-ref numeric-rank a #f))
  (define rb (hash-ref numeric-rank b #f))
  (cond [(and ra rb) (if (>= ra rb) a b)]
        [ra a] [rb b] [else "Any"]))
(define (composite-or a b) (cond [(vector? a) a] [(vector? b) b] [else #f]))

;; ---- control flow ----------------------------------------------------------
(define (compile-if c t e env)
  (define-values (cn _ct) (compile c env))
  (define-values (tn tt) (compile t env))
  (define-values (en _et) (compile e env))
  (values (elm-if cn tn en) tt))

(define (compile-case comparand items els env)
  (define cmp (and comparand (let-values ([(n _t) (compile comparand env)]) n)))
  (define result-type (box "Any"))
  (define its (for/list ([it (in-list items)])
                (define-values (wn _wt) (compile (cql-case-item-when it) env))
                (define-values (tn tt) (compile (cql-case-item-then it) env))
                (set-box! result-type tt)
                (elm-case-item wn tn)))
  (define en (and els (let-values ([(n _t) (compile els env)]) n)))
  (values (elm-case cmp its en) (unbox result-type)))

;; ---- collections -----------------------------------------------------------
(define (compile-list elems env)
  (define-values (nodes t)
    (for/fold ([acc '()] [t "Any"] #:result (values (reverse acc) t)) ([e (in-list elems)])
      (define-values (n et) (compile e env))
      (values (cons n acc) et)))
  (values (elm-list nodes) (vector 'list t)))

(define (compile-interval low high lc hc env)
  (define-values (ln lt) (compile low env))
  (define-values (hn _ht) (compile high env))
  (values (elm-interval ln hn lc hc) (vector 'interval lt)))

(define (compile-tuple elems env)
  (values (elm-tuple (for/list ([te (in-list elems)])
                       (define-values (vn _t) (compile (cql-tuple-elem-value te) env))
                       (elm-tuple-elem (cql-tuple-elem-name te) vn)))
          "Tuple"))

;; ---- navigation / calls / indexing ----------------------------------------
(define (compile-property target name env)
  (define-values (tn _t) (compile target env))
  (values (elm-property tn (symbol->string name)) "Any"))

(define (compile-index target idx env)
  (define-values (tn tt) (compile target env))
  (define-values (in _it) (compile idx env))
  (values (elm-nary "Indexer" (list tn in) (elem-type tt)) (elem-type tt)))

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
        'Avg         '(unary . "Decimal")  'First '(unary . "Any") 'Last '(unary . "Any")
        'Length      '(unary . "Integer")  'Upper '(unary . "String") 'Lower '(unary . "String")
        'Exists      '(unary . "Boolean")  'Not '(unary . "Boolean") 'Distinct '(unary . #f)
        'Log         '(nary . "Decimal")   'Power '(nary . #f) 'Round '(nary . "Decimal")
        'Coalesce    '(nary . "Any")       'Combine '(nary . "String") 'Concatenate '(nary . "String")
        'StartsWith  '(nary . "Boolean")   'EndsWith '(nary . "Boolean") 'Substring '(nary . "String")
        'PositionOf  '(nary . "Integer")   'Indexer '(nary . "Any")
        ;; interval operators
        'Start '(unary . #f) 'End '(unary . #f) 'Width '(unary . #f) 'PointFrom '(unary . #f)
        'SingletonFrom '(unary . #f) 'Collapse '(unary . #f) 'Expand '(nary . #f)
        ;; list operators
        'Take '(nary . #f) 'Skip '(nary . #f) 'Tail '(unary . #f) 'Slice '(nary . #f)
        'IndexOf '(nary . "Integer") 'Flatten '(unary . #f)
        ;; aggregate functions
        'AllTrue '(unary . "Boolean") 'AnyTrue '(unary . "Boolean") 'Product '(unary . #f) 'Mode '(unary . #f)
        'Median '(unary . "Decimal") 'StdDev '(unary . "Decimal") 'Variance '(unary . "Decimal")
        'PopulationStdDev '(unary . "Decimal") 'PopulationVariance '(unary . "Decimal") 'GeometricMean '(unary . "Decimal")
        ;; string functions
        'Split '(nary . #f) 'LastPositionOf '(nary . "Integer") 'Matches '(nary . "Boolean")
        'ReplaceMatches '(nary . "String") 'Message '(nary . "Any")
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

(define (compile-call target name args env)
  (cond
    ;; temporal constructors (only in call position, no target)
    [(and (not target) (memq name '(Date DateTime Time))) (compile-constructor name args env)]
    ;; expand's per-argument may be a bare unit keyword (normalize, then compile as usual)
    [(and (not target) (eq? name 'Expand) (= 2 (length args)) (not (equal? args (map normalize-per-arg args))))
     (compile-call target 'Expand (map normalize-per-arg args) env)]
    ;; user-defined function (local) -> FunctionRef
    [(and (not target) (hash-ref (cenv-funcs env) name #f))
     (values (elm-function-ref (symbol->string name) #f (map (lambda (a) (let-values ([(n _t) (compile a env)]) n)) args))
             "Any")]
    [else
     ;; method-style x.f(args) is sugar for f(x, args)
     (define all-args (if target (cons target args) args))
     (define spec (hash-ref system-ops name #f))
     (cond
       [(not spec) (fail (format "unknown function '~a'" name))]
       [else
        (define nodes (map (lambda (a) (let-values ([(n _t) (compile a env)]) n)) all-args))
        (define rt (cdr spec))
        (case (car spec)
          [(unary)
           (unless (= 1 (length nodes)) (fail (format "~a expects 1 argument" name)))
           (values (elm-unary (symbol->string name) (car nodes) rt) (or rt "Any"))]
          [(nary)
           (values (elm-nary (symbol->string name) nodes rt) (or rt "Any"))])])]))

;; ---- type ops --------------------------------------------------------------
(define (compile-typeop op operand type env)
  (define-values (o _t) (compile operand env))
  (case op
    [(is) (values (elm-is o (compile-type type)) "Boolean")]
    [(as) (values (elm-as o (compile-type type) #f) (type-simple type))]
    [else (fail (format "unsupported type operator ~a" op))]))

(define (compile-cast operand type env)
  (define-values (o _t) (compile operand env))
  (values (elm-as o (compile-type type) #t) (type-simple type)))

(define (compile-convert operand type env)
  (define-values (o _t) (compile operand env))
  (define simple (type-simple type))
  ;; convert to a System type -> a To<Type> operator when known, else a strict As
  (if (and simple (member simple '("String" "Integer" "Long" "Decimal" "Quantity" "Boolean" "Date" "DateTime" "Time")))
      (values (elm-to o simple) simple)
      (values (elm-as o (compile-type type) #t) (or simple "Any"))))

;; a type-spec's simple System name (for result-type inference), or #f for composite/qualified
(define (type-simple type)
  (match type
    [(named-type name) (let ([s (symbol->string name)]) (if (regexp-match? #rx"[.]" s) #f s))]
    [_ #f]))

(define (compile-type type)
  (match type
    [(named-type name) (elm-named-type (qualify-type-name (symbol->string name)))]
    [(interval-type el) (elm-interval-type (compile-type el))]
    [(list-type el) (elm-list-type (compile-type el))]
    [_ (fail (format "unsupported type specifier: ~s" type))]))

;; a bare simple name is a System type; a qualified name (Model.Type) is passed through as-is
(define system-simple-types
  '("Integer" "Long" "Decimal" "Quantity" "Ratio" "String" "Boolean" "Date" "DateTime" "Time"
    "Code" "Concept" "Any" "Vocabulary" "ValueSet" "CodeSystem"))
(define (qualify-type-name s)
  (cond [(regexp-match? #rx"[.{]" s) s]                     ; already qualified
        [(member s system-simple-types) (system-type-name s)]
        [else s]))

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
