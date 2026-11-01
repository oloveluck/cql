#lang racket/base

;;; ast.rkt — the CQL AST + lowering from brag's parse-tree datum.
;;;
;;; Follows the fhirpath/parser/ast.rkt pattern: AST structs are #:prefab so they survive the
;;; reader->expander boundary as literal data (the #lang reader emits them into the module body).
;;; `lower` collapses the precedence cascade (chains of single-child wrappers) into compact nodes and
;;; left-folds operator chains into binops. Literal precision is preserved (dec-lit/qty-lit/long-lit
;;; keep their lexemes; temporals keep their lexeme).

(require racket/match
         racket/contract
         (only-in racket/list add-between filter-map append-map last))

(provide (struct-out cql-library)
         (struct-out cql-using) (struct-out cql-include) (struct-out cql-parameter)
         (struct-out cql-codesystem) (struct-out cql-valueset) (struct-out cql-context)
         (struct-out cql-def) (struct-out cql-func) (struct-out cql-operand)
         (struct-out cql-lit) (struct-out cql-binop) (struct-out cql-unop)
         (struct-out cql-if) (struct-out cql-case) (struct-out cql-case-item)
         (struct-out cql-interval) (struct-out cql-list) (struct-out cql-tuple) (struct-out cql-tuple-elem)
         (struct-out cql-property) (struct-out cql-ref) (struct-out cql-call) (struct-out cql-index)
         (struct-out cql-component) (struct-out cql-between) (struct-out cql-timing)
         (struct-out cql-query) (struct-out cql-source) (struct-out cql-let-def)
         (struct-out cql-return) (struct-out cql-agg) (struct-out cql-minmax) (struct-out cql-instance)
         (struct-out cql-typeop) (struct-out cql-cast) (struct-out cql-convert)
         (struct-out named-type) (struct-out interval-type) (struct-out list-type)
         (struct-out dec-lit) (struct-out long-lit) (struct-out qty-lit) (struct-out ratio-lit)
         (struct-out date-lit) (struct-out datetime-lit) (struct-out time-lit)
         (contract-out
          [lower         (-> any/c any/c)]
          [lower-library (-> any/c cql-library?)]))

;; ---- AST -------------------------------------------------------------------
;; library + definitions
(struct cql-library   (id version defs)                   #:prefab) ; id: symbol|#f ; defs: (listof def)
(struct cql-using     (model version alias)               #:prefab)
(struct cql-include   (lib version alias)                 #:prefab)
(struct cql-parameter (name type default)                 #:prefab) ; type/default may be #f
(struct cql-codesystem (name id version)                  #:prefab)
(struct cql-valueset  (name id version)                   #:prefab)
(struct cql-context   (name)                              #:prefab)
(struct cql-def       (name access body)                  #:prefab) ; access: 'public|'private ; body: expr
(struct cql-func      (name access operands result-type body) #:prefab) ; body: expr | 'external
(struct cql-operand   (name type)                         #:prefab)

;; expressions
(struct cql-lit       (value)         #:prefab) ; int | dec-lit | long-lit | string | bool | 'null | qty-lit | *-lit
(struct cql-binop     (op lhs rhs)    #:prefab) ; op symbol
(struct cql-unop      (op operand)    #:prefab) ; op: '+ '- 'not 'exists
(struct cql-if        (cond then els) #:prefab)
(struct cql-case      (comparand items els) #:prefab) ; comparand #f for a standard case; els may be #f
(struct cql-case-item (when then)     #:prefab)
(struct cql-interval  (low high low-closed? high-closed?) #:prefab)
(struct cql-list      (elements)      #:prefab)
(struct cql-tuple     (elements)      #:prefab) ; (listof cql-tuple-elem)
(struct cql-tuple-elem (name value)   #:prefab)
(struct cql-property  (target name)   #:prefab) ; target.name ; name symbol
(struct cql-ref       (name)          #:prefab) ; bare identifier reference ; name symbol
(struct cql-call      (target name args) #:prefab) ; target #f for a local/system function
(struct cql-index     (target idx)    #:prefab)
(struct cql-component (precision operand) #:prefab) ; <precision> from operand ; precision a symbol
(struct cql-between   (kind precision a b) #:prefab) ; kind 'duration|'difference ; precision a symbol
(struct cql-timing    (op precision a b) #:prefab)  ; op 'before|'after|'same-as|'same-or-before|'same-or-after
;; queries
(struct cql-query   (sources lets where ret sort agg) #:prefab) ; sort 'asc|'desc|#f
(struct cql-source  (alias expr)   #:prefab)
(struct cql-let-def (name expr)    #:prefab)
(struct cql-return  (distinct? expr) #:prefab)
(struct cql-agg     (distinct? name starting body) #:prefab)
(struct cql-minmax  (which type) #:prefab)          ; which 'min|'max ; type a type-spec node
(struct cql-instance (type elements) #:prefab)      ; Code{…}/Concept{…}/Quantity{…}/Model.Type{…} selector
(struct cql-typeop    (op operand type) #:prefab) ; op 'is|'as ; type a type-spec node
(struct cql-cast      (operand type)  #:prefab)
(struct cql-convert   (operand type)  #:prefab)

;; type specifiers
(struct named-type    (name)          #:prefab) ; name symbol (qualified via ".")
(struct interval-type (element)       #:prefab)
(struct list-type     (element)       #:prefab)

;; literal tags (dec/qty/date/datetime/time shared in spirit with FHIRPath; defined here for CQL)
(struct dec-lit      (lexeme)        #:prefab)
(struct long-lit     (lexeme)        #:prefab)
(struct qty-lit      (number unit)   #:prefab) ; number: exact-int|dec-lit ; unit: string
(struct ratio-lit    (numerator denominator) #:prefab) ; both: qty-lit | number-lit
(struct date-lit     (str)           #:prefab)
(struct datetime-lit (str)           #:prefab)
(struct time-lit     (str)           #:prefab)

;; ---- helpers ---------------------------------------------------------------
(define (op->sym s) (if (symbol? s) s (string->symbol s)))
(define (id->sym x) (if (symbol? x) x (string->symbol x)))

;; a grammar `identifier` node is (identifier IDENTIFIER|"as"|…) ; unwrap to its symbol
(define (unwrap-id x)
  (match x
    [(list 'identifier a) (id->sym a)]
    [(? symbol? s) s]
    [(? string? s) (string->symbol s)]
    [_ x]))

;; ---- library lowering ------------------------------------------------------
(define (lower-library datum)
  (match datum
    [(list 'cql-library parts ...)
     (define header (findf (tagged? 'library-header) parts))
     (define-values (id version)
       (if header (lower-header header) (values #f #f)))
     (define defs (filter-map (lambda (p) (and (tagged?* 'definition p) (lower-definition p))) parts))
     (cql-library id version defs)]
    [_ (error 'cql-lower "not a library: ~s" datum)]))

(define ((tagged? tag) x) (and (pair? x) (eq? (car x) tag)))
(define (tagged?* tag x) (and (pair? x) (eq? (car x) tag)))

(define (lower-header h)
  (match h
    [(list 'library-header qid rest ...)
     (values (lower-qualified qid) (find-version rest))]))

(define (find-version parts)
  (define v (findf (tagged? 'version-spec) parts))
  (and v (match v [(list 'version-spec s) s])))

(define (find-alias parts)
  (define a (findf (tagged? 'called-spec) parts))
  (and a (match a [(list 'called-spec idn) (unwrap-id idn)])))

(define (lower-definition d)
  (match d
    [(list 'definition inner) (lower-definition inner)]
    [(list 'using-def qid rest ...) (cql-using (lower-qualified qid) (find-version rest) (find-alias rest))]
    [(list 'include-def qid rest ...) (cql-include (lower-qualified qid) (find-version rest) (find-alias rest))]
    [(list 'parameter-def idn rest ...)
     (cql-parameter (unwrap-id idn)
                    (let ([t (findf (tagged? 'type-specifier) rest)]) (and t (lower-type t)))
                    (let ([d (findf (tagged? 'param-default) rest)])
                      (and d (match d [(list 'param-default e) (lower e)]))))]
    [(list 'codesystem-def idn cid rest ...) (cql-codesystem (unwrap-id idn) cid (find-version rest))]
    [(list 'valueset-def idn vid rest ...) (cql-valueset (unwrap-id idn) vid (find-version rest))]
    [(list 'context-def idn) (cql-context (unwrap-id idn))]
    [(list 'statement-def inner) (lower-definition inner)]
    [(list 'expression-def parts ...) (lower-expression-def parts)]
    [(list 'function-def parts ...) (lower-function-def parts)]
    [_ (error 'cql-lower "unexpected definition: ~s" d)]))

(define (access-of parts)
  (define a (findf (tagged? 'access-modifier) parts))
  (if a (match a [(list 'access-modifier m) (string->symbol m)]) 'public))

(define (lower-expression-def parts)
  ;; parts: access-modifier? identifier expr
  (define name (unwrap-id (findf (tagged? 'identifier) parts)))
  (define body (lower (findf (tagged? 'expr) parts)))
  (cql-def name (access-of parts) body))

(define (lower-function-def parts)
  (define name (unwrap-id (findf (tagged? 'identifier) parts)))
  (define ops (let ([ol (findf (tagged? 'operand-list) parts)])
                (if ol (lower-operand-list ol) '())))
  (define rt (let ([r (findf (tagged? 'returns-spec) parts)])
               (and r (match r [(list 'returns-spec ts) (lower-type ts)]))))
  (define body (let ([b (findf (tagged? 'func-body) parts)])
                 (match b
                   [(list 'func-body "external") 'external]
                   [(list 'func-body e) (lower e)]
                   [_ 'external])))
  (cql-func name (access-of parts) ops rt body))

(define (lower-operand-list ol)
  (match ol
    [(list 'operand-list ops ...)
     (map (lambda (o) (match o [(list 'operand idn ts) (cql-operand (unwrap-id idn) (lower-type ts))])) ops)]))

;; ---- expression lowering ---------------------------------------------------
(define (lower datum)
  (match datum
    [(list 'expr e) (lower e)]
    [(list 'query parts ...) (lower-query parts)]

    ;; binary levels: operand (op operand)* — left associative
    [(list (or 'implies-expr 'or-expr 'and-expr 'membership-expr 'equality-expr
               'setop-expr 'additive-expr 'multiplicative-expr 'power-expr)
           first rest ...)
     (fold-binops (lower first) rest)]

    ;; comparison level also carries timing operators (before/after/same-as/…), possibly precision-qualified
    [(list 'comparison-expr first tails ...) (fold-comp (lower first) tails)]

    ;; type test / cast / convert
    [(list 'type-expr operand) (lower operand)]
    [(list 'type-expr operand tails ...) (fold-typeops (lower operand) tails)]
    [(list 'cast-expr operand ts) (cql-cast (lower operand) (lower-type ts))]
    [(list 'convert-expr operand ts) (cql-convert (lower operand) (lower-type ts))]

    ;; unary
    [(list 'unary-expr operand) (lower operand)]
    [(list 'unary-expr (? string? op) operand) (cql-unop (op->sym op) (lower operand))]

    ;; interval/list prefix operators -> system operator calls (reuse the call machinery)
    [(list 'interval-prefix kw operand) (interval-prefix->call kw (lower operand) #f)]
    [(list 'interval-prefix kw operand per) (interval-prefix->call kw (lower operand) (lower per))]

    ;; postfix: primary tail*
    [(list 'postfix-expr prim tails ...)
     (for/fold ([acc (lower prim)]) ([t (in-list tails)]) (apply-tail acc t))]

    [(list 'primary inner) (lower inner)]

    ;; selectors and control
    [(list 'if-expr c t e) (cql-if (lower c) (lower t) (lower e))]
    [(list 'case-expr parts ...) (lower-case parts)]
    [(list 'interval-selector open low high close)
     (cql-interval (lower low) (lower high)
                   (equal? (cadr open) "[") (equal? (cadr close) "]"))]
    [(list 'typed-list-selector parts ...)
     (cql-list (append-map-body parts 'list-body))]
    [(list 'typed-tuple-selector parts ...)
     (cql-tuple (tuple-elems parts))]
    [(list 'braced) (cql-list '())]
    [(list 'braced (list 'braced-body inner)) (lower-braced-body inner)]

    ;; calls / refs
    [(list 'function-call name) (cql-call #f (unwrap-id name) '())]
    [(list 'function-call name (list 'arg-list args ...)) (cql-call #f (unwrap-id name) (map lower args))]
    [(list 'member idn) (cql-ref (unwrap-id idn))]
    [(list 'component-from idn operand) (cql-component (unwrap-id idn) (lower operand))]
    [(list 'minmax which ts) (cql-minmax (if (equal? which "minimum") 'min 'max) (lower-type ts))]
    [(list 'instance-selector head parts ...)
     (cql-instance (if (string? head) (string->symbol head) (lower-qualified head))
                   (let ([b (findf (tagged? 'tuple-body) parts)])
                     (if b (match b [(list 'tuple-body es ...) (map lower-tuple-elem es)]) '())))]
    [(list 'duration-of units operand)
     ;; duration in U of X  ==  U between start of X and end of X
     (let ([x (lower operand)])
       (cql-between 'duration (unwrap-id units)
                    (cql-call #f 'Start (list x)) (cql-call #f 'End (list x))))]
    [(list 'difference-between units a b) (cql-between 'difference (unwrap-id units) (lower a) (lower b))]

    ;; leaves
    [(list 'literal "null") (cql-lit 'null)]
    [(list 'literal (list 'ratio (list 'quantity n ...) (list 'quantity d ...)))
     (cql-lit (ratio-lit (lower-quantity n) (lower-quantity d)))]
    [(list 'literal (list 'quantity qparts ...)) (cql-lit (lower-quantity qparts))]
    [(list 'literal v) (cql-lit (lit-value v))]
    [(list 'identifier x) (cql-ref (id->sym x))]

    [_ (error 'cql-lower "unexpected node: ~s" datum)]))

;; ---- query lowering --------------------------------------------------------
(define (lower-query parts)
  (define sources (for/list ([p (in-list parts)] #:when (tagged?* 'source-clause p)) (lower-source p)))
  (define tail (findf (tagged? 'query-tail) parts))
  (define-values (lets where ret sort agg) (lower-tail tail))
  (cql-query sources lets where ret sort agg))
(define (lower-source p)
  (match p [(list 'source-clause e (list 'alias idn)) (cql-source (unwrap-id idn) (lower e))]))
(define (lower-tail tail)
  (define parts (if tail (cdr tail) '()))
  (define lets (append-map (lambda (lc) (match lc [(list 'let-clause items ...) (map lower-let-item items)] [_ '()]))
                           (filter (tagged? 'let-clause) parts)))
  (define where (let ([w (findf (tagged? 'where-clause) parts)]) (and w (match w [(list 'where-clause e) (lower e)]))))
  (define roa (findf (tagged? 'return-or-agg) parts))
  (define-values (ret agg) (if roa (lower-roa roa) (values #f #f)))
  (define sort (let ([s (findf (tagged? 'sort-clause) parts)])
                 (and s (match s [(list 'sort-clause (list 'sort-dir d)) (if (member d '("desc" "descending")) 'desc 'asc)]))))
  (values lets where ret sort agg))
(define (lower-let-item li) (match li [(list 'let-item idn e) (cql-let-def (unwrap-id idn) (lower e))]))
(define (return-mod-distinct? parts)
  (let ([m (findf (tagged? 'return-mod) parts)]) (and m (equal? (cadr m) "distinct"))))
(define (lower-roa roa)
  (match roa
    [(list 'return-or-agg (list 'return-clause parts ...))
     (values (cql-return (return-mod-distinct? parts) (lower (last parts))) #f)]
    [(list 'return-or-agg (list 'aggregate-clause parts ...))
     (define name (unwrap-id (findf (tagged? 'identifier) parts)))
     (define starting (let ([s (findf (tagged? 'starting-clause) parts)]) (and s (match s [(list 'starting-clause e) (lower e)]))))
     (values #f (cql-agg (return-mod-distinct? parts) name starting (lower (last parts))))]))

(define (fold-binops acc rest)
  (match rest
    ['() acc]
    [(list-rest op operand more) (fold-binops (cql-binop (op->sym op) acc (lower operand)) more)]))

;; fold the comparison level: each tail is a plain comparison (op string), a timing-rel subtree, or a
;; between-tail. `<unit-word> between A and B` is a DURATION (context-sensitive, like the reference
;; grammar); any other `X between A and B` desugars to X >= A and X <= B.
(define between-units
  '(year years month months week weeks day days hour hours minute minutes second seconds millisecond milliseconds))
(define (fold-comp acc tails)
  (for/fold ([acc acc]) ([t (in-list tails)])
    (match t
      [(list 'comp-tail (? string? op) operand) (cql-binop (op->sym op) acc (lower operand))]
      [(list 'comp-tail (list 'timing-rel top operand))
       (let-values ([(op prec) (parse-timing-op top)]) (cql-timing op prec acc (lower operand)))]
      [(list 'comp-tail (list 'between-tail a b))
       (if (and (cql-ref? acc) (memq (cql-ref-name acc) between-units))
           (cql-between 'duration (cql-ref-name acc) (lower a) (lower b))
           (let ([lo (lower a)] [hi (lower b)])
             (cql-binop 'and (cql-binop '>= acc lo) (cql-binop '<= acc hi))))])))

;; interval/list prefix -> a system-operator call node
(define (interval-prefix->call kw operand per)
  (case kw
    [("start") (cql-call #f 'Start (list operand))]
    [("end") (cql-call #f 'End (list operand))]
    [("width") (cql-call #f 'Width (list operand))]
    [("point") (cql-call #f 'PointFrom (list operand))]
    [("singleton") (cql-call #f 'SingletonFrom (list operand))]
    [("collapse") (cql-call #f 'Collapse (list operand))]
    [("distinct") (cql-call #f 'Distinct (list operand))]
    [("flatten") (cql-call #f 'Flatten (list operand))]
    [("predecessor") (cql-call #f 'Predecessor (list operand))]
    [("successor") (cql-call #f 'Successor (list operand))]
    [("expand") (cql-call #f 'Expand (if per (list operand per) (list operand)))]
    [else (error 'cql-lower "unknown interval prefix ~a" kw)]))

;; parse-timing-op : datum -> (values op-symbol precision-symbol|#f).
;; Generic: collect the surfacing keyword strings + any precision, then map the keyword sequence.
(define timing-keys
  (hash '("before") 'before '("after") 'after
        '("same") 'same-as '("same" "before") 'same-or-before '("same" "after") 'same-or-after
        '("on" "before") 'same-or-before '("on" "after") 'same-or-after
        '("meets") 'meets '("meets" "before") 'meets-before '("meets" "after") 'meets-after
        '("overlaps") 'overlaps '("overlaps" "before") 'overlaps-before '("overlaps" "after") 'overlaps-after
        '("starts") 'starts '("ends") 'ends
        '("includes") 'includes '("properly" "includes") 'properly-includes
        '("included") 'included-in '("properly" "included") 'properly-included-in
        '("during") 'included-in '("properly" "during") 'properly-included-in))
(define (parse-timing-op t)
  (match t
    [(list 'timing-op parts ...)
     (define prec
       (cond [(findf (tagged? 'prec-of) parts) => po->prec]
             [(findf (tagged? 'prec) parts) => (lambda (p) (unwrap-id (cadr p)))]
             [else #f]))
     (define kws (filter string? parts))
     (values (hash-ref timing-keys kws (lambda () (error 'cql-lower "unknown timing op ~s" kws))) prec)]))
(define (po->prec po) (match po [(list 'prec-of (list 'prec p)) (unwrap-id p)]))

(define (fold-typeops acc tails)
  (for/fold ([acc acc]) ([t (in-list tails)])
    (match t
      [(list 'type-op-tail "is" "null") (cql-call #f 'IsNull (list acc))]
      [(list 'type-op-tail "is" "not" "null") (cql-unop 'not (cql-call #f 'IsNull (list acc)))]
      [(list 'type-op-tail op ts) (cql-typeop (op->sym op) acc (lower-type ts))])))

(define (apply-tail target tail)
  (match tail
    [(list 'postfix-tail (list 'invocation inner))
     (match inner
       [(list 'member idn) (cql-property target (unwrap-id idn))]
       [(list 'function-call name) (cql-call target (unwrap-id name) '())]
       [(list 'function-call name (list 'arg-list args ...)) (cql-call target (unwrap-id name) (map lower args))])]
    [(list 'postfix-tail (list 'indexer idx)) (cql-index target (lower idx))]))

;; case: children are an optional comparand expr, one+ case-item, an optional case-else
(define (lower-case parts)
  (define comparand (let ([e (findf (tagged? 'expr) parts)]) (and e (lower e))))
  (define items (for/list ([p (in-list parts)] #:when (tagged?* 'case-item p))
                  (match p [(list 'case-item w t) (cql-case-item (lower w) (lower t))])))
  (define els (let ([e (findf (tagged? 'case-else) parts)]) (and e (match e [(list 'case-else x) (lower x)]))))
  (cql-case comparand items els))

;; a typed-list-selector's optional list-body
(define (append-map-body parts body-tag)
  (define b (findf (tagged? body-tag) parts))
  (if b (match b [(list _ es ...) (map lower es)]) '()))

(define (tuple-elems parts)
  (define b (findf (tagged? 'tuple-body) parts))
  (if b (match b [(list 'tuple-body es ...) (map lower-tuple-elem es)]) '()))

(define (lower-tuple-elem e)
  (match e [(list 'tuple-element idn v) (cql-tuple-elem (unwrap-id idn) (lower v))]))

(define (lower-braced-body inner)
  (match inner
    [(list 'list-body es ...) (cql-list (map lower es))]
    [(list 'tuple-body es ...) (cql-tuple (map lower-tuple-elem es))]))

;; ---- type-spec lowering ----------------------------------------------------
(define (lower-type ts)
  (match ts
    [(list 'type-specifier inner) (lower-type inner)]
    [(list 'named-type qid) (named-type (lower-qualified qid))]
    [(list 'interval-type inner) (interval-type (lower-type inner))]
    [(list 'list-type inner) (list-type (lower-type inner))]
    [_ (error 'cql-lower "unexpected type-specifier: ~s" ts)]))

(define (lower-qualified qid)
  (match qid
    [(list 'qualified-id ids ...)
     (string->symbol (apply string-append (add-between (map (lambda (i) (symbol->string (unwrap-id i))) ids) ".")))]
    [_ (unwrap-id qid)]))

;; ---- literals --------------------------------------------------------------
(define (lower-quantity qparts)
  (match qparts
    [(list num) (num-value num)]
    [(list num (list 'unit u)) (qty-lit (num-value num) (unit->string u))]))

(define (num-value tok) (if (regexp-match? #rx"[.]" tok) (dec-lit tok) (string->number tok)))

(define (unit->string u)
  (cond [(string? u) u]
        [(and (pair? u) (eq? (car u) 'identifier)) (symbol->string (unwrap-id u))]
        [(symbol? u) (symbol->string u)]
        [else (format "~a" u)]))

(define (lit-value v)
  (cond
    [(or (dec-lit? v) (long-lit? v) (qty-lit? v) (date-lit? v) (datetime-lit? v) (time-lit? v)) v]
    [(string? v) v]                 ; STRING, or a LONGNUMBER handled below
    [(boolean? v) v]
    [(number? v) v]
    [else v]))

(module+ test
  (require rackunit)
  ;; NOTE: parsing is exercised in parse.rkt's tests; here we just sanity-check struct exports.
  (check-true (cql-lit? (cql-lit 1)))
  (check-equal? (cql-binop-op (cql-binop '+ (cql-lit 1) (cql-lit 2))) '+))
