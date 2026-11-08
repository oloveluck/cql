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
         (struct-out cql-code-def) (struct-out cql-concept-def) (struct-out cql-retrieve)
         (struct-out cql-relationship) (struct-out cql-sort-item) (struct-out cql-code-selector)
         (struct-out cql-concept-selector) (struct-out cql-timing-offset) (struct-out cql-within)
         (struct-out tuple-type) (struct-out choice-type)
         (struct-out cql-typeop) (struct-out cql-cast) (struct-out cql-convert) (struct-out cql-convert-unit)
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
(struct cql-parameter (name type default access)          #:prefab) ; type/default may be #f
(struct cql-codesystem (name id version access)           #:prefab)
(struct cql-valueset  (name id version codesystems access) #:prefab) ; codesystems: (listof term-ref)
(struct cql-code-def  (name id system display access)     #:prefab) ; system: term-ref ; display string|#f
(struct cql-concept-def (name codes display access)       #:prefab) ; codes: (listof term-ref)
(struct cql-context   (name)                              #:prefab)
(struct cql-def       (name access body)                  #:prefab) ; access: 'public|'private ; body: expr
(struct cql-func      (name access operands result-type body fluent?) #:prefab) ; body: expr | 'external
;; A term-ref (a reference to a codesystem/valueset/code/concept definition) is one of:
;;   (cql-ref name)                       — local
;;   (cql-property (cql-ref lib) name)    — Library.name
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
(struct cql-query   (sources lets where ret sort agg rels) #:prefab) ; sort 'asc|'ascending|'desc|'descending|(listof cql-sort-item)|#f
(struct cql-relationship (kind source such-that) #:prefab) ; kind 'with|'without ; source a cql-source
(struct cql-sort-item (expr direction) #:prefab)           ; direction 'asc|'ascending|'desc|'descending
;; [context -> Type: code-path comparator terminology] ; all but type may be #f
(struct cql-retrieve (context type code-path comparator terminology) #:prefab) ; type symbol ; code-path symbol
(struct cql-code-selector (code system display) #:prefab)   ; Code 'c' from "CS" display 'd'
(struct cql-concept-selector (codes display) #:prefab)      ; Concept { Code … , … } display 'd'
;; A 3 days or less before B ; rel a timing op symbol ; qual 'or-more|'or-less|'less-than|'more-than|#f
(struct cql-timing-offset (op precision quantity qual a b) #:prefab)
(struct cql-within (properly? quantity a b) #:prefab)       ; A within 3 days of B
(struct cql-source  (alias expr)   #:prefab)
(struct cql-let-def (name expr)    #:prefab)
(struct cql-return  (distinct? expr) #:prefab)
(struct cql-agg     (distinct? name starting body) #:prefab)
(struct cql-minmax  (which type) #:prefab)          ; which 'min|'max ; type a type-spec node
(struct cql-instance (type elements) #:prefab)      ; Code{…}/Concept{…}/Quantity{…}/Model.Type{…} selector
(struct cql-typeop    (op operand type) #:prefab) ; op 'is|'as ; type a type-spec node
(struct cql-cast      (operand type)  #:prefab)
(struct cql-convert   (operand type)  #:prefab)
(struct cql-convert-unit (operand unit) #:prefab) ; convert X to 'mg' / to days — unit: a UCUM string

;; type specifiers
(struct named-type    (name)          #:prefab) ; name symbol (qualified via ".")
(struct interval-type (element)       #:prefab)
(struct list-type     (element)       #:prefab)
(struct tuple-type    (elements)      #:prefab) ; (listof (cons symbol type-spec))
(struct choice-type   (types)         #:prefab)

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
    [(list 'kw-id a) (id->sym a)]
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
    [(list 'parameter-def parts ...)
     (define rest (drop-access parts))
     (cql-parameter (unwrap-id (car rest))
                    (let ([t (findf (tagged? 'type-specifier) rest)]) (and t (lower-type t)))
                    (let ([d (findf (tagged? 'param-default) rest)])
                      (and d (match d [(list 'param-default e) (lower e)])))
                    (access-of parts))]
    [(list 'codesystem-def parts ...)
     (match (drop-access parts)
       [(list idn cid rest ...) (cql-codesystem (unwrap-id idn) cid (find-version rest) (access-of parts))])]
    [(list 'valueset-def parts ...)
     (match (drop-access parts)
       [(list idn vid rest ...)
        (cql-valueset (unwrap-id idn) vid (find-version rest)
                      (let ([c (findf (tagged? 'codesystems-spec) rest)]) (if c (map lower-term-ref (cdr c)) '()))
                      (access-of parts))])]
    [(list 'code-def parts ...)
     (match (drop-access parts)
       [(list idn cid tr rest ...)
        (cql-code-def (unwrap-id idn) cid (lower-term-ref tr) (find-display rest) (access-of parts))])]
    [(list 'concept-def parts ...)
     (match (drop-access parts)
       [(list idn rest ...)
        (cql-concept-def (unwrap-id idn) (map lower-term-ref (filter (tagged? 'term-ref) rest))
                         (find-display rest) (access-of parts))])]
    [(list 'context-def idn) (cql-context (unwrap-id idn))]
    [(list 'context-def _model idn) (cql-context (unwrap-id idn))]
    [(list 'statement-def inner) (lower-definition inner)]
    [(list 'expression-def parts ...) (lower-expression-def parts)]
    [(list 'function-def parts ...) (lower-function-def parts)]
    [_ (error 'cql-lower "unexpected definition: ~s" d)]))

(define (drop-access parts) (filter (lambda (p) (not ((tagged? 'access-modifier) p))) parts))
(define (find-display parts)
  (define d (findf (tagged? 'display-clause) parts))
  (and d (cadr d)))
;; lower-term-ref : datum -> term-ref
(define (lower-term-ref t)
  (match t
    [(list 'term-ref idn) (cql-ref (unwrap-id idn))]
    [(list 'term-ref lib idn) (cql-property (cql-ref (unwrap-id lib)) (unwrap-id idn))]))

(define (access-of parts)
  (define a (findf (tagged? 'access-modifier) parts))
  (if a (match a [(list 'access-modifier m) (string->symbol m)]) 'public))

(define (lower-expression-def parts)
  ;; parts: access-modifier? identifier expr
  (define name (unwrap-id (findf (tagged? 'identifier) parts)))
  (define body (lower (findf (tagged? 'expr) parts)))
  (cql-def name (access-of parts) body))

(define (lower-function-def parts)
  (define name (unwrap-id (findf (lambda (p) (or ((tagged? 'identifier) p) ((tagged? 'kw-id) p))) parts)))
  (define ops (let ([ol (findf (tagged? 'operand-list) parts)])
                (if ol (lower-operand-list ol) '())))
  (define rt (let ([r (findf (tagged? 'returns-spec) parts)])
               (and r (match r [(list 'returns-spec ts) (lower-type ts)]))))
  (define body (let ([b (findf (tagged? 'func-body) parts)])
                 (match b
                   [(list 'func-body "external") 'external]
                   [(list 'func-body e) (lower e)]
                   [_ 'external])))
  (cql-func name (access-of parts) ops rt body (and (findf (tagged? 'fluent) parts) #t)))

(define (lower-operand-list ol)
  (match ol
    [(list 'operand-list ops ...)
     (map (lambda (o) (match o [(list 'operand idn ts) (cql-operand (unwrap-id idn) (lower-type ts))])) ops)]))

;; ---- expression lowering ---------------------------------------------------
(define (lower datum)
  (match datum
    [(list 'expr e) (lower e)]
    [(list 'query parts ...) (lower-query parts)]
    [(list 'bare-query source) (lower-query (list source))]

    ;; binary levels: operand (op operand)* — left associative
    [(list (or 'implies-expr 'or-expr 'and-expr 'membership-expr 'equality-expr
               'setop-expr 'additive-expr 'multiplicative-expr 'power-expr
               'implies-open 'or-open 'and-open 'membership-open 'equality-open
               'setop-open 'additive-open 'multiplicative-open 'power-open)
           first rest ...)
     (fold-binops (lower first) rest)]

    ;; comparison level also carries timing operators (before/after/same-as/…), possibly precision-qualified
    [(list (or 'comparison-expr 'comparison-open) first tails ...) (fold-comp (lower first) (map closed-tail tails))]

    ;; type test / cast / convert
    [(list 'type-expr operand) (lower operand)]
    [(list 'type-expr operand tails ...) (fold-typeops (lower operand) tails)]
    [(list 'cast-expr operand ts) (cql-cast (lower operand) (lower-type ts))]
    [(list 'convert-expr operand (? string? unit)) (cql-convert-unit (lower operand) unit)]
    [(list 'convert-expr operand ts)
     ;; a calendar word is a unit (cql.g4's `unit: dateTimePrecision | pluralDateTimePrecision | STRING`),
     ;; written as its UCUM unit (the reference translator: `to days` -> 'd')
     (match ts
       [(list 'type-specifier (list 'named-type (list 'qualified-id (and idn (list 'identifier _)))))
        #:when (hash-ref calendar-ucum (unwrap-id idn) #f)
        (cql-convert-unit (lower operand) (hash-ref calendar-ucum (unwrap-id idn)))]
       [_ (cql-convert (lower operand) (lower-type ts))])]

    ;; unary
    [(list 'unary-expr operand) (lower operand)]
    [(list 'unary-expr (? string? op) operand) (cql-unop (op->sym op) (lower operand))]
    [(list 'unary-open operand) (lower operand)]
    [(list 'unary-open (? string? op) operand) (cql-unop (op->sym op) (lower operand))]
    [(list 'interval-prefix-open kw operand) (interval-prefix->call kw (lower operand) #f)]

    ;; interval/list prefix operators -> system operator calls (reuse the call machinery)
    [(list 'interval-prefix kw operand) (interval-prefix->call kw (lower operand) #f)]
    [(list 'interval-prefix kw operand per) (interval-prefix->call kw (lower operand) (lower per))]
    [(list 'aliased-set-op kw source) (interval-prefix->call kw (lower-query (list source)) #f)]

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
    [(list 'braced ":") (cql-tuple '())]
    [(list 'braced (list 'braced-body inner)) (lower-braced-body inner)]

    ;; calls / refs
    [(list 'function-call name) (cql-call #f (unwrap-id name) '())]
    [(list 'function-call name (list 'arg-list args ...)) (cql-call #f (unwrap-id name) (map lower args))]
    [(list 'member idn) (cql-ref (unwrap-id idn))]
    [(list 'component-from idn operand) (cql-component (unwrap-id idn) (lower operand))]
    [(list 'minmax which ts) (cql-minmax (if (equal? which "minimum") 'min 'max) (lower-type ts))]
    [(list 'retrieve parts ...) (lower-retrieve parts)]
    [(list 'code-selector code tr rest ...) (cql-code-selector code (lower-term-ref tr) (find-display rest))]
    [(list 'concept-selector parts ...)
     (cql-concept-selector (map lower (filter (tagged? 'code-selector) parts)) (find-display parts))]
    [(list 'qualified-source first more ...)
     (for/fold ([acc (cql-ref (unwrap-id first))]) ([m (in-list more)]) (cql-property acc (unwrap-id m)))]
    [(list 'query-source e) (lower e)]
    [(list 'instance-selector head parts ...)
     (cql-instance (if (string? head) (string->symbol head) (lower-qualified head))
                   (let ([b (findf (tagged? 'tuple-body) parts)])
                     (if b (match b [(list 'tuple-body es ...) (map lower-tuple-elem es)]) '())))]
    [(list 'duration-of units operand)
     ;; duration in U of X  ==  U between start of X and end of X
     (let ([x (lower operand)])
       (cql-between 'duration (unwrap-id units)
                    (cql-call #f 'Start (list x)) (cql-call #f 'End (list x))))]
    [(list 'duration-between units a b) (cql-between 'duration (unwrap-id units) (lower a) (lower b))]
    [(list 'difference-between units a b) (cql-between 'difference (unwrap-id units) (lower a) (lower b))]

    ;; leaves
    [(list 'literal "null") (cql-lit 'null)]
    [(list 'literal (list 'ratio (list 'quantity n ...) (list 'quantity d ...)))
     (cql-lit (ratio-lit (lower-quantity n) (lower-quantity d)))]
    [(list 'literal (list 'quantity qparts ...)) (cql-lit (lower-quantity qparts))]
    [(list 'literal v) (cql-lit (lit-value v))]
    [(list 'identifier x) (cql-ref (id->sym x))]

    [_ (error 'cql-lower "unexpected node: ~s" datum)]))

;; the UCUM unit of a calendar word in `convert X to <word>`
(define calendar-ucum
  (hasheq 'year "a" 'years "a" 'month "mo" 'months "mo" 'week "wk" 'weeks "wk" 'day "d" 'days "d"
          'hour "h" 'hours "h" 'minute "min" 'minutes "min" 'second "s" 'seconds "s"
          'millisecond "ms" 'milliseconds "ms"))

;; ---- query lowering --------------------------------------------------------
(define (lower-query parts)
  (define sources (for/list ([p (in-list parts)] #:when (tagged?* 'source-clause p)) (lower-source p)))
  (define tail (findf (tagged? 'query-tail) parts))
  (define-values (lets where ret sort agg) (lower-tail tail))
  (define rels (map lower-relationship (filter (tagged? 'inclusion-clause) (if tail (cdr tail) '()))))
  (cql-query sources lets where ret sort agg rels))
(define (lower-source p)
  (match p [(list 'source-clause e (list 'alias idn)) (cql-source (unwrap-id idn) (lower e))]))

;; lower-retrieve : (listof datum) -> cql-retrieve
(define (lower-retrieve parts)
  (define ctx (let ([c (findf (tagged? 'retrieve-context) parts)]) (and c (lower (cadr c)))))
  (define type (match (findf (tagged? 'named-type) parts) [(list 'named-type q) (lower-named q)]))
  (define filt (findf (tagged? 'retrieve-filter) parts))
  (match filt
    [#f (cql-retrieve ctx type #f #f #f)]
    [(list 'retrieve-filter (list 'terminology t)) (cql-retrieve ctx type #f #f (lower t))]
    [(list 'retrieve-filter (list 'code-path ids ...) (list 'code-comparator cmp) (list 'terminology t))
     (cql-retrieve ctx type
                   (string->symbol (apply string-append (add-between (map (compose1 symbol->string unwrap-id) ids) ".")))
                   (string->symbol cmp) (lower t))]))
(define (compose1 f g) (lambda (x) (f (g x))))

(define (lower-relationship r)
  (match r
    [(list 'inclusion-clause (list (and kind (or 'with-clause 'without-clause)) src e))
     (cql-relationship (if (eq? kind 'with-clause) 'with 'without) (lower-source src) (lower e))]))
(define (lower-sort-item it)
  (match it
    [(list 'sort-item e) (cql-sort-item (lower e) 'asc)]
    [(list 'sort-item e (list 'sort-dir d)) (cql-sort-item (lower e) (dir->sym d))]))
;; the word as written: ELM's SortDirection has all four, and the reference keeps the spelling
(define (dir->sym d) (string->symbol d))
(define (lower-tail tail)
  (define parts (if tail (cdr tail) '()))
  (define lets (append-map (lambda (lc) (match lc [(list 'let-clause items ...) (map lower-let-item items)] [_ '()]))
                           (filter (tagged? 'let-clause) parts)))
  (define where (let ([w (findf (tagged? 'where-clause) parts)]) (and w (match w [(list 'where-clause e) (lower e)]))))
  (define roa (findf (tagged? 'return-or-agg) parts))
  (define-values (ret agg) (if roa (lower-roa roa) (values #f #f)))
  (define sort (let ([s (findf (tagged? 'sort-clause) parts)])
                 (and s (match s
                          [(list 'sort-clause (list 'sort-dir d)) (dir->sym d)]
                          [(list 'sort-clause items ...) (map lower-sort-item items)]))))
  (values lets where ret sort agg))
(define (lower-let-item li) (match li [(list 'let-item idn e) (cql-let-def (unwrap-id idn) (lower e))]))
(define (return-mod-distinct? parts)
  (let ([m (findf (tagged? 'return-mod) parts)]) (and m (equal? (cadr m) "distinct"))))
;; return-mod-of : parts -> #t (distinct) | 'all | #f (unstated)
(define (return-mod-of parts)
  (let ([m (findf (tagged? 'return-mod) parts)])
    (cond [(not m) #f] [(equal? (cadr m) "distinct") #t] [else 'all])))
(define (lower-roa roa)
  (match roa
    [(list 'return-or-agg (list 'return-clause parts ...))
     (values (cql-return (return-mod-of parts) (lower (last parts))) #f)]
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
;; closed-tail : datum -> datum   an open comparison tail (its operand ends in an open term) in the
;; closed tail's shape
(define (closed-tail t)
  (match t
    [(list 'comp-tail-open (list 'timing-rel-open top operand)) (list 'comp-tail (list 'timing-rel top operand))]
    [(list 'comp-tail-open op operand) (list 'comp-tail op operand)]
    [_ t]))
(define (fold-comp acc tails)
  (for/fold ([acc acc]) ([t (in-list tails)])
    (match t
      [(list 'comp-tail (? string? op) operand) (cql-binop (op->sym op) acc (lower operand))]
      [(list 'comp-tail (list 'timing-rel top operand)) (lower-timing top acc (lower operand))]
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
    [("collapse") (cql-call #f 'Collapse (if per (list operand per) (list operand)))]
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
        '("during") 'included-in '("properly" "during") 'properly-included-in
        '("within") 'within '("properly" "within") 'within
        '("in") 'included-in '("contains") 'includes
        '("before" "on") 'same-or-before '("after" "on") 'same-or-after))
;; lower-timing : timing-op-datum cql-expr cql-expr -> cql-expr
;; `starts`/`ends` before the phrase take Start/End of the left operand, `start`/`end` after it of the
;; right one (as the reference translator does); a quantity offset or `within` makes a richer node.
(define (lower-timing top a b)
  (define parts (cdr top))
  (define (bound tag x)
    (match (findf (tagged? tag) parts)
      [(list _ "starts") (cql-call #f 'Start (list x))] [(list _ "start") (cql-call #f 'Start (list x))]
      [(list _ "ends") (cql-call #f 'End (list x))] [(list _ "end") (cql-call #f 'End (list x))]
      [_ x]))
  (define a* (bound 'timing-lead a))
  (define b* (bound 'timing-trail b))
  (define-values (op prec) (parse-timing-op top))
  (define offset (findf (tagged? 'quantity-offset) parts))
  (define within-q (findf (tagged? 'quantity) parts))
  (cond
    [(eq? op 'within) (cql-within (and (member "properly" parts) #t) (lower-quantity (cdr within-q)) a* b*)]
    [offset
     (match offset
       [(list 'quantity-offset (list 'quantity q ...)) (cql-timing-offset op prec (lower-quantity q) #f a* b*)]
       [(list 'quantity-offset (list 'quantity q ...) (list 'offset-qual s))
        (cql-timing-offset op prec (lower-quantity q) (if (equal? s "or more") 'or-more 'or-less) a* b*)]
       [(list 'quantity-offset (list 'excl-qual s) (list 'quantity q ...))
        (cql-timing-offset op prec (lower-quantity q) (if (equal? s "less than") 'less-than 'more-than) a* b*)])]
    [else (cql-timing op prec a* b*)]))
(define (parse-timing-op t)
  (match t
    [(list 'timing-op parts ...)
     (define prec
       (cond [(findf (tagged? 'prec-of) parts) => po->prec]
             [(findf (tagged? 'prec) parts) => (lambda (p) (unwrap-id (cadr p)))]
             [else #f]))
     (define kws (apply append (for/list ([p (in-list parts)])
                            (match p
                              [(? string? s) (list s)]
                              [(list 'same-tail s ...) s]
                              [_ '()]))))
     (values (hash-ref timing-keys kws (lambda () (error 'cql-lower "unknown timing op ~s" kws))) prec)]))
(define (po->prec po) (match po [(list 'prec-of (list 'prec p)) (unwrap-id p)]))

(define (fold-typeops acc tails)
  (for/fold ([acc acc]) ([t (in-list tails)])
    (match t
      [(list 'type-op-tail "is" "null") (cql-call #f 'IsNull (list acc))]
      [(list 'type-op-tail "is" "not" "null") (cql-unop 'not (cql-call #f 'IsNull (list acc)))]
      [(list 'type-op-tail "is" (? boolean? b)) (cql-call #f (if b 'IsTrue 'IsFalse) (list acc))]
      [(list 'type-op-tail "is" "not" (? boolean? b)) (cql-unop 'not (cql-call #f (if b 'IsTrue 'IsFalse) (list acc)))]
      [(list 'type-op-tail op ts) (cql-typeop (op->sym op) acc (lower-type ts))])))

(define (apply-tail target tail)
  (match tail
    [(list 'postfix-tail (list 'invocation inner))
     (match inner
       [(list (or 'member 'dot-member) idn) (cql-property target (unwrap-id idn))]
       [(list (or 'function-call 'dot-call) name) (cql-call target (unwrap-id name) '())]
       [(list (or 'function-call 'dot-call) name (list 'arg-list args ...)) (cql-call target (unwrap-id name) (map lower args))])]
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
    [(list 'named-type qid) (named-type (lower-named qid))]
    [(list 'tuple-type elts ...)
     (tuple-type (for/list ([e (in-list elts)]) (match e [(list 'tuple-type-elt n t) (cons (unwrap-id n) (lower-type t))])))]
    [(list 'choice-type ts ...) (choice-type (map lower-type ts))]
    [(list 'interval-type inner) (interval-type (lower-type inner))]
    [(list 'list-type inner) (list-type (lower-type inner))]
    [_ (error 'cql-lower "unexpected type-specifier: ~s" ts)]))

(define (lower-named q) (if (string? q) (string->symbol q) (lower-qualified q)))
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
