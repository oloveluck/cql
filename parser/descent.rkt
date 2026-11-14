#lang racket/base

;;; descent.rkt — CQL read by recursive descent: tokens -> the datum brag's parser gives, or #f.
;;;
;;; grammar.rkt (brag, a port of cql.g4) is the definition of CQL text and stays so: it reports
;;; every syntax error, and tests/descent-test.rkt holds this reader to it — the same datum, over
;;; the HL7 suites, every vendored library, and generated text. What this reader adds is speed:
;;; brag's general parser costs ~60 µs a token and is quadratic in a long list; this one reads
;;; the grammar's rules directly, a token at a time, and produces the very datum
;;; `g:parse-to-datum` would (rule names as heads, `/` cuts left out, `@` splices spliced), so the
;;; lowering (ast.rkt), the caches and the definition splitting of parse.rkt are unchanged.
;;;
;;; It only ever ACCEPTS. On any token it does not expect it answers #f, and parse.rkt hands the
;;; same tokens to brag, whose answer — a datum, or the positioned error — is the one given. The
;;; grammar reads a few texts two ways, and brag's choice among readings is its own; this reader
;;; leaves every such text to brag (each refusal below says which reading it avoids):
;;;   - a query (or `from` query) followed by a clause keyword (let with without where return
;;;     aggregate sort) where an enclosing query's tail could take that clause too: `… where
;;;     exists [A] a return x` (the context flag `tail?`);
;;;   - a comma that could separate either a query's own items (let items, sort items, `from`
;;;     sources) or the enclosing list's elements, arguments or tuple elements (the flag
;;;     `comma`: what the enclosing brackets separate);
;;;   - `distinct`/`flatten` before a bare query (interval-prefix or aliased-set-op);
;;;   - `return distinct X` (a return-mod, or Distinct of X);
;;;   - a number followed by `as`/`is`/`in`/`contains`/`code`/`concept`, unless an operand follows
;;;     the keyword (a unit named `in`, or the operator);
;;;   - `in`/`contains` followed by `+`/`-` (`[A] in - 1`: `in` an alias, or membership of -1);
;;;   - a code selector's system followed by `.` (`Code 'x' from A.B`: the system `A.B`, or `.B`
;;;     of the selector), and `minimum`/`maximum` of a dotted type (`minimum System.Decimal`);
;;;   - `duration`/`difference in … between A and B` with an operator after B (`… and B is null`:
;;;     the operator is B's, or applies to the whole term);
;;;   - `starts`/`ends` before a quantity offset without `or less`/`or more` (`A ends 1 hour
;;;     before B`: a lead and an offset, or `ends` of 1 hour then `before B`).
;;; In an expression, the keywords that are also identifiers (`as` `is` `in` `contains`) are read
;;; as names only after `.`, as tuple keys and in a retrieve's code path; `code` and `concept` are
;;; names wherever an identifier is.
;;;
;;; A Token is a brag token-struct; its type is a symbol: a named token (NUMBER STRING BOOL
;;; IDENTIFIER DATE DATETIME TIME LONGNUMBER) or the text of a keyword/operator (`and`, `|.|`,
;;; `|such that|`), its value what the datum holds (a lexeme string, a symbol, a literal struct).
;;; A Datum is brag's parse-to-datum shape: (rule-name child …), children datums or token values.
;;; A Cursor is (cursor tokens i tail? comma): the text's tokens (spaces and comments left out),
;;; the index of the next one, and two context flags (above); reading advances it by mutation,
;;; local to one parse.
;;; A Reading is (values Datum Boolean): a datum and whether it is OPEN — the grammar's -open
;;; forms, an expression whose last operand is a query or an if-then-else (grammar.rkt says why).

(require racket/contract racket/match
         (only-in brag/support token-struct-type token-struct-val))

(provide
 (contract-out
  ;; descend-library : (listof Token) -> Datum | #f   (cql-library …), or #f when brag reads it
  [descend-library (-> list? (or/c #f pair?))]))

;; A Comma is #f (no enclosing comma), 'list (elements, arguments: expressions) or 'keyed (tuple
;; elements, let items: `name: expression`) — what a comma separates where this is read
(struct cursor (tokens [i #:mutable] [tail? #:mutable] [comma #:mutable]))

;; ---- the cursor ---------------------------------------------------------------------------------
;; peek-kind : Cursor [Nat] -> Symbol|#f   the type of the token `ahead` places on
(define (peek-kind c [ahead 0])
  (define j (+ (cursor-i c) ahead))
  (define v (cursor-tokens c))
  (and (< j (vector-length v)) (token-struct-type (vector-ref v j))))
;; next! : Cursor -> Token   reads one (the text ending here is not ours to say)
(define (next! c)
  (define v (cursor-tokens c))
  (define i (cursor-i c))
  (unless (< i (vector-length v)) (refuse))
  (set-cursor-i! c (add1 i))
  (vector-ref v i))
;; take! : Cursor -> Any   reads one, its value
(define (take! c) (token-struct-val (next! c)))
;; at? : Cursor (listof Symbol)|Symbol -> Boolean   the next token is of one of these types
(define (at? c kinds)
  (define k (peek-kind c))
  (and k (if (symbol? kinds) (eq? k kinds) (memq k kinds) ) #t))
;; expect! : Cursor Symbol -> Any   reads a token of this type, its value; else refuses
(define (expect! c k) (if (eq? (peek-kind c) k) (take! c) (refuse)))
;; skip! : Cursor Symbol -> Void   reads a token of this type (a `/` cut: not in the datum)
(define (skip! c k) (void (expect! c k)))

;; refused: the escape when the text is not what this reader takes
(struct refusal ())
(define the-refusal (refusal))
(define (refuse) (raise the-refusal #t))

;; with-context : Cursor Boolean Comma (-> X) -> X   reads with the flags set, then restores them
(define (with-context c tail? comma read)
  (define t (cursor-tail? c)) (define m (cursor-comma c))
  (set-cursor-tail?! c tail?) (set-cursor-comma! c comma)
  (begin0 (read) (set-cursor-tail?! c t) (set-cursor-comma! c m)))
;; enclosed : Cursor (-> X) -> X   inside brackets or before a closing keyword: no enclosing query
;; or list can continue after what is read
(define (enclosed c read) (with-context c #f #f read))
;; listed : Cursor (-> X) -> X   an element of a comma-separated list inside brackets
(define (listed c read) (with-context c #f 'list read))
;; keyed : Cursor (-> X) -> X   the value of a tuple element inside braces
(define (keyed c read) (with-context c #f 'keyed read))

;; own-comma? : Cursor Boolean -> Boolean   at a comma inside a query (between `from` sources, let
;; items, sort items): whether it is the query's rather than the enclosing construct's, given
;; whether the query's items are `name: expression` (let items); refuses when either could take it
(define (own-comma? c keyed-item?)
  (define keyed-next? (and (ref-id-kind? (peek-kind c 1)) (eq? (peek-kind c 2) ':)))
  (case (cursor-comma c)
    [(#f) #t]
    [(list) (if keyed-item? keyed-next? (refuse))]       ; an element is no `name:`
    [(keyed) (if keyed-next? (if keyed-item? (refuse) #f) (not keyed-item?))]))

;; optional : Boolean (-> Datum) -> (listof Datum)   the datum when the condition holds, else none
(define (optional present? read) (if present? (list (read)) '()))

;; ---- names --------------------------------------------------------------------------------------
;; identifier : IDENTIFIER | "as" | "is" | "in" | "contains" | "code" | "concept"
(define identifier-kinds '(IDENTIFIER as is in contains code concept))
;; the identifiers read as names in an expression (the other four are left to brag there)
(define name-kinds '(IDENTIFIER code concept))
;; kw-id: the keywords a name may be after "." and as a tuple key
(define kw-id-kinds
  '(start end display version by Code Concept sort context default occurs starts ends expand width point
    fluent library parameter valueset codesystem codesystems called asc desc ascending descending convert
    private public singleton collapse distinct flatten all Choice with without where return exists minimum
    maximum predecessor successor))
(define (ref-id-kind? k) (and k (or (memq k identifier-kinds) (memq k kw-id-kinds)) #t))

;; identifier : Cursor -> Datum   (identifier v), any of the grammar's identifier tokens
(define (identifier c)
  (if (at? c identifier-kinds) `(identifier ,(take! c)) (refuse)))
;; name : Cursor -> Datum   (identifier v) where an expression takes it as a name
(define (name c)
  (if (at? c name-kinds) `(identifier ,(take! c)) (refuse)))
;; ref-id : Cursor -> Datum   @ref-id : identifier | kw-id (spliced: the datum is one of the two)
(define (ref-id c)
  (cond [(at? c identifier-kinds) `(identifier ,(take! c))]
        [(at? c kw-id-kinds) `(kw-id ,(take! c))]
        [else (refuse)]))
;; qualified-id : identifier (/"." @ref-id)*
(define (qualified-id c)
  (define first (identifier c))
  `(qualified-id ,first ,@(dotted-ref-ids c)))
;; dotted-ref-ids : Cursor -> (listof Datum)   (/"." @ref-id)*
(define (dotted-ref-ids c)
  (if (at? c '|.|) (begin (next! c) (let ([r (ref-id c)]) (cons r (dotted-ref-ids c)))) '()))

;; ---- the library --------------------------------------------------------------------------------
;; cql-library : library-header? definition*
(define (descend-library tokens)
  (with-handlers ([refusal? (lambda (_) #f)])
    (define c (cursor (list->vector tokens) 0 #f #f))
    (define header (optional (at? c 'library) (lambda () (library-header c))))
    (define defs (let loop () (if (peek-kind c) (cons (definition c) (loop)) '())))
    `(cql-library ,@header ,@defs)))

;; library-header : /"library" qualified-id version-spec?
(define (library-header c)
  (skip! c 'library)
  (define q (qualified-id c))
  `(library-header ,q ,@(version-spec? c)))
;; version-spec? : Cursor -> (listof Datum)   (version-spec STRING), when `version` is next
(define (version-spec? c)
  (optional (at? c 'version) (lambda () (skip! c 'version) `(version-spec ,(expect! c 'STRING)))))
;; called-spec? : (called-spec identifier), when `called` is next
(define (called-spec? c)
  (optional (at? c 'called) (lambda () (skip! c 'called) `(called-spec ,(identifier c)))))
;; display-clause? : (display-clause STRING), when `display` is next
(define (display-clause? c)
  (optional (at? c 'display) (lambda () (skip! c 'display) `(display-clause ,(expect! c 'STRING)))))

;; definition : using-def | include-def | parameter-def | codesystem-def | valueset-def | code-def
;;            | concept-def | context-def | statement-def
(define (definition c)
  (define access (optional (at? c '(public private)) (lambda () `(access-modifier ,(take! c)))))
  (define (unmodified read) (if (null? access) (read) (refuse)))
  `(definition
     ,(case (peek-kind c)
        [(using) (unmodified (lambda () (using-or-include c 'using 'using-def)))]
        [(include) (unmodified (lambda () (using-or-include c 'include 'include-def)))]
        [(context) (unmodified (lambda () (context-def c)))]
        [(define) (unmodified (lambda () `(statement-def ,(statement-def c))))]
        [(parameter) (parameter-def c access)]
        [(codesystem) (codesystem-def c access)]
        [(valueset) (valueset-def c access)]
        [(code) (code-def c access)]
        [(concept) (concept-def c access)]
        [else (refuse)])))

;; using-def / include-def : /keyword qualified-id version-spec? called-spec?
(define (using-or-include c keyword tag)
  (skip! c keyword)
  (define q (qualified-id c))
  (define v (version-spec? c))
  `(,tag ,q ,@v ,@(called-spec? c)))
;; context-def : /"context" identifier | /"context" identifier /"." identifier
(define (context-def c)
  (skip! c 'context)
  (define a (identifier c))
  `(context-def ,a ,@(optional (at? c '|.|) (lambda () (next! c) (identifier c)))))
;; parameter-def : access-modifier? /"parameter" identifier type-specifier? param-default?
(define (parameter-def c access)
  (skip! c 'parameter)
  (define n (identifier c))
  (define t (optional (type-start? c) (lambda () (type-specifier c))))
  (define d (optional (at? c 'default)
                      (lambda () (skip! c 'default) `(param-default ,(enclosed c (lambda () (expr c)))))))
  `(parameter-def ,@access ,n ,@t ,@d))
;; codesystem-def : access-modifier? /"codesystem" identifier /":" STRING version-spec?
(define (codesystem-def c access)
  (skip! c 'codesystem)
  (define n (identifier c))
  (skip! c ':)
  (define id (expect! c 'STRING))
  `(codesystem-def ,@access ,n ,id ,@(version-spec? c)))
;; valueset-def : access-modifier? /"valueset" identifier /":" STRING version-spec? codesystems-spec?
;; codesystems-spec : /"codesystems" /"{" term-ref (/"," term-ref)* /"}"
(define (valueset-def c access)
  (skip! c 'valueset)
  (define n (identifier c))
  (skip! c ':)
  (define id (expect! c 'STRING))
  (define v (version-spec? c))
  (define cs (optional (at? c 'codesystems)
                       (lambda () (skip! c 'codesystems) (skip! c '|{|)
                         `(codesystems-spec ,@(separated c '|,| term-ref '|}|)))))
  `(valueset-def ,@access ,n ,id ,@v ,@cs))
;; code-def : access-modifier? /"code" identifier /":" STRING /"from" term-ref display-clause?
(define (code-def c access)
  (skip! c 'code)
  (define n (identifier c))
  (skip! c ':)
  (define id (expect! c 'STRING))
  (skip! c 'from)
  (define tr (term-ref c))
  `(code-def ,@access ,n ,id ,tr ,@(display-clause? c)))
;; concept-def : access-modifier? /"concept" identifier /":" /"{" term-ref (/"," term-ref)* /"}" display-clause?
(define (concept-def c access)
  (skip! c 'concept)
  (define n (identifier c))
  (skip! c ':) (skip! c '|{|)
  (define trs (separated c '|,| term-ref '|}|))
  `(concept-def ,@access ,n ,@trs ,@(display-clause? c)))
;; term-ref : identifier | identifier /"." identifier
(define (term-ref c)
  (define a (identifier c))
  `(term-ref ,a ,@(optional (at? c '|.|) (lambda () (next! c) (identifier c)))))
;; separated : Cursor Symbol (Cursor -> Datum) Symbol -> (listof Datum)   item (sep item)* close
(define (separated c sep item close)
  (let loop ([acc (list (item c))])
    (cond [(at? c sep) (next! c) (loop (cons (item c) acc))]
          [else (skip! c close) (reverse acc)])))

;; statement-def : expression-def | function-def
;; expression-def : /"define" access-modifier? identifier /":" expr
;; function-def   : /"define" access-modifier? fluent? /"function" @ref-id /"(" operand-list? /")"
;;                  returns-spec? /":" func-body
(define (statement-def c)
  (skip! c 'define)
  (define access (optional (at? c '(public private)) (lambda () `(access-modifier ,(take! c)))))
  (cond
    [(at? c '(fluent function))
     (define fluent (optional (at? c 'fluent) (lambda () `(fluent ,(take! c)))))
     (skip! c 'function)
     (define n (ref-id c))
     (skip! c '|(|)
     (define ops (optional (not (at? c '|)|))
                           (lambda () `(operand-list ,@(separated c '|,| operand '|)|)))))
     (when (null? ops) (skip! c '|)|))
     (define returns (optional (at? c 'returns) (lambda () (skip! c 'returns) `(returns-spec ,(type-specifier c)))))
     (skip! c ':)
     (define body (if (at? c 'external) `(func-body ,(take! c)) `(func-body ,(enclosed c (lambda () (expr c))))))
     `(function-def ,@access ,@fluent ,n ,@ops ,@returns ,body)]
    [else
     (define n (identifier c))
     (skip! c ':)
     `(expression-def ,@access ,n ,(enclosed c (lambda () (expr c))))]))
;; operand : identifier type-specifier
(define (operand c)
  (define n (identifier c))
  `(operand ,n ,(type-specifier c)))

;; ---- type specifiers ----------------------------------------------------------------------------
;; type-start? : Cursor -> Boolean   a type specifier begins here
(define (type-start? c) (at? c '(IDENTIFIER code concept Interval List Tuple Choice Code Concept)))
;; type-specifier : interval-type | list-type | named-type | tuple-type | choice-type
(define (type-specifier c)
  `(type-specifier
    ,(case (peek-kind c)
       [(Interval) (next! c) `(interval-type ,(type-argument c))]
       [(List) (next! c) `(list-type ,(type-argument c))]
       ;; tuple-type : /"Tuple" /"{" tuple-type-elt (/"," tuple-type-elt)* /"}"
       [(Tuple) (next! c) (skip! c '|{|)
                `(tuple-type ,@(separated c '|,| (lambda (c) (let ([n (ref-id c)]) `(tuple-type-elt ,n ,(type-specifier c)))) '|}|))]
       ;; choice-type : /"Choice" /"<" type-specifier (/"," type-specifier)* /">"
       [(Choice) (next! c) (skip! c '<) `(choice-type ,@(separated c '|,| type-specifier '>))]
       [else (named-type c)])))
;; type-argument : /"<" type-specifier /">"
(define (type-argument c)
  (skip! c '<)
  (begin0 (type-specifier c) (skip! c '>)))
;; named-type : qualified-id | "Code" | "Concept"
(define (named-type c)
  (cond [(at? c '(Code Concept)) `(named-type ,(take! c))]
        [(at? c name-kinds) `(named-type ,(qualified-id c))]
        [else (refuse)]))

;; ---- the precedence cascade ---------------------------------------------------------------------
;; expr : implies-expr | implies-open
(define (expr c)
  (define-values (d _open?) (implies-level c))
  `(expr ,d))

;; level : Cursor Symbol Symbol (listof Symbol) (Cursor -> Reading) -> Reading
;; tag : operand (@op operand)*   and   tag-open : (operand @op)* operand-open
(define (level c tag open-tag ops operand)
  (let loop ([items '()])                  ; operands and operators read so far, last first
    (define-values (d open?) (operand c))
    (cond
      [open? (when (at? c ops) (refuse))    ; an open term ends its level (grammar.rkt)
             (values (cons open-tag (reverse (cons d items))) #t)]
      [(at? c ops)
       ;; `X in - 1`: X an alias named `in`, or membership of -1
       (when (and (at? c '(in contains)) (memq (peek-kind c 1) '(+ -))) (refuse))
       (define op (take! c))
       (loop (list* op d items))]
      [else (values (cons tag (reverse (cons d items))) #f)])))

(define (implies-level c) (level c 'implies-expr 'implies-open '(implies) or-level))
(define (or-level c) (level c 'or-expr 'or-open '(or xor) and-level))
(define (and-level c) (level c 'and-expr 'and-open '(and) membership-level))
(define (membership-level c) (level c 'membership-expr 'membership-open '(in contains) equality-level))
(define (equality-level c) (level c 'equality-expr 'equality-open '(= != ~ !~) comparison-level))
(define (setop-level c) (level c 'setop-expr 'setop-open '(union intersect except) additive-level))
(define (additive-level c) (level c 'additive-expr 'additive-open '(+ - &) multiplicative-level))
(define (multiplicative-level c) (level c 'multiplicative-expr 'multiplicative-open '(* / div mod) power-level))
;; power-expr : type-expr (@power-op type-expr)*   power-open : (type-expr @power-op)* unary-open
(define (power-level c) (level c 'power-expr 'power-open '(^) type-level))

;; closed : Reading -> Datum   the datum of a reading that must be closed (an operand the grammar
;; does not let be open)
(define-syntax-rule (closed reading)
  (let-values ([(d open?) reading]) (if open? (refuse) d)))

;; comparison-expr : setop-expr comp-tail*
;; comparison-open : setop-open | setop-expr comp-tail* comp-tail-open
;; comp-tail       : @comparison-op setop-expr | timing-rel | between-tail
;; comp-tail-open  : @comparison-op setop-open | timing-rel-open
(define (comparison-level c)
  (define-values (first open?) (setop-level c))
  (cond
    [open? (values `(comparison-open ,first) #t)]
    [else
     (let loop ([tails '()])                ; last first
       (define (operand-tail make-closed make-open)
         (define-values (d open?) (setop-level c))
         (if open?
             (begin (when (comp-tail-start? c) (refuse))
                    (values `(comparison-open ,first ,@(reverse tails) ,(make-open d)) #t))
             (loop (cons (make-closed d) tails))))
       (cond
         [(at? c '(<= >= < >))
          (define op (take! c))
          (operand-tail (lambda (d) `(comp-tail ,op ,d)) (lambda (d) `(comp-tail-open ,op ,d)))]
         ;; between-tail : /"between" additive-expr /"and" additive-expr
         [(at? c 'between)
          (next! c)
          (define a (closed (additive-level c)))
          (skip! c 'and)
          (define b (closed (additive-level c)))
          (loop (cons `(comp-tail (between-tail ,a ,b)) tails))]
         [(timing-start? c)
          (define top (timing-op c))
          (operand-tail (lambda (d) `(comp-tail (timing-rel ,top ,d)))
                        (lambda (d) `(comp-tail-open (timing-rel-open ,top ,d))))]
         [else (values `(comparison-expr ,first ,@(reverse tails)) #f)]))]))
;; comp-tail-start? : Cursor -> Boolean   a comparison tail begins here
(define (comp-tail-start? c) (or (at? c '(<= >= < > between)) (timing-start? c)))

;; ---- timing phrases -----------------------------------------------------------------------------
(define timing-words '(starts ends occurs same properly includes during included within meets overlaps
                       on before after NUMBER |less than| |more than|))
;; timing-start? : Cursor -> Boolean   a timing-op begins here (`in`/`contains` only with a
;; precision: `A in day of B`; without one they are membership)
(define (timing-start? c)
  (define k (peek-kind c))
  (cond [(memq k '(in contains)) (and (eq? (peek-kind c 1) 'IDENTIFIER) (eq? (peek-kind c 2) 'of))]
        [else (and k (memq k timing-words) #t)]))

;; timing-op : the grammar's fifteen forms, read by their first words:
;;   timing-lead? "same" prec? same-tail timing-trail?
;;   "properly"? "includes" prec-of? timing-trail?
;;   timing-lead? "properly"? "during" prec-of?
;;   timing-lead? "properly"? "included" /"in" prec-of?
;;   timing-lead? quantity-offset? temporal-rel prec-of? timing-trail?
;;   timing-lead? "properly"? "within" quantity /"of" timing-trail?
;;   "meets" ("before"|"after")? prec-of? | "overlaps" ("before"|"after")? prec-of?
;;   "starts" prec-of? | "ends" prec-of? | "in" prec-of | "contains" prec-of
(define (timing-op c)
  `(timing-op
    ,@(case (peek-kind c)
        [(in contains) (let ([w (take! c)]) (list w (prec-of c)))]
        [(meets overlaps)
         (define w (take! c))
         (define side (optional (at? c '(before after)) (lambda () (take! c))))
         `(,w ,@side ,@(prec-of? c))]
        [(includes) (let ([w (take! c)]) `(,w ,@(prec-of? c) ,@(timing-trail? c)))]
        [(properly)
         (if (eq? (peek-kind c 1) 'includes)
             (let* ([p (take! c)] [w (take! c)]) `(,p ,w ,@(prec-of? c) ,@(timing-trail? c)))
             (led-timing c '()))]
        [(starts ends)
         (case (peek-kind c 1)
           ;; `A starts 3 days before B`: a lead and an offset, or `starts` of 3 days then a
           ;; second tail; with `or less`/`or more` after the quantity only the first
           [(NUMBER) (if (qualified-offset-ahead? c 1) (led-timing c (list `(timing-lead ,(take! c)))) (refuse))]
           [(same properly during included within on before after |less than| |more than|)
            (led-timing c (list `(timing-lead ,(take! c))))]
           [else (let ([w (take! c)]) `(,w ,@(prec-of? c)))])]
        [(occurs) (led-timing c (list `(timing-lead ,(take! c))))]
        [else (led-timing c '())])))
;; led-timing : Cursor (listof Datum) -> (listof Datum)   the forms that may follow a timing-lead
(define (led-timing c lead)
  (case (peek-kind c)
    [(same)
     (define w (take! c))
     (define prec (optional (at? c 'IDENTIFIER) (lambda () `(prec ,(identifier c)))))
     ;; same-tail : /"as" | /"or" "before" | /"or" "after"
     (define tail (cond [(at? c 'as) (next! c) '(same-tail)]
                        [(and (at? c 'or) (memq (peek-kind c 1) '(before after))) (next! c) `(same-tail ,(take! c))]
                        [else (refuse)]))
     `(,@lead ,w ,@prec ,tail ,@(timing-trail? c))]
    [(properly)
     (define p (take! c))
     (unless (at? c '(during included within)) (refuse))
     `(,@lead ,p ,@(led-timing c '()))]
    [(during) (let ([w (take! c)]) `(,@lead ,w ,@(prec-of? c)))]
    [(included) (let ([w (take! c)]) (skip! c 'in) `(,@lead ,w ,@(prec-of? c)))]
    [(within)
     (define w (take! c))
     (define q (quantity c #:keyword-unit (lambda () (refuse))))
     (skip! c 'of)
     `(,@lead ,w ,q ,@(timing-trail? c))]
    [else
     ;; quantity-offset : quantity offset-qual? | excl-qual quantity
     (define offset
       (case (peek-kind c)
         [(NUMBER)
          (define q (quantity c #:keyword-unit (lambda () (refuse))))
          (list `(quantity-offset ,q ,@(optional (at? c '(|or more| |or less|)) (lambda () `(offset-qual ,(take! c))))))]
         [(|less than| |more than|)
          (define x (take! c))
          (list `(quantity-offset (excl-qual ,x) ,(quantity c #:keyword-unit (lambda () (refuse)))))]
         [else '()]))
     `(,@lead ,@offset ,@(temporal-rel c) ,@(prec-of? c) ,@(timing-trail? c))]))
;; qualified-offset-ahead? : Cursor Nat -> Boolean   a quantity with an offset-qual begins `ahead` places on
(define (qualified-offset-ahead? c ahead)
  (and (eq? (peek-kind c ahead) 'NUMBER)
       (let ([k (if (memq (peek-kind c (add1 ahead)) '(IDENTIFIER STRING)) (+ ahead 2) (add1 ahead))])
         (and (memq (peek-kind c k) '(|or more| |or less|)) #t))))
;; temporal-rel (spliced) : "on" /"or" "before" | "on" /"or" "after" | "before" | "after"
;;                        | "before" /"or" "on" | "after" /"or" "on"
(define (temporal-rel c)
  (case (peek-kind c)
    [(on) (define on (take! c)) (skip! c 'or)
          (if (at? c '(before after)) (list on (take! c)) (refuse))]
    [(before after)
     (define w (take! c))
     (if (and (at? c 'or) (eq? (peek-kind c 1) 'on)) (begin (next! c) (list w (take! c))) (list w))]
    [else (refuse)]))
;; prec-of? : (prec-of (prec identifier)) when a precision and `of` are next
(define (prec-of? c)
  (optional (and (eq? (peek-kind c) 'IDENTIFIER) (eq? (peek-kind c 1) 'of)) (lambda () (prec-of c))))
;; prec-of : prec /"of"   prec : identifier
(define (prec-of c)
  (define p `(prec ,(name c)))
  (skip! c 'of)
  `(prec-of ,p))
;; timing-trail? : (timing-trail "start"|"end") — not when `of` follows (`start of B` is the operand)
(define (timing-trail? c)
  (optional (and (at? c '(start end)) (not (eq? (peek-kind c 1) 'of))) (lambda () `(timing-trail ,(take! c)))))

;; ---- type operators and the unary level ---------------------------------------------------------
;; type-expr : unary-expr | cast-expr | convert-expr | unary-expr type-op-tail+
;; An open unary goes up as it is (power-open's last operand is a unary-open).
(define (type-level c)
  (case (peek-kind c)
    ;; cast-expr : /"cast" unary-expr /"as" type-specifier
    [(cast) (next! c)
            (define u (closed (unary-level c)))
            (skip! c 'as)
            (values `(type-expr (cast-expr ,u ,(type-specifier c))) #f)]
    ;; convert-expr : /"convert" unary-expr /"to" type-specifier | /"convert" unary-expr /"to" STRING
    [(convert) (next! c)
               (define u (closed (unary-level c)))
               (skip! c 'to)
               (values `(type-expr (convert-expr ,u ,(if (at? c 'STRING) (take! c) (type-specifier c)))) #f)]
    [else
     (define-values (u open?) (unary-level c))
     (if open?
         (values u #t)
         (values `(type-expr ,u ,@(let loop () (if (at? c '(is as)) (cons (type-op-tail c) (loop)) '()))) #f))]))
;; type-op-tail : @type-op type-specifier | "is" "null" | "is" "not" "null" | "is" BOOL | "is" "not" BOOL
(define (type-op-tail c)
  (define op (take! c))
  `(type-op-tail
    ,op
    ,@(cond
        [(and (equal? op "is") (at? c '(null BOOL))) (list (take! c))]
        [(and (equal? op "is") (at? c 'not))
         (define n (take! c))
         (if (at? c '(null BOOL)) (list n (take! c)) (refuse))]
        [(type-start? c) (list (type-specifier c))]
        [else (refuse)])))

;; unary-expr : @unary-op unary-expr | interval-prefix | duration-of | component-from | postfix-expr
;;            | bare-query | duration-between | difference-between
;; unary-open : query | if-expr | @unary-op unary-open | interval-prefix-open
(define (unary-level c)
  (case (peek-kind c)
    [(+ - not exists)
     (define op (take! c))
     (define-values (d open?) (unary-level c))
     (values (list (if open? 'unary-open 'unary-expr) op d) open?)]
    ;; interval-prefix : "start" /"of" unary-expr | … | "point" /"from" unary-expr | …
    [(start end width predecessor successor) (prefixed c 'of)]
    [(point singleton) (prefixed c 'from)]
    ;; "collapse" unary-expr (/"per" unary-expr)? | "expand" …
    [(collapse expand)
     (define w (take! c))
     (define u (closed (unary-level c)))
     (define per (optional (at? c 'per) (lambda () (next! c) (closed (unary-level c)))))
     (values `(unary-expr (interval-prefix ,w ,u ,@per)) #f)]
    ;; "distinct" unary-expr, interval-prefix-open : "distinct" query, and the aliased-set-op
    [(distinct flatten)
     (define w (take! c))
     (define-values (u open?) (unary-level c))
     (match-open u open?
       (lambda (q) (values `(unary-open (interval-prefix-open ,w ,q)) #t))
       (lambda () (values `(unary-expr (interval-prefix ,w ,u)) #f)))]
    ;; duration-of : /"duration" /"in" identifier /"of" unary-expr
    ;; duration-between : /"duration" /"in" identifier /"between" additive-expr /"and" additive-expr
    [(duration)
     (next! c) (skip! c 'in)
     (define unit (name c))
     (case (peek-kind c)
       [(of) (next! c) (values `(unary-expr (duration-of ,unit ,(closed (unary-level c)))) #f)]
       [(between) (values `(unary-expr ,(between c 'duration-between unit)) #f)]
       [else (refuse)])]
    ;; difference-between : /"difference" /"in" identifier /"between" additive-expr /"and" additive-expr
    [(difference)
     (next! c) (skip! c 'in)
     (define unit (name c))
     (unless (at? c 'between) (refuse))
     (values `(unary-expr ,(between c 'difference-between unit)) #f)]
    ;; if-expr : /"if" expr /"then" expr /"else" expr — open; the else part ends where its context does
    [(if)
     (next! c)
     (define test (enclosed c (lambda () (expr c))))
     (skip! c 'then)
     (define then (enclosed c (lambda () (expr c))))
     (skip! c 'else)
     (values `(unary-open (if-expr ,test ,then ,(expr c))) #t)]
    ;; query : /"from" source-clause (/"," source-clause)* query-tail?
    [(from)
     (next! c)
     (define sources
       (let loop ([acc (list (source-clause c))])
         (cond [(and (at? c '|,|) (own-comma? c #f))   ; another source, or the next element
                (next! c) (loop (cons (source-clause c) acc))]
               [else (reverse acc)])))
     (values `(unary-open (query ,@sources ,@(query-tail? c))) #t)]
    ;; component-from : identifier /"from" unary-expr
    [(IDENTIFIER)
     (if (eq? (peek-kind c 1) 'from)
         (let ([p (name c)]) (next! c) (values `(unary-expr (component-from ,p ,(closed (unary-level c)))) #f))
         (postfix-or-query c))]
    [else (postfix-or-query c)]))
;; match-open : Datum Boolean (Datum -> X) (-> X) -> X   distinct/flatten's operand: an open query
;; (the query datum to the first), a closed term that is no bare query (to the second); anything
;; else is refused — a bare query is read two ways (interval-prefix or aliased-set-op)
(define (match-open u open? query closed)
  (cond
    [open? (if (and (eq? (car u) 'unary-open) (pair? (cadr u)) (eq? (car (cadr u)) 'query)) (query (cadr u)) (refuse))]
    [(and (pair? (cdr u)) (pair? (cadr u)) (eq? (car (cadr u)) 'bare-query)) (refuse)]
    [else (closed)]))
;; prefixed : Cursor Symbol -> Reading   "kw" /word unary-expr
(define (prefixed c word)
  (define w (take! c))
  (skip! c word)
  (values `(unary-expr (interval-prefix ,w ,(closed (unary-level c)))) #f))
;; between : Cursor Symbol Datum -> Datum   … /"between" additive-expr /"and" additive-expr
;; The term is a unary-expr: an operator after its last operand (`… and B is null`, `… and B * 2`)
;; may be the operand's or the term's, so only a last operand with no operator of its own is ours.
(define (between c tag unit)
  (skip! c 'between)
  (define a (closed (additive-level c)))
  (skip! c 'and)
  (define b (closed (additive-level c)))
  (unless (single-term? b) (refuse))
  `(,tag ,unit ,a ,b))
;; single-term? : Datum -> Boolean   (additive-expr (multiplicative-expr (power-expr (type-expr U))))
(define (single-term? d)
  (let loop ([d d] [tags '(additive-expr multiplicative-expr power-expr type-expr)])
    (cond [(null? tags) #t]
          [else (and (pair? d) (eq? (car d) (car tags)) (= (length d) 2) (loop (cadr d) (cdr tags)))])))

;; ---- queries ------------------------------------------------------------------------------------
;; postfix-or-query : Cursor -> Reading   a postfix-expr; when an alias follows, the source clause
;; it begins — a bare-query (closed) or, with a tail, a query (open)
(define (postfix-or-query c)
  (define-values (p source) (postfix-expr c))
  (cond
    [(at? c name-kinds)
     (unless source (refuse))
     (define clause `(source-clause ,source (alias ,(name c))))
     (if (tail-start? c)
         (values `(unary-open (query ,clause ,@(query-tail? c))) #t)
         (values `(unary-expr (bare-query ,clause)) #f))]
    [else (values `(unary-expr ,p) #f)]))
;; source-clause : query-source alias
(define (source-clause c)
  (define-values (_p source) (postfix-expr c))
  (unless (and source (at? c name-kinds)) (refuse))
  `(source-clause ,source (alias ,(name c))))
;; tail-start? : Cursor -> Boolean   a query-tail begins here
(define (tail-start? c) (at? c '(let with without where return aggregate sort)))

;; query-tail? : Cursor -> (listof Datum)   the query-tail when one is next. Its five alternatives
;; are one sequence: let-clause* inclusion-clause* where-clause? return-or-agg? sort-clause?, not
;; empty. Inside an enclosing query's tail the clause could be that query's: brag decides.
(define (query-tail? c)
  (cond
    [(not (tail-start? c)) '()]
    [(cursor-tail? c) (refuse)]
    [else
     (with-context c #t (cursor-comma c)
       (lambda ()
         (define lets (let loop () (if (at? c 'let) (cons (let-clause c) (loop)) '())))
         (define inclusions (let loop () (if (at? c '(with without)) (cons (inclusion-clause c) (loop)) '())))
         (define where (optional (at? c 'where) (lambda () (next! c) `(where-clause ,(expr c)))))
         (define roa (optional (at? c '(return aggregate)) (lambda () `(return-or-agg ,(return-or-aggregate c)))))
         (define sort (optional (at? c 'sort) (lambda () (sort-clause c))))
         (list `(query-tail ,@lets ,@inclusions ,@where ,@roa ,@sort))))]))
;; let-clause : /"let" let-item (/"," let-item)*    let-item : identifier /":" expr
(define (let-clause c)
  (skip! c 'let)
  (let loop ([acc (list (let-item c))])
    (cond [(and (at? c '|,|) (own-comma? c #t)) (next! c) (loop (cons (let-item c) acc))]
          [else `(let-clause ,@(reverse acc))])))
(define (let-item c)
  (define n (name c))
  (skip! c ':)
  `(let-item ,n ,(with-context c #t 'keyed (lambda () (expr c)))))
;; inclusion-clause : with-clause | without-clause
;; with-clause : /"with" source-clause /"such that" expr   (without-clause the same)
(define (inclusion-clause c)
  (define tag (if (eq? (peek-kind c) 'with) 'with-clause 'without-clause))
  (next! c)
  (define s (source-clause c))
  (skip! c '|such that|)
  `(inclusion-clause (,tag ,s ,(expr c))))
;; return-or-agg : return-clause | aggregate-clause
;; return-clause : /"return" return-mod? expr
;; aggregate-clause : /"aggregate" return-mod? identifier starting-clause? /":" expr
(define (return-or-aggregate c)
  (case (peek-kind c)
    [(return)
     (next! c)
     ;; `return distinct X`: a return-mod, or Distinct of X
     (when (at? c 'distinct) (refuse))
     (define m (optional (at? c 'all) (lambda () `(return-mod ,(take! c)))))
     `(return-clause ,@m ,(expr c))]
    [else
     (next! c)
     (define m (optional (at? c '(all distinct)) (lambda () `(return-mod ,(take! c)))))
     (define n (name c))
     (define s (optional (at? c 'starting) (lambda () (next! c) `(starting-clause ,(expr c)))))
     (skip! c ':)
     `(aggregate-clause ,@m ,n ,@s ,(expr c))]))
;; sort-clause : /"sort" sort-dir | /"sort" /"by" sort-item (/"," sort-item)*
;; sort-item : unary-expr sort-dir?
(define sort-dirs '(asc ascending desc descending))
(define (sort-clause c)
  (skip! c 'sort)
  (cond
    [(at? c sort-dirs) `(sort-clause (sort-dir ,(take! c)))]
    [else
     (skip! c 'by)
     (define (item c)
       (define u (closed (unary-level c)))
       `(sort-item ,u ,@(optional (at? c sort-dirs) (lambda () `(sort-dir ,(take! c))))))
     (let loop ([acc (list (item c))])
       (cond [(and (at? c '|,|) (own-comma? c #f)) (next! c) (loop (cons (item c) acc))]
             [else `(sort-clause ,@(reverse acc))]))]))

;; ---- postfix expressions and primaries ----------------------------------------------------------
;; postfix-expr : Cursor -> (values Datum Datum|#f)   (postfix-expr primary postfix-tail*), and the
;; query-source it can begin when an alias follows it: query-source : /"(" expr /")" | retrieve
;; | member | qualified-source, where qualified-source : identifier (/"." @ref-id)+
(define (postfix-expr c)
  (define-values (prim kind) (primary c))
  (define-values (tails dotted)                ; dotted: the ref-ids when every tail is a member
    (let loop ([tails '()] [dotted '()])
      (case (peek-kind c)
        [(|.|)
         (next! c)
         (define r (ref-id c))
         (if (at? c '|(|)
             (loop (cons `(postfix-tail (invocation (dot-call ,r ,@(arguments c)))) tails) #f)
             (loop (cons `(postfix-tail (invocation (dot-member ,r))) tails) (and dotted (cons r dotted))))]
        [(|[|)
         (next! c)
         (define i (enclosed c (lambda () (expr c))))
         (skip! c '|]|)
         (loop (cons `(postfix-tail (indexer ,i)) tails) #f)]
        [else (values (reverse tails) (and dotted (reverse dotted)))])))
  (values `(postfix-expr ,prim ,@tails)
          (cond [(null? tails) (case kind
                                 [(paren retrieve member) `(query-source ,(cadr prim))]
                                 [else #f])]
                [(and dotted (eq? kind 'member)) `(query-source (qualified-source ,(cadr (cadr prim)) ,@dotted))]
                [else #f])))
;; arguments : Cursor -> (listof Datum)   /"(" arg-list? /")"   arg-list : expr (/"," expr)*
(define (arguments c)
  (skip! c '|(|)
  (cond [(at? c '|)|) (next! c) '()]
        [else (list `(arg-list ,@(listed c (lambda () (separated c '|,| expr '|)|)))))]))

;; primary : Cursor -> (values Datum Symbol|#f)   (primary …) and what it is, when it can be a
;; query source: 'paren, 'retrieve, 'member
(define (primary c)
  (define (just d) (values `(primary ,d) #f))
  (case (peek-kind c)
    [(NUMBER) (just (number-literal c))]
    [(LONGNUMBER STRING BOOL DATE DATETIME TIME null) (just `(literal ,(take! c)))]
    [(|(|) (next! c)
           (define e (enclosed c (lambda () (expr c))))
           (skip! c '|)|)
           (values `(primary ,e) 'paren)]
    [(|[|) (values `(primary ,(retrieve c)) 'retrieve)]
    [(|{|) (just (braced c))]
    ;; interval-selector : /"Interval" interval-open expr /"," expr interval-close
    [(Interval)
     (next! c)
     (define open (if (at? c '(|[| |(|)) `(interval-open ,(take! c)) (refuse)))
     (define-values (low high) (listed c (lambda () (define l (expr c)) (skip! c '|,|) (values l (expr c)))))
     (define close (if (at? c '(|]| |)|)) `(interval-close ,(take! c)) (refuse)))
     (just `(interval-selector ,open ,low ,high ,close))]
    ;; typed-list-selector : /"List" type-arg? /"{" list-body? /"}"
    [(List)
     (next! c)
     (define arg (optional (at? c '<) (lambda () `(type-arg ,(type-argument c)))))
     (skip! c '|{|)
     (just `(typed-list-selector ,@arg ,@(list-body? c)))]
    ;; typed-tuple-selector : /"Tuple" /"{" tuple-body? /"}"
    [(Tuple) (next! c) (skip! c '|{|) (just `(typed-tuple-selector ,@(tuple-body? c)))]
    ;; minmax : "minimum" type-specifier | "maximum" type-specifier
    ;; `minimum System.Decimal`: the type System.Decimal, or `.Decimal` of the type System
    [(minimum maximum)
     (define w (take! c))
     (define t (type-specifier c))
     (match t [`(type-specifier (named-type (qualified-id ,_ ,_ ,_ ...))) (refuse)] [_ (void)])
     (just `(minmax ,w ,t))]
    [(Code) (just (code-primary c))]
    [(Concept) (just (concept-primary c))]
    [(case) (just (case-expr c))]
    [(IDENTIFIER code concept) (name-primary c)]
    [else (refuse)]))

;; number-literal : literal : ratio | quantity …   ratio : quantity /":" quantity
(define (number-literal c)
  (define q (quantity c #:keyword-unit (lambda () (keyword-after-number c))))
  (if (and (at? c ':) (eq? (peek-kind c 1) 'NUMBER))
      (begin (next! c) `(literal (ratio ,q ,(quantity c #:keyword-unit (lambda () (keyword-after-number c))))))
      `(literal ,q)))
;; quantity : NUMBER unit?   unit : identifier | STRING
;; keyword-unit: what to do when a keyword the grammar also takes as an identifier follows the number
(define (quantity c #:keyword-unit keyword-unit)
  (define n (expect! c 'NUMBER))
  `(quantity ,n
             ,@(case (peek-kind c)
                 [(IDENTIFIER) (list `(unit (identifier ,(take! c))))]
                 [(STRING) (list `(unit ,(take! c)))]
                 [(as is in contains code concept) (keyword-unit)]
                 [else '()])))
;; keyword-after-number : Cursor -> '()   `5 in X`: `in` is the operator for sure when what follows
;; it begins an operand (it could not follow a quantity); otherwise the grammar reads the keyword
;; as the unit too, and brag decides
(define operand-starts '(IDENTIFIER NUMBER STRING BOOL LONGNUMBER DATE DATETIME TIME |(| |{| |[| null not
                         Interval List Tuple Code Concept))
(define (keyword-after-number c)
  (if (and (at? c '(as is in contains)) (memq (peek-kind c 1) operand-starts)) '() (refuse)))

;; name-primary : function-call | instance-selector (a qualified-id and `{`) | member
(define (name-primary c)
  (cond
    ;; function-call : identifier /"(" arg-list? /")"
    [(eq? (peek-kind c 1) '|(|)
     (define n (name c))
     (values `(primary (function-call ,n ,@(arguments c))) #f)]
    [(instance-ahead? c)
     (define q (let ([first (name c)]) `(qualified-id ,first ,@(dotted-ref-ids c))))
     (skip! c '|{|)
     (values `(primary (instance-selector ,q ,@(tuple-body? c))) #f)]
    [else (values `(primary (member ,(name c))) 'member)]))
;; instance-ahead? : Cursor -> Boolean   a name, (. ref-id)*, then `{`
(define (instance-ahead? c)
  (let loop ([k 1])
    (case (peek-kind c k)
      [(|{|) #t]
      [(|.|) (and (ref-id-kind? (peek-kind c (add1 k))) (loop (+ k 2)))]
      [else #f])))

;; braced : /"{" braced-body? /"}" | /"{" ":" /"}"     braced-body : tuple-body | list-body
(define (braced c)
  (skip! c '|{|)
  (cond
    [(at? c '|}|) (next! c) '(braced)]
    [(and (at? c ':) (eq? (peek-kind c 1) '|}|)) (let ([colon (take! c)]) (next! c) `(braced ,colon))]
    [(tuple-ahead? c) `(braced (braced-body ,@(tuple-body? c)))]
    [else `(braced (braced-body ,@(list-body? c)))]))
;; tuple-ahead? : Cursor -> Boolean   a tuple element (a name and `:`) is next
(define (tuple-ahead? c) (and (ref-id-kind? (peek-kind c)) (eq? (peek-kind c 1) ':)))
;; list-body? : Cursor -> (listof Datum)   list-body : expr (/"," expr)*, then the closing `}`
(define (list-body? c)
  (cond [(at? c '|}|) (next! c) '()]
        [else (list `(list-body ,@(listed c (lambda () (separated c '|,| expr '|}|)))))]))
;; tuple-body? : Cursor -> (listof Datum)   tuple-body : tuple-element (/"," tuple-element)*, then `}`
;; tuple-element : @ref-id /":" expr
(define (tuple-body? c)
  (cond [(at? c '|}|) (next! c) '()]
        [else (list `(tuple-body ,@(keyed c (lambda () (separated c '|,| tuple-element '|}|)))))]))
(define (tuple-element c)
  (define n (ref-id c))
  (skip! c ':)
  `(tuple-element ,n ,(expr c)))

;; code-primary : code-selector | instance-selector "Code"
;; code-selector : /"Code" STRING /"from" term-ref display-clause?
(define (code-primary c)
  (case (peek-kind c 1)
    [(STRING) (begin0 (code-selector c)
                      ;; `Code 'x' from A.B`, `….display`: the system is `A.B`, or a member of the selector
                      (when (at? c '(|.| |[|)) (refuse)))]
    [(|{|) (let ([w (take! c)]) (next! c) `(instance-selector ,w ,@(tuple-body? c)))]
    [else (refuse)]))
(define (code-selector c)
  (skip! c 'Code)
  (define code (expect! c 'STRING))
  (skip! c 'from)
  (define tr `(term-ref ,(name c)))
  (when (at? c '|.|) (refuse))
  `(code-selector ,code ,tr ,@(display-clause? c)))
;; concept-primary : concept-selector | instance-selector "Concept"
;; concept-selector : /"Concept" /"{" code-selector (/"," code-selector)* /"}" display-clause?
(define (concept-primary c)
  (unless (eq? (peek-kind c 1) '|{|) (refuse))
  (cond
    [(and (eq? (peek-kind c 2) 'Code) (eq? (peek-kind c 3) 'STRING))
     (next! c) (next! c)
     (define codes (separated c '|,| concept-code '|}|))
     (begin0 `(concept-selector ,@codes ,@(display-clause? c))
             (when (at? c '(|.| |[|)) (refuse)))]
    [else (let ([w (take! c)]) (next! c) `(instance-selector ,w ,@(tuple-body? c)))]))
;; concept-code : a code-selector inside a concept's braces (its system may be qualified there)
(define (concept-code c)
  (skip! c 'Code)
  (define code (expect! c 'STRING))
  (skip! c 'from)
  `(code-selector ,code ,(term-ref c) ,@(display-clause? c)))

;; case-expr : /"case" expr? case-item+ case-else? /"end"
;; case-item : /"when" expr /"then" expr      case-else : /"else" expr
(define (case-expr c)
  (skip! c 'case)
  (enclosed c
    (lambda ()
      (define comparand (optional (not (at? c 'when)) (lambda () (expr c))))
      (define items (let loop () (if (at? c 'when)
                                     (let* ([_ (next! c)] [w (expr c)] [_t (skip! c 'then)] [t (expr c)])
                                       (cons `(case-item ,w ,t) (loop)))
                                     '())))
      (when (null? items) (refuse))
      (define els (optional (at? c 'else) (lambda () (next! c) `(case-else ,(expr c)))))
      (skip! c 'end)
      `(case-expr ,@comparand ,@items ,@els))))

;; retrieve : /"[" retrieve-context? named-type (/":" retrieve-filter)? /"]"
;; retrieve-context : postfix-expr /"->"
;; retrieve-filter  : code-path code-comparator terminology | terminology
(define (retrieve c)
  (skip! c '|[|)
  (enclosed c
    (lambda ()
      (define context (optional (arrow-ahead? c)
                                (lambda () (define-values (p _s) (postfix-expr c)) (skip! c '->) `(retrieve-context ,p))))
      (define type (named-type c))
      (define filter (optional (at? c ':) (lambda () (next! c) (retrieve-filter c))))
      (skip! c '|]|)
      `(retrieve ,@context ,type ,@filter))))
;; arrow-ahead? : Cursor -> Boolean   a `->` comes before this retrieve's `:` or `]`
(define (arrow-ahead? c)
  (let loop ([k 0] [depth 0])
    (case (peek-kind c k)
      [(#f) #f]
      [(->) (zero? depth)]
      [(|(| |[| |{|) (loop (add1 k) (add1 depth))]
      [(|)| |]| |}|) (and (positive? depth) (loop (add1 k) (sub1 depth)))]
      [(:) (and (positive? depth) (loop (add1 k) depth))]
      [else (loop (add1 k) depth)])))
(define (retrieve-filter c)
  `(retrieve-filter
    ,@(if (code-path-ahead? c)
          (let* ([path `(code-path ,(ref-id c) ,@(dotted-ref-ids c))]
                 [cmp `(code-comparator ,(take! c))])
            (list path cmp (terminology c)))
          (list (terminology c)))))
;; code-path-ahead? : Cursor -> Boolean   @ref-id (/"." @ref-id)* then "in" | "=" | "~"
(define (code-path-ahead? c)
  (and (ref-id-kind? (peek-kind c))
       (let loop ([k 1])
         (case (peek-kind c k)
           [(in = ~) #t]
           [(|.|) (and (ref-id-kind? (peek-kind c (add1 k))) (loop (+ k 2)))]
           [else #f]))))
;; terminology : postfix-expr
(define (terminology c)
  (define-values (p _s) (postfix-expr c))
  `(terminology ,p))

(module+ test
  (require rackunit (only-in brag/support token srcloc-token? srcloc-token-token token-struct-skip?)
           (prefix-in g: "grammar.rkt") (only-in "lexer.rkt" tokenize))
  ;; tokens by hand: (type . value) pairs
  (define (toks . kvs) (for/list ([kv (in-list kvs)]) (token (car kv) (cdr kv))))
  (define (id s) (cons 'IDENTIFIER (string->symbol s)))
  (define (kw s) (cons (string->symbol s) s))
  (define (num s) (cons 'NUMBER s))
  ;; the cascade around one literal, as brag writes it
  (define (closed-lit q)
    `(expr (implies-expr (or-expr (and-expr (membership-expr (equality-expr (comparison-expr (setop-expr (additive-expr (multiplicative-expr (power-expr (type-expr (unary-expr (postfix-expr (primary (literal ,q)))))))))))))))))
  (check-equal? (descend-library (toks (kw "define") (id "X") (kw ":") (num "1")))
                `(cql-library (definition (statement-def (expression-def (identifier X) ,(closed-lit '(quantity "1")))))))
  (check-equal? (descend-library (toks (kw "define") (id "X") (kw ":") (num "5") (id "days")))
                `(cql-library (definition (statement-def (expression-def (identifier X) ,(closed-lit '(quantity "5" (unit (identifier days)))))))))
  ;; a header and a using
  (check-equal? (descend-library (toks (kw "library") (id "L") (kw "version") (cons 'STRING "1") (kw "using") (id "FHIR")))
                '(cql-library (library-header (qualified-id (identifier L)) (version-spec "1"))
                              (definition (using-def (qualified-id (identifier FHIR))))))
  ;; what is left to brag
  (check-false (descend-library (toks (kw "define") (id "X") (kw ":") (num "5") (kw "is"))))       ; a unit, or `is`
  (check-false (descend-library (toks (kw "define") (id "X") (kw ":") (kw "return"))))
  (check-false (descend-library (toks (kw "define") (id "X") (kw ":") (num "1") (kw "+"))))
  (check-false (descend-library (toks (kw "define") (id "X") (kw ":") (id "a") (kw "where") (id "b"))))
  (check-false (descend-library (toks (kw "define") (id "X") (kw ":") (id "xs") (id "x") (kw "return") (kw "distinct") (id "x"))))
  ;; from text: descent's datum is brag's, or descent leaves the text to brag
  (define (lexed text)
    (define next (tokenize (open-input-string (string-append "define X: " text))))
    (let loop () (define t (next)) (if (void? t) '() (cons t (loop)))))
  (define (real ts) (for*/list ([t (in-list ts)] [k (in-value (if (srcloc-token? t) (srcloc-token-token t) t))]
                                #:unless (token-struct-skip? k)) k))
  (define (brag text) (g:parse-to-datum (let ([ts (lexed text)]) (lambda () (if (null? ts) (void) (begin0 (car ts) (set! ts (cdr ts))))))))
  (define (D text) (descend-library (real (lexed text))))
  (for ([text (in-list '("[Encounter] E where E.period during \"MP\" return E.id"
                         "from [A] a, [B] b where a.x = b.y sort by a.x desc"
                         "A 3 days or less on or before day of B" "A starts same day as end B" "A within 3 days of B"
                         "if c then [A] a where p else 1 + 2" "Interval[1, 2) contains 1 and X is not null"
                         "duration in days between A and B > 0" "{ a: 1, start: 'x' }" "Code '1' from \"CS\" display 'd'"
                         "Concept { Code '1' from L.\"CS\" } display 'c'" "cast X as List<Integer>" "convert 5 to days"))])
    (check-equal? (D text) (brag text) text))
  (for ([text (in-list '("[A] a where exists [B] b return 1"        ; the return: the inner query's or the outer's
                         "{ a: X x let m: 1, b: 2 }"               ; `b: 2`: a let item or a tuple element
                         "duration in days between A and B is null" "A ends 1 hour before B"
                         "minimum System.Decimal" "distinct (xs) x" "Code '1' from A.B" "[A] in - 1"))])
    (check-false (D text) text)
    (check-true (pair? (brag text)) text)))
