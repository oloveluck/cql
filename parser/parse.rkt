#lang racket/base

;;; parse.rkt — the CQL parse API: string -> Result<AST>.
;;;
;;; Mirrors fhirpath/parser/parse.rkt: string -> tokens (lexer) -> brag datum (grammar) -> compact
;;; prefab AST (lower). `parse-cql` reads a whole library; `parse-cql-expr` reads a single bare
;;; expression (used by the conformance harness on cql-tests `<expression>` fragments). A parse
;;; failure is caught and returned as (err (cql-error 'parse …)). Results are memoized (prefab ASTs
;;; are immutable, so sharing is safe).

(require racket/contract racket/match
         (only-in racket/list take drop last)
         (prefix-in g: "grammar.rkt")
         "lexer.rkt"
         (only-in brag/support srcloc-token? srcloc-token-token srcloc-token-srcloc token-struct? token-struct-type
                  token-struct-skip? token token-struct-val)
         "ast.rkt"
         (only-in "../error.rkt" cql-error)
         (only-in fhir/diagnostic exn->diagnostic diagnostic->string diagnostic diagnostic-loc loc loc-line loc-column loc-position
                  loc-source exn:fail:diagnostic error-position attach-loc! attached-loc)
         (only-in fhir/result/result ok err ok? err?))

(provide
 (contract-out
  ;; (ok cql-library) | (err cql-error) — a parse error carries its diagnostic (positions in `source`)
  [parse-cql      (->* (string?) (#:source (or/c string? path? #f)) (or/c ok? err?))]
  [parse-cql-expr (-> string? (or/c ok? err?))]))  ; (ok expr-AST)   | (err cql-error)

;; a bare expression (and a long list's element) is parsed as the body of this throwaway definition
(define expr-prefix "define __expr__: ")

;; The caches. A parse is the same for the same text, so results are kept — but a language server
;; parses a document at every keystroke and a workspace server lives for days, so each cache is
;; BOUNDED: when it reaches its limit it is emptied (the next parse of a text fills it again; nothing
;; relies on a cached AST being the same object beyond one evaluation, whose resolver holds it).
;;   lib-cache   : (cons text source) -> Result<cql-library>
;;   expr-cache  : text -> Result<expr>
;;   chunk-cache : ChunkKey -> cql-library      one definition's tokens -> its AST (below)
(define lib-cache (make-hash))
(define expr-cache (make-hash))
(define chunk-cache (make-hash))
(define lib-cache-limit 512)
(define expr-cache-limit 16384)
(define chunk-cache-limit 16384)
;; cached : table key limit (-> value) -> value   hash-ref!, emptying a table that has reached its limit
(define (cached table key limit compute)
  (hash-ref table key
            (lambda ()
              (define v (compute))
              (when (>= (hash-count table) limit) (hash-clear! table))
              (hash-set! table key v)
              v)))

(define (parse-cql str #:source [source #f])
  (cached lib-cache (cons str source) lib-cache-limit
          (lambda ()
            (with-handlers ([exn:fail? (lambda (e) (parse-failure e source))])
              (ok (merge-libraries
                   (for/list ([chunk (in-list (definition-chunks (open-input-string str)))])
                     (with-handlers ([exn:fail? (lambda (e) (raise (chunk-failure e chunk source)))])
                       (located (parsed-chunk chunk) chunk source)))))))))

;; ---- one definition's tokens -> its AST, parsed once -----------------------------------------
;; A ChunkKey is (listof (cons token-type token-value)): a chunk's tokens without their positions and
;; without the skipped ones (space, comments). What a chunk parses to depends on nothing else —
;; positions go to diagnostics, and a parse that fails raises and is not kept — so a definition met
;; again is not parsed again: the same library under another source name, an `include` or a value
;; set declared by a hundred libraries, FHIRHelpers 4.0.1's functions in FHIRHelpers 4.4.000, and
;; every definition of a document but the one being edited (the grammar's parser costs ~60 µs a
;; token; of the 5,001 definitions of the QI-Core corpus 1,896 repeat an earlier one).
;; chunk-key : tokens -> ChunkKey
(define (chunk-key chunk)
  (for*/list ([t (in-list chunk)] [k (in-value (probe-token t))] #:when k)
    (cons (token-struct-type k) (token-struct-val k))))
;; parsed-chunk : tokens -> cql-library   the chunk's definitions: fresh definition structs (each use
;; attaches its own position to them) over the cached, immutable bodies
(define (parsed-chunk chunk)
  (define lib (cached chunk-cache (chunk-key chunk) chunk-cache-limit (lambda () (parse-chunk chunk))))
  (cql-library (cql-library-id lib) (cql-library-version lib) (map fresh-definition (cql-library-defs lib))))
;; fresh-definition : def -> def   the same definition as a new object (a prefab struct of any kind)
(define (fresh-definition d)
  (apply make-prefab-struct (prefab-struct-key d) (cdr (vector->list (struct->vector d)))))
;; parse-chunk : tokens -> cql-library   raises the grammar's exception when the chunk does not parse
(define (parse-chunk chunk)
  (or (and (> (length chunk) long-list-tokens) (parse-long-list chunk #:at-least long-list-elements))
      (lower-library (g:parse-to-datum (list->token-thunk chunk)))))

;; ---- a definition that is one long list ------------------------------------------------------
;; The grammar's parser is QUADRATIC in the number of a list's elements (100 elements: 0.1-0.3 s;
;; 400: 1-3 s; 800: 3-8 s), and memory with it: a code table written as a list of a thousand tuples
;; (`define "DrugIngredients": { {…}, {…}, … }`, 93,000 tokens in one of the QDM corpus's
;; libraries) took 27 s, 18 of them collecting garbage. Such a definition — its body exactly a
;; braced list, not a tuple — is parsed ELEMENT BY ELEMENT, the way a library is parsed definition
;; by definition: the definition with an empty list, then each element as an expression, put
;; together as the list. Anything that does not fit, and any element that does not parse, is left
;; to the whole-definition parse, so every error is the one it was (at the price it was).
(define long-list-tokens 400)      ; a shorter definition is not looked at
(define long-list-elements 50)     ; a shorter list is parsed with its definition
;; parse-long-list : tokens [#:at-least n] -> cql-library | #f   (#f also for a list of fewer elements)
(define (parse-long-list chunk #:at-least [at-least 1])
  (define toks (filter probe-token chunk))
  (define (type-of t) (token-struct-type (probe-token t)))
  (define (is? t . types) (and (memq (type-of t) types) #t))
  ;; the body's `{`: directly after the first top-level `:` (the definition's own; an operand list's
  ;; parentheses are skipped), closed by the last token
  (define open-index
    (let loop ([ts toks] [i 0] [depth 0])
      (cond [(null? ts) #f]
            [(is? (car ts) '|(| '|[| '|{| '<) (loop (cdr ts) (add1 i) (add1 depth))]
            [(is? (car ts) '|)| '|]| '|}| '>) (loop (cdr ts) (add1 i) (sub1 depth))]
            [(and (zero? depth) (is? (car ts) ':)) (and (pair? (cdr ts)) (is? (cadr ts) '|{|) (add1 i))]
            [else (loop (cdr ts) (add1 i) depth)])))
  (define elements (let ([es (and open-index (is? (last toks) '|}|) (list-elements (drop toks (add1 open-index))))])
                     (and es (>= (length es) at-least) es)))
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (match (and elements
                (cql-library-defs (lower-library (g:parse-to-datum
                                                  (list->token-thunk (append (take toks (add1 open-index)) (list (last toks))))))))
      [(list (cql-def name access (cql-list '())))
       (cql-library #f #f (list (cql-def name access (cql-list (map parse-element elements)))))]
      [(list (cql-func name access operands result-type (cql-list '()) fluent?))
       (cql-library #f #f (list (cql-func name access operands result-type (cql-list (map parse-element elements)) fluent?)))]
      [_ #f])))
;; list-elements : tokens -> (listof tokens) | #f   the tokens after a list's `{`, up to the `}` that
;; ends them, split at its commas; #f when the brace closes before the end, when an element is empty,
;; or when it is a tuple (`{ name: value, … }`)
(define (list-elements toks)
  (define (type-of t) (token-struct-type (probe-token t)))
  (and (not (and (pair? toks) (pair? (cdr toks)) (eq? (type-of (cadr toks)) ':)))
       (let loop ([ts toks] [depth 0] [cur '()] [acc '()])
         (cond [(null? ts) #f]
               [(and (zero? depth) (eq? (type-of (car ts)) '|}|))
                (and (null? (cdr ts)) (pair? cur) (reverse (cons (reverse cur) acc)))]
               [(and (zero? depth) (eq? (type-of (car ts)) '|,|))
                (and (pair? cur) (loop (cdr ts) depth '() (cons (reverse cur) acc)))]
               [(memq (type-of (car ts)) '(|(| |[| |{|)) (loop (cdr ts) (add1 depth) (cons (car ts) cur) acc)]
               [(memq (type-of (car ts)) '(|)| |]| |}|)) (loop (cdr ts) (sub1 depth) (cons (car ts) cur) acc)]
               [else (loop (cdr ts) depth (cons (car ts) cur) acc)]))))
;; parse-element : tokens -> expr   one list element, as the body of a throwaway definition
(define element-prefix
  (let ([next (tokenize (open-input-string expr-prefix))])
    (let loop () (let ([t (next)]) (if (void? t) '() (cons t (loop)))))))
(define (parse-element toks)
  (match (cql-library-defs (cached chunk-cache (cons 'element (chunk-key toks)) chunk-cache-limit
                                   (lambda () (lower-library (g:parse-to-datum (list->token-thunk (append element-prefix toks)))))))
    [(list (cql-def _ _ body)) body]))
;; located : cql-library tokens source -> cql-library   each definition remembers where it starts
;; (fhir/diagnostic attach-loc!) so translation errors can point at it
(define (located lib chunk source)
  (define first (for/first ([t (in-list chunk)] #:when (probe-token t)) t))
  (when (and first (srcloc-token? first))
    (define l (token-loc first source))
    (for ([d (in-list (cql-library-defs lib))]) (attach-loc! d l)))
  lib)
;; token-loc : srcloc-token source -> loc   where a token stands (1-based line and column)
(define (token-loc t source)
  (define sl (srcloc-token-srcloc t))
  (loc (and source (if (path? source) (path->string source) source))
       (srcloc-line sl) (and (srcloc-column sl) (add1 (srcloc-column sl))) (srcloc-position sl) (srcloc-span sl)))
;; chunk-failure : exn tokens source -> exn:fail:diagnostic   positioned; "the definition ends after"
;; when the parse ran out of this definition's tokens; a bare unit (`5 mg`) at the word
(define (chunk-failure e chunk source)
  (define last-pos (for/last ([t (in-list chunk)] #:when (srcloc-token? t)
                              #:unless (let ([k (srcloc-token-token t)]) (and (token-struct? k) (token-struct-skip? k))))
                     (srcloc-position (srcloc-token-srcloc t))))
  (define d
    (cond [(and (exn:fail:bare-unit? e) (bare-unit-token chunk (exn:fail:bare-unit-number e) (exn:fail:bare-unit-unit e)))
           => (lambda (t) (diagnostic 'cql 'parse 'error (exn-message e) (token-loc t source)))]
          [else (exn->diagnostic e 'cql #:source source #:last-position last-pos #:unit "definition"
                                 #:ran-out? (ran-out-probe chunk last-pos))]))
  (exn:fail:diagnostic (diagnostic->string d) (current-continuation-marks) d))
;; bare-unit-token : tokens number-lexeme word -> srcloc-token|#f   the word directly after that number
(define (bare-unit-token chunk number unit)
  (define real (filter (lambda (t) (and (srcloc-token? t) (probe-token t))) chunk))
  (for/first ([n (in-list real)] [u (in-list (if (pair? real) (cdr real) '()))]
              #:when (and (eq? (token-struct-type (probe-token n)) 'NUMBER) (equal? (token-struct-val (probe-token n)) number)
                          (eq? (token-struct-type (probe-token u)) 'IDENTIFIER)
                          (equal? (format "~a" (token-struct-val (probe-token u))) unit)))
    u))

;; brag's general CFG parser is superlinear in input length, and FHIRHelpers is ~600 lines: parsing
;; it whole takes seconds. A library is a sequence of definitions, so we parse DEFINITION BY
;; DEFINITION: the token stream splits before every definition keyword in column 0 (where every
;; real-world library starts its definitions; continuation lines are indented). Each chunk is a
;; valid library on its own; tokens keep their original source locations, so errors still point
;; into the file. (Same technique as fsh/parser/parse.rkt's rule-by-rule parsing.)
(define definition-starts
  '(library using include codesystem valueset code concept parameter context define public private))
;; definition-chunks : input-port -> (listof (listof token))
(define (definition-chunks in)
  (define next (tokenize in))
  (let loop ([cur '()] [acc '()])
    (define t (next))
    (cond
      [(void? t) (reverse (if (null? cur) acc (cons (reverse cur) acc)))]
      [(and (pair? cur) (definition-start? t)) (loop (list t) (cons (reverse cur) acc))]
      [else (loop (cons t cur) acc)])))
(define (definition-start? t)
  (and (srcloc-token? t)
       (eqv? (srcloc-column (srcloc-token-srcloc t)) 0)
       (let ([tok (srcloc-token-token t)])
         (and (token-struct? tok) (memq (token-struct-type tok) definition-starts) #t))))
;; probe-token : token|srcloc-token -> token-struct|#f   (skip tokens excluded)
(define (probe-token t)
  (define k (if (srcloc-token? t) (srcloc-token-token t) t))
  (and (token-struct? k) (not (token-struct-skip? k)) k))
;; ran-out-probe : tokens last-position -> (-> boolean)   re-parse with a sentinel token after the
;; last one: if the error moves to the sentinel, the input was a valid prefix that ran out
(define ((ran-out-probe toks last-pos) )
  ;; the sentinel is a COPY of the last real token (a type the grammar knows), placed after it
  (define last-real (for/last ([t (in-list toks)] #:when (probe-token t)) (probe-token t)))
  (and last-pos last-real
       (with-handlers ([exn:fail? (lambda (e) (let ([p (error-position e)]) (and p (> p last-pos))))])
         (g:parse-to-datum (let ([ts (append toks (list (token (token-struct-type last-real) (token-struct-val last-real)
                                                                #:position (+ last-pos 100000) #:line 1 #:column 0)))])
                             (lambda () (if (null? ts) (void) (begin0 (car ts) (set! ts (cdr ts)))))))
         #f)))
(define (list->token-thunk toks)
  (lambda () (if (null? toks) (void) (begin0 (car toks) (set! toks (cdr toks))))))
;; merge-libraries : (listof cql-library) -> cql-library   the header's id/version, all definitions
(define (merge-libraries libs)
  (define head (for/first ([l (in-list libs)] #:when (cql-library-id l)) l))
  (cql-library (and head (cql-library-id head)) (and head (cql-library-version head))
               (apply append (map cql-library-defs libs))))

;; Parse a bare expression by wrapping it in a throwaway define and extracting the body. This keeps
;; one grammar (no separate expression start-rule) while letting the harness parse fragments.
;; parse-failure : exn source -> (err cql-error)   the message is the positioned one-liner; the
;; diagnostic rides along. #:prefix: characters the expression entry prepends on line 1.
(define (parse-failure e source #:prefix [prefix 0])
  (define d0 (exn->diagnostic e 'cql #:source source))
  (define d (let ([l (diagnostic-loc d0)])
              (if (and l (> prefix 0) (eqv? (loc-line l) 1) (loc-column l))
                  (struct-copy diagnostic d0 [loc (struct-copy loc l [column (max 1 (- (loc-column l) prefix))]
                                                                  [position (and (loc-position l) (max 1 (- (loc-position l) prefix)))])])
                  d0)))
  (err (cql-error 'parse (diagnostic->string d) d)))

(define (parse-cql-expr str)
  (cached expr-cache str expr-cache-limit
          (lambda ()
            (with-handlers ([exn:fail? (lambda (e) (parse-failure e #f #:prefix (string-length expr-prefix)))])
              (define lib (lower-library
                           (g:parse-to-datum
                            (tokenize (open-input-string (string-append expr-prefix str))))))
              (define defs (cql-library-defs lib))
              (cond
                [(and (pair? defs) (cql-def? (car defs))) (ok (cql-def-body (car defs)))]
                [else (err (cql-error 'parse "expected a single expression"))])))))

(module+ test
  (require rackunit (only-in cql/error cql-error-string)
           (only-in fhir/result/result ok? ok-value err? err-error))
  (define (P s) (ok-value (parse-cql-expr s)))
  ;; literals
  (check-equal? (P "1") (cql-lit 1))
  (check-equal? (P "1.50") (cql-lit (dec-lit "1.50")))
  (check-equal? (P "42L") (cql-lit (long-lit "42")))
  (check-equal? (P "'hello'") (cql-lit "hello"))
  (check-equal? (P "true") (cql-lit #t))
  (check-equal? (P "null") (cql-lit 'null))
  (check-equal? (P "@2014-01-25") (cql-lit (date-lit "2014-01-25")))
  ;; arithmetic precedence: 1 + 2 * 3 => 1 + (2 * 3)
  (check-equal? (P "1 + 2 * 3")
                (cql-binop '+ (cql-lit 1) (cql-binop '* (cql-lit 2) (cql-lit 3))))
  ;; power binds tighter than *
  (check-equal? (P "2 * 3 ^ 2")
                (cql-binop '* (cql-lit 2) (cql-binop '^ (cql-lit 3) (cql-lit 2))))
  ;; comparison / equality / logical
  (check-equal? (P "1 = 1") (cql-binop '= (cql-lit 1) (cql-lit 1)))
  (check-equal? (P "3 > 2") (cql-binop '> (cql-lit 3) (cql-lit 2)))
  (check-equal? (P "true and false") (cql-binop 'and (cql-lit #t) (cql-lit #f)))
  (check-equal? (P "a or b and c")
                (cql-binop 'or (cql-ref 'a) (cql-binop 'and (cql-ref 'b) (cql-ref 'c))))
  ;; unary not / minus
  (check-equal? (P "not true") (cql-unop 'not (cql-lit #t)))
  (check-equal? (P "-3") (cql-unop '- (cql-lit 3)))
  ;; string concat
  (check-equal? (P "'a' + 'b'") (cql-binop '+ (cql-lit "a") (cql-lit "b")))
  ;; quantity
  (check-equal? (P "5 'mg'") (cql-lit (qty-lit 5 "mg")))
  (check-equal? (P "3 months") (cql-lit (qty-lit 3 "months")))
  ;; list / interval / tuple
  (check-equal? (P "{1, 2, 3}") (cql-list (list (cql-lit 1) (cql-lit 2) (cql-lit 3))))
  (check-equal? (P "List{1, 2}") (cql-list (list (cql-lit 1) (cql-lit 2))))
  (check-equal? (P "Interval[1, 10]") (cql-interval (cql-lit 1) (cql-lit 10) #t #t))
  (check-equal? (P "Interval[1, 10)") (cql-interval (cql-lit 1) (cql-lit 10) #t #f))
  (check-equal? (P "Tuple{a: 1, b: 2}")
                (cql-tuple (list (cql-tuple-elem 'a (cql-lit 1)) (cql-tuple-elem 'b (cql-lit 2)))))
  (check-equal? (P "{x: 1}") (cql-tuple (list (cql-tuple-elem 'x (cql-lit 1)))))
  ;; membership + interval
  (check-equal? (P "5 in Interval[1, 10]")
                (cql-binop 'in (cql-lit 5) (cql-interval (cql-lit 1) (cql-lit 10) #t #t)))
  ;; if / case
  (check-equal? (P "if true then 1 else 2") (cql-if (cql-lit #t) (cql-lit 1) (cql-lit 2)))
  ;; open terms (query, if) extend as far as they can: the else takes the call, the where takes the
  ;; comparison and the `or`, and an if may be a right operand
  (check-equal? (P "if c then 1 else L.f(y)")
                (cql-if (cql-ref 'c) (cql-lit 1) (cql-call (cql-ref 'L) 'f (list (cql-ref 'y)))))
  (check-equal? (P "if c then null else (xs) i where i > 1")
                (cql-if (cql-ref 'c) (cql-lit 'null)
                        (cql-query (list (cql-source 'i (cql-ref 'xs))) '()
                                   (cql-binop '> (cql-ref 'i) (cql-lit 1)) #f #f #f '())))
  (check-equal? (P "exists (xs) a where p or q")
                (cql-unop 'exists (cql-query (list (cql-source 'a (cql-ref 'xs))) '()
                                             (cql-binop 'or (cql-ref 'p) (cql-ref 'q)) #f #f #f '())))
  (check-equal? (P "1 + if c then 2 else 3 * 4")
                (cql-binop '+ (cql-lit 1) (cql-if (cql-ref 'c) (cql-lit 2) (cql-binop '* (cql-lit 3) (cql-lit 4)))))
  ;; a query as the right operand of membership; distinct/flatten of a query
  (check-equal? (P "x in (rs) r return r.d")
                (cql-binop 'in (cql-ref 'x)
                           (cql-query (list (cql-source 'r (cql-ref 'rs))) '() #f
                                      (cql-return #f (cql-property (cql-ref 'r) 'd)) #f #f '())))
  (check-equal? (P "distinct (xs) s sort asc")
                (cql-call #f 'Distinct (list (cql-query (list (cql-source 's (cql-ref 'xs))) '() #f #f 'asc #f '()))))
  ;; flatten of a clause-less query takes postfix tails: (flatten (xs) i).f(p)
  (check-equal? (P "flatten (xs) i.f(p)")
                (cql-call (cql-call #f 'Flatten (list (cql-query (list (cql-source 'i (cql-ref 'xs))) '() #f #f #f #f '())))
                          'f (list (cql-ref 'p))))
  (check-equal? (P "case when true then 1 else 2 end")
                (cql-case #f (list (cql-case-item (cql-lit #t) (cql-lit 1))) (cql-lit 2)))
  ;; navigation + function call
  (check-equal? (P "X.value") (cql-property (cql-ref 'X) 'value))
  (check-equal? (P "ToString(5)") (cql-call #f 'ToString (list (cql-lit 5))))
  (check-equal? (P "X.first()") (cql-call (cql-ref 'X) 'first '()))
  ;; type ops
  (check-equal? (P "5 is Integer") (cql-typeop 'is (cql-lit 5) (named-type 'Integer)))
  (check-equal? (P "convert 5 to String") (cql-convert (cql-lit 5) (named-type 'String)))
  (check-equal? (P "cast x as Quantity") (cql-cast (cql-ref 'x) (named-type 'Quantity)))
  ;; set operators
  (check-equal? (P "{1} union {2}")
                (cql-binop 'union (cql-list (list (cql-lit 1))) (cql-list (list (cql-lit 2)))))

  ;; ---- library parsing ----
  (define lib (ok-value (parse-cql "library Demo version '1.0'\ndefine X: 1 + 1\ndefine private Y: X * 2")))
  (check-equal? (cql-library-id lib) 'Demo)
  (check-equal? (cql-library-version lib) "1.0")
  (check-equal? (length (cql-library-defs lib)) 2)
  (check-equal? (cql-def-name (car (cql-library-defs lib))) 'X)
  (check-equal? (cql-def-access (cadr (cql-library-defs lib))) 'private)
  ;; function def
  (define lib2 (ok-value (parse-cql "define function Add(a Integer, b Integer) returns Integer: a + b")))
  (define f (car (cql-library-defs lib2)))
  (check-true (cql-func? f))
  (check-equal? (cql-func-name f) 'Add)
  (check-equal? (map cql-operand-name (cql-func-operands f)) '(a b))
  ;; parse error surfaces as (err cql-error)
  (check-true (err? (parse-cql-expr "1 +")))

  ;; ---- terminology definitions, retrieves, relationships, sort by, timing phrases (cql.g4) ----
  (define (L s) (cql-library-defs (ok-value (parse-cql s))))
  (check-equal? (L "codesystem \"LOINC\": 'http://loinc.org'\ncode \"BD\": '21112-8' from \"LOINC\" display 'Birth date'")
                (list (cql-codesystem 'LOINC "http://loinc.org" #f 'public)
                      (cql-code-def 'BD "21112-8" (cql-ref 'LOINC) "Birth date" 'public)))
  (check-equal? (L "private valueset \"V\": 'urn:x' codesystems { \"CS\" }")
                (list (cql-valueset 'V "urn:x" #f (list (cql-ref 'CS)) 'private)))
  (check-equal? (L "concept \"C\": { \"A\", Lib.\"B\" } display 'c'")
                (list (cql-concept-def 'C (list (cql-ref 'A) (cql-property (cql-ref 'Lib) 'B)) "c" 'public)))
  (check-true (cql-func-fluent? (car (L "define fluent function version(c String): c"))))
  (check-equal? (P "[Encounter]") (cql-retrieve #f 'Encounter #f #f #f))
  (check-equal? (P "[\"Condition\": code in \"VS\"]") (cql-retrieve #f 'Condition 'code 'in (cql-ref 'VS)))
  (let ([q (P "[Encounter] E with [Condition] C such that C.onset during E.period sort by start of period desc")])
    (check-equal? (cql-relationship-kind (car (cql-query-rels q))) 'with)
    (check-equal? (map cql-sort-item-direction (cql-query-sort q)) '(desc)))
  ;; keywords as member names, tuple keys and qualified-name segments
  (check-equal? (P "period.start") (cql-property (cql-ref 'period) 'start))
  (check-equal? (cql-instance-type (P "System.Code { code: '1' }")) 'System.Code)
  (check-equal? (map cql-tuple-elem-name (cql-tuple-elements (P "Tuple { start: 1, display: 'x' }"))) '(start display))
  ;; timing phrases: starts/ends bounds, quantity offsets, within
  (check-equal? (P "A starts before end B")
                (cql-timing 'before #f (cql-call #f 'Start (list (cql-ref 'A))) (cql-call #f 'End (list (cql-ref 'B)))))
  (check-equal? (P "A 3 days or less on or before day of B")
                (cql-timing-offset 'same-or-before 'day (qty-lit 3 "days") 'or-less (cql-ref 'A) (cql-ref 'B)))
  (check-true (cql-within? (P "A within 3 days of B")))
  (check-equal? (P "X is true") (cql-call #f 'IsTrue (list (cql-ref 'X))))
  (check-equal? (P "{ : }") (cql-tuple '()))
  ;; diagnostics
  (define (diag src) (cql-error-string (err-error (parse-cql src #:source "t.cql"))))
  (check-equal? (diag "library T\ndefine X: 1 + \ndefine Y: 2\n") "t.cql:2:13: cql parse error: the definition ends after `+`")
  (check-equal? (diag "library T\ndefine X: (1 + )\n") "t.cql:2:16: cql parse error: unexpected `)`")
  (check-equal? (cql-error-string (err-error (parse-cql-expr "1 + * 2"))) "<input>:1:5: cql parse error: unexpected `*`")
  ;; a quantity's unit is a calendar word or a string (cql.g4's `unit`): `5 mg` is no quantity — it
  ;; was read as `5 'mg'` — and the error is at the word
  (check-equal? (P "5 days") (cql-lit (qty-lit 5 "days")))
  (check-equal? (P "5 'mg'") (cql-lit (qty-lit 5 "mg")))
  (check-equal? (diag "library T\ndefine X: 1\ndefine Y:\n  4 'mg' +\n  5 mg\n")
                "t.cql:5:5: cql parse error: a quantity's unit is written in quotes: 5 'mg' (only the calendar words — years, months, … milliseconds — are written bare)")
  (check-regexp-match #rx"a quantity's unit is written in quotes: 2.5 'mL'" (cql-error-string (err-error (parse-cql-expr "2.5 mL"))))

  ;; ---- a definition parsed once: the same definition in two texts, each at its own place -----
  (let* ([a (ok-value (parse-cql "library A\ndefine \"Shared\": 1 + 2\n" #:source "a.cql"))]
         [b (ok-value (parse-cql "library B\n\n\n\ndefine \"Shared\": 1 + 2\n" #:source "b.cql"))]
         [da (car (cql-library-defs a))] [db (car (cql-library-defs b))])
    (check-equal? da db)
    (check-false (eq? da db))                                   ; a definition struct of its own …
    (check-true (eq? (cql-def-body da) (cql-def-body db)))      ; … over the one parsed body
    (check-equal? (map (lambda (d) (list (loc-source (attached-loc d)) (loc-line (attached-loc d)))) (list da db))
                  '(("a.cql" 2) ("b.cql" 5))))
  ;; comments and spacing are not part of what a definition parses to
  (check-equal? (chunk-key (car (definition-chunks (open-input-string "define X: /* c */ 1 +  2 // d"))))
                (chunk-key (car (definition-chunks (open-input-string "define X: 1 + 2")))))
  ;; a cache that reaches its limit is emptied and filled again
  (let ([t (make-hash)] [computed 0])
    (define (get k) (cached t k 2 (lambda () (set! computed (add1 computed)) (* k k))))
    (check-equal? (list (get 1) (get 1) (get 2) (get 3) (get 1)) '(1 1 4 9 1))
    (check-equal? computed 4)
    (check-equal? (hash-count t) 2))

  ;; ---- a definition that is one long list, parsed element by element --------------------------
  (define (chunk-of text) (car (definition-chunks (open-input-string text))))
  (define (whole chunk) (lower-library (g:parse-to-datum (list->token-thunk chunk))))
  (define long-list
    (string-append "define \"Table\": {\n"
                   (apply string-append
                          (for/list ([i (in-range 40)])
                            (format "  { code: ~a, name: 'n~a', range: Interval[~a, ~a), tags: { 'a', 'b' }, top: Max({ ~a, 2 }) }~a\n"
                                    i i i (+ i 5) i (if (= i 39) "" ","))))
                   "}"))
  (check-equal? (parse-long-list (chunk-of long-list)) (whole (chunk-of long-list)))
  (check-equal? (length (cql-list-elements (cql-def-body (car (cql-library-defs (parse-long-list (chunk-of long-list))))))) 40)
  (check-equal? (parse-long-list (chunk-of "define function F(a List<Integer>, b Tuple { x Integer }): { a, { b }, 1 + 2 }"))
                (whole (chunk-of "define function F(a List<Integer>, b Tuple { x Integer }): { a, { b }, 1 + 2 }")))
  (check-equal? (parse-long-list (chunk-of "define private X: { 1 }")) (whole (chunk-of "define private X: { 1 }")))
  ;; what is not exactly a list is left to the whole-definition parse: a tuple, a list inside a larger
  ;; expression, a typed list, an empty list, an element that does not parse
  (check-false (parse-long-list (chunk-of "define X: { a: 1, b: 2 }")))
  (check-false (parse-long-list (chunk-of "define X: { 1, 2 } union { 3 }")))
  (check-false (parse-long-list (chunk-of "define X: List<Integer> { 1, 2 }")))
  (check-false (parse-long-list (chunk-of "define X: { }")))
  (check-false (parse-long-list (chunk-of "define X: { 1, 2 + }")))
  (check-false (parse-long-list (chunk-of "define X: { 1, , 2 }")))
  (check-false (parse-long-list (chunk-of "parameter P default { 1, 2 }")))
  (check-false (parse-long-list (chunk-of "define X: { 1, 2, 3 }") #:at-least 4))
  ;; past the thresholds a library takes that path (1,500 elements: a second or two; parsed whole,
  ;; half a minute), with the errors it had
  (define (numbers n tail) (string-append "library Big\ndefine \"T\": { " (apply string-append (for/list ([i (in-range n)]) (format "~a, " i))) tail " }\n"))
  (check-equal? (length (cql-list-elements (cql-def-body (car (cql-library-defs (ok-value (parse-cql (numbers 1500 "0")))))))) 1501)
  (check-regexp-match #rx"^t.cql:2:[0-9]+: cql parse error: unexpected `}`"
                      (cql-error-string (err-error (parse-cql (numbers 150 "0 +") #:source "t.cql")))))
