#lang racket/base

;;; parse.rkt — the CQL parse API: string -> Result<AST>.
;;;
;;; Mirrors fhirpath/parser/parse.rkt: string -> tokens (lexer) -> brag datum (grammar) -> compact
;;; prefab AST (lower). `parse-cql` reads a whole library; `parse-cql-expr` reads a single bare
;;; expression (used by the conformance harness on cql-tests `<expression>` fragments). A parse
;;; failure is caught and returned as (err (cql-error 'parse …)). Results are memoized (prefab ASTs
;;; are immutable, so sharing is safe).

(require racket/contract
         (prefix-in g: "grammar.rkt")
         "lexer.rkt"
         "ast.rkt"
         (only-in "../error.rkt" cql-error)
         (only-in fhir/result/result ok err ok? err?))

(provide
 (contract-out
  [parse-cql      (-> string? (or/c ok? err?))]   ; (ok cql-library) | (err cql-error)
  [parse-cql-expr (-> string? (or/c ok? err?))]))  ; (ok expr-AST)   | (err cql-error)

(define lib-cache (make-hash))
(define expr-cache (make-hash))

(define (parse-cql str)
  (hash-ref! lib-cache str
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e) (err (cql-error 'parse (exn-message e))))])
                 (ok (lower-library (g:parse-to-datum (tokenize (open-input-string str)))))))))

;; Parse a bare expression by wrapping it in a throwaway define and extracting the body. This keeps
;; one grammar (no separate expression start-rule) while letting the harness parse fragments.
(define (parse-cql-expr str)
  (hash-ref! expr-cache str
             (lambda ()
               (with-handlers ([exn:fail? (lambda (e) (err (cql-error 'parse (exn-message e))))])
                 (define lib (lower-library
                              (g:parse-to-datum
                               (tokenize (open-input-string (string-append "define __expr__: " str))))))
                 (define defs (cql-library-defs lib))
                 (cond
                   [(and (pair? defs) (cql-def? (car defs))) (ok (cql-def-body (car defs)))]
                   [else (err (cql-error 'parse "expected a single expression"))])))))

(module+ test
  (require rackunit
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
  (check-true (err? (parse-cql-expr "1 +"))))
