#lang racket/base

;;; harness.rkt — the data-driven CQL conformance runner.
;;;
;;; Reads a cql-tests-format XML suite (<tests><group><test><expression>…</expression>
;;; <output>…</output></test></group></tests>), and for each test compiles the CQL <expression> to
;;; ELM, RUNS the ELM (never CQL directly), and compares the resulting value to the interpreted
;;; <output> — exactly the compile→interpret→compare-expected loop the FHIRPath TCK uses. A test with
;;; invalidType="semantic"/"invalid" is expected to fail compilation/evaluation.

(require racket/contract racket/list xml
         (only-in "../parser/parse.rkt" parse-cql-expr)
         (only-in "../compile.rkt" compile-expr)
         (only-in "../interp.rkt" eval-elm make-ienv test-equal?)
         (only-in "../value.rkt" cql-null? render)
         (only-in "../error.rkt" cql-error?)
         (only-in fhir/result/result ok? ok-value err-error))

(provide (contract-out [run-suite (-> path-string? any)]))   ; -> (values passed total failures skipped)

;; ---- XML helpers (same shape as the FHIRPath harness) ---------------------
(define (children e name)
  (filter (lambda (c) (and (element? c) (eq? (element-name c) name))) (element-content e)))
(define (child e name) (let ([cs (children e name)]) (and (pair? cs) (car cs))))
(define (attr e name) (let ([a (assq name (map (lambda (a) (cons (attribute-name a) (attribute-value a)))
                                               (element-attributes e)))]) (and a (cdr a))))
(define (elem-text e) (string-trim* (apply string-append (for/list ([c (element-content e)] #:when (pcdata? c)) (pcdata-string c)))))
(define (string-trim* s)
  (regexp-replace #px"\\s+$" (regexp-replace #px"^\\s+" s "") ""))
(define (all-tests doc)
  (let loop ([x (document-element doc)])
    (cond [(and (element? x) (eq? (element-name x) 'test)) (list x)]
          [(element? x) (append-map loop (element-content x))]
          [else '()])))

;; ---- evaluate one CQL expression string -> (values value error?) -----------
(define (eval-cql str)
  (with-handlers ([cql-error? (lambda (e) (values #f e))]
                  [exn:fail? (lambda (e) (values #f e))])
    (define pr (parse-cql-expr str))
    (cond
      [(not (ok? pr)) (values #f (err-error pr))]
      [else
       (define cr (compile-expr (ok-value pr)))
       (cond
         [(not (ok? cr)) (values #f (err-error cr))]
         [else (values (eval-elm (ok-value cr) (make-ienv)) #f)])])))

;; ---- run a suite ----------------------------------------------------------
;; -> (values passed total failures skipped). `total` counts only attempted (expression) tests;
;; library-style tests (out of the expression harness's scope) are skipped and reported separately.
(define (run-suite xml-path)
  (define doc (call-with-input-file xml-path read-xml))
  (define tests (all-tests doc))
  (define failures '())
  (define passed 0)
  (define attempted 0)
  (define skipped 0)
  (for ([t (in-list tests)])
    (define name (or (attr t 'name) "?"))
    (define expr-el (child t 'expression))
    (cond
      [(not expr-el) (set! skipped (add1 skipped))]        ; a <library> test — deferred slice
      [else
       (set! attempted (add1 attempted))
       (define invalid (attr expr-el 'invalid))            ; "true" | "semantic" | "syntax" | #f
       (define expr (elem-text expr-el))
       (define out (let ([o (child t 'output)]) (and o (elem-text o))))
       (define-values (val vex) (eval-cql expr))
       (cond
         ;; a test marked invalid passes iff our compile/eval fails
         [invalid (if vex (set! passed (add1 passed))
                      (set! failures (cons (list name expr "expected failure but got" (safe-render val)) failures)))]
         ;; error where a value was expected
         [vex (set! failures (cons (list name expr "error" (if (cql-error? vex) (format "~a" vex) (exn-message vex))) failures))]
         ;; no <output> means "no error expected" (value is whatever)
         [(not out) (set! passed (add1 passed))]
         [else
          (define-values (exp eex) (eval-cql out))
          (cond
            [eex (set! failures (cons (list name expr "bad expected-output" out) failures))]
            [(test-equal? val exp) (set! passed (add1 passed))]
            [else (set! failures (cons (list name expr (safe-render exp) (safe-render val)) failures))])])]))
  (values passed attempted (reverse failures) skipped))

(define (safe-render v) (with-handlers ([exn:fail? (lambda (_) "<unrenderable>")]) (if (cql-null? v) "null" (render v))))

(module+ test
  (require rackunit racket/runtime-path)
  (define-runtime-path vendor "vendor")
  (define-values (passed total failures skipped) (run-suite (build-path vendor "cql-smoke.xml")))
  (printf "cql harness smoke: ~a/~a (skipped ~a)\n" passed total skipped)
  (for ([f (in-list failures)]) (printf "  FAIL ~a: ~s -> expected ~a got ~a\n" (car f) (cadr f) (caddr f) (cadddr f)))
  (check-equal? passed total "all smoke tests pass"))
