#lang racket/base

;;; evaluator-test.rkt — the compiled evaluator against the tree walker, result by result.
;;;
;;; The tree walker (`eval-elm`) is the reference semantics; the compiled evaluator
;;; (`eval-elm/compiled`, interp.rkt's closure compiler) must give the SAME outcome everywhere: the
;;; same value (equal?) or the same error (its message). Pass counts agreeing is not enough — this
;;; compares every individual result, over three corpora:
;;;   - the HL7 CQL suite (tests/vendor/cql-tests): every test's expression and expected output,
;;;     both evaluated in one environment (one `now`, so Now()/Today() agree);
;;;   - the 32 published libraries of the ELM corpus (tests/vendor/elm-corpus): every definition,
;;;     over no data (retrieves are empty, value sets unavailable: errors are outcomes too);
;;;   - the eCQM corpus (tests/vendor/ecqm): every definition of each measure's library over each of
;;;     the 76 test patients' data, with the measures' value sets.
;;; An Outcome is (list 'value v) or (list 'error message).

(require rackunit racket/list racket/string racket/runtime-path xml
         (only-in cql/parser/parse parse-cql-expr)
         (only-in cql/parser/ast cql-library-defs cql-def? cql-def-name)
         (only-in cql/compile compile-expr)
         (only-in cql/interp eval-elm eval-elm/compiled make-ienv current-elm-evaluator)
         (only-in cql/error cql-error? cql-error-string)
         cql/run
         (only-in fhir/result/result ok? ok-value)
         "measure-harness.rkt"
         "elm-corpus-harness.rkt")

(define-runtime-path suite-dir "vendor/cql-tests")
(define-runtime-path elm-corpus "vendor/elm-corpus")
(define-runtime-path ecqm "vendor/ecqm")

;; outcome : (-> CqlValue) -> Outcome
(define (outcome thunk)
  (with-handlers ([cql-error? (lambda (e) (list 'error (cql-error-string e)))]
                  [exn:fail? (lambda (e) (list 'error (exn-message e)))])
    (list 'value (thunk))))

;; differences : (listof (list label Outcome Outcome)) -> (listof …)   the pairs that disagree
(define (differences rows) (filter (lambda (r) (not (equal? (cadr r) (caddr r)))) rows))

(define (report name rows)
  (define bad (differences rows))
  (printf "  evaluator differential, ~a: ~a results, ~a differ\n" name (length rows) (length bad))
  (for ([b (in-list (take bad (min 10 (length bad))))])
    (printf "    ~a\n      tree:     ~s\n      compiled: ~s\n" (car b) (cadr b) (caddr b)))
  (check-equal? (map car bad) '() (format "~a: the compiled evaluator differs from the tree walker" name)))

;; ---- the HL7 suite: every expression and expected output ------------------------------------
(define (all-tests x)
  (cond [(and (element? x) (eq? (element-name x) 'test)) (list x)]
        [(element? x) (append-map all-tests (element-content x))]
        [else '()]))
(define (texts t name)
  (for/list ([c (in-list (element-content t))] #:when (and (element? c) (eq? (element-name c) name)))
    (string-trim (apply string-append (for/list ([p (element-content c)] #:when (pcdata? p)) (pcdata-string p))))))
(define (compile-text s)
  (with-handlers ([(lambda (_) #t) (lambda (_) #f)])
    (define p (parse-cql-expr s))
    (and (ok? p) (let ([c (compile-expr (ok-value p))]) (and (ok? c) (ok-value c))))))

(define suite-rows
  (for*/list ([f (in-list (sort (directory-list suite-dir #:build? #t) string<? #:key path->string))]
              #:when (regexp-match? #rx"[.]xml$" (path->string f))
              [t (in-list (all-tests (document-element (call-with-input-file f read-xml))))]
              [s (in-list (append (texts t 'expression) (texts t 'output)))]
              [elm (in-value (compile-text s))] #:when elm)
    (define env (make-ienv))
    (list s (outcome (lambda () (eval-elm elm env))) (outcome (lambda () (eval-elm/compiled elm env))))))
(check-true (>= (length suite-rows) 3590) "fewer HL7 suite expressions than expected")
(report "HL7 suite" suite-rows)

;; ---- the ELM corpus: every definition of 32 published libraries, no data --------------------
;; definitions : cql-library resolver -> (hash name -> Outcome)   under the current evaluator
(define (definitions lib resolver)
  (define rl (load-runtime-library lib #:resolver resolver))
  (for/hash ([d (in-list (cql-library-defs lib))] #:when (cql-def? d))
    (define name (format "~a" (cql-def-name d)))
    (values name (outcome (lambda () (rlib-value rl name))))))

(define corpus-rows
  (append*
   (for/list ([entry (in-list (corpus-libraries elm-corpus))])
     (define-values (pkg name lib resolver) (apply values entry))
     (define tree (parameterize ([current-elm-evaluator 'tree]) (definitions lib resolver)))
     (define compiled (parameterize ([current-elm-evaluator 'compiled]) (definitions lib resolver)))
     (for/list ([(d o) (in-hash tree)])
       (list (format "~a/~a: ~a" pkg name d) o (hash-ref compiled d #f))))))
(check-true (>= (length corpus-rows) 565) "fewer ELM corpus definitions than expected")
(report "ELM corpus" corpus-rows)

;; ---- the eCQM corpus: every definition, every test patient ----------------------------------
(define ecqm-tree (parameterize ([current-elm-evaluator 'tree]) (measure-corpus-definitions ecqm)))
(define ecqm-compiled (parameterize ([current-elm-evaluator 'compiled]) (measure-corpus-definitions ecqm)))
(check-equal? (length ecqm-tree) 76)
(define ecqm-rows
  (for*/list ([t (in-list ecqm-tree)] [c (in-value (assoc (car t) ecqm-compiled))] [(d o) (in-hash (cdr t))])
    (list (format "~a: ~a" (car t) d) o (and c (hash-ref (cdr c) d #f)))))
(report "eCQM corpus" ecqm-rows)
;; the comparison is not vacuous: definitions evaluate to values, not only errors
(check-true (> (count (lambda (r) (eq? 'value (car (cadr r)))) ecqm-rows) 1000))
