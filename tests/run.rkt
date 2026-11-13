#lang racket/base

;;; run.rkt — the `raco cql --tck` entry point: run the vendored OFFICIAL CQL conformance suites
;;; (cqframework/cql-tests) and report pass/total per suite + overall, ratcheting a floor. Mirrors
;;; fhirpath/tests/run-r5.rkt. A library-style test (one without an <expression>) is counted as
;;; skipped; the vendored suites have none.

(require racket/runtime-path
         (only-in racket/path file-name-from-path)
         (only-in "harness.rkt" run-suite))

(define-runtime-path suite-dir "vendor/cql-tests")

(module+ main
  (define verbose? (member "-v" (vector->list (current-command-line-arguments))))
  (define suites (sort (for/list ([f (in-list (directory-list suite-dir))]
                                  #:when (regexp-match? #px"\\.xml$" (path->string f)))
                         (build-path suite-dir f))
                       string<? #:key path->string))
  (define-values (tp ta ts all-failures)
    (for/fold ([tp 0] [ta 0] [ts 0] [fs '()]) ([s (in-list suites)])
      (define-values (p a failures skipped) (run-suite s))
      (printf "~a: ~a/~a~a\n" (path->string (file-name-from-path s)) p a
              (if (> skipped 0) (format " (skipped ~a)" skipped) ""))
      (values (+ tp p) (+ ta a) (+ ts skipped)
              (append fs (map (lambda (f) (cons (path->string (file-name-from-path s)) f)) failures)))))
  (when verbose?
    (for ([f (in-list all-failures)])
      (printf "  FAIL [~a] ~a: ~s -> expected ~a got ~a\n"
              (car f) (cadr f) (caddr f) (cadddr f) (list-ref f 4))))
  (printf "CQL conformance: ~a/~a  (skipped ~a library-style tests)\n" tp ta ts)
  ;; the ratchet. The 3 misses are the corpus's own (TESTING.md names them): 2x it contradicts its
  ;; literal-range rule (Floor(2147483648) expects a literal ValueLiteralsAndSelectors requires to be
  ;; an error), 1x a standalone uncertainty bound ([17, 44]) against its own arithmetic ([16, 44]).
  (define floor 1820)
  (unless (>= tp floor)
    (eprintf "CQL conformance regressed: ~a < floor ~a\n" tp floor)
    (exit 1)))
