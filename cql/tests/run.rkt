#lang racket/base

;;; run.rkt — the `raco fhir cql-tck` entry point: run the vendored OFFICIAL CQL conformance suites
;;; (cqframework/cql-tests) and report pass/total per suite + overall, ratcheting a floor. Mirrors
;;; fhirpath/tests/run-r5.rkt. Library-style tests and the deferred CQL slices (retrieves, queries,
;;; timing, terminology) surface as skips/failures until those slices land — that's the honest signal.

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
  ;; ratchet: slice-1 (expression core) against the full official suite. Most remaining failures are
  ;; the deferred slices (DateTime/Interval timing, Aggregates, Queries, quantity/UCUM, boundary ops).
  ;; The 4 remaining misses are documented irreducible corpus edges (see CLAUDE.md): 2× the corpus
  ;; contradicting its own literal-range rules (Floor(2147483648) vs ValueLiterals), 1× a standalone
  ;; uncertainty bound contradicting the same corpus's arithmetic convention, 1× the
  ;; `starts N <unit> or less on or after` timing-phrase family (a whole grammar for one test).
  (define floor 1819)
  (unless (>= tp floor)
    (eprintf "CQL conformance regressed: ~a < floor ~a\n" tp floor)
    (exit 1)))
