#lang racket/base

;;; bench.rkt — a wall-clock benchmark for the CQL pipeline, to guard against performance regressions
;;; across loop iterations. Times (1) the full official suite end-to-end (parse+compile+run over every
;;; vendored test) and (2) the isolated eval loop (pre-compiled ELM, run N times) — since parse is
;;; memoized, (2) measures the dispatch + operator cost the registry refactor targets.
;;; `racket cql/tests/bench.rkt`.

(require racket/runtime-path racket/list
         (only-in "harness.rkt" run-suite)
         (only-in "../parser/parse.rkt" parse-cql-expr)
         (only-in "../compile.rkt" compile-expr)
         (only-in "../interp.rkt" eval-elm make-ienv)
         (only-in fhir/result/result ok? ok-value))

(define-runtime-path suite-dir "vendor/cql-tests")

(define (compile-one s)
  (define pr (parse-cql-expr s))
  (and (ok? pr) (let ([cr (compile-expr (ok-value pr))]) (and (ok? cr) (ok-value cr)))))

(module+ main
  (define suites (for/list ([f (in-list (directory-list suite-dir))]
                            #:when (regexp-match? #px"\\.xml$" (path->string f)))
                   (build-path suite-dir f)))
  ;; (1) end-to-end over the whole suite
  (printf "== end-to-end (parse+compile+run, full official suite) ==\n")
  (define-values (_r cpu real gc)
    (time-apply (lambda () (for ([s (in-list suites)]) (run-suite s))) '()))
  (printf "  cpu=~ams real=~ams gc=~ams\n" cpu real gc)
  ;; (2) isolated eval: compile a representative in-scope set once, then hammer eval-elm
  (define exprs '("1 + 2 * 3" "3.0 / 4 + 1.5" "'a' + 'b' + 'c'" "1 = 1 and 2 < 3 or false"
                  "{1,2,3,4,5} union {3,4,5,6}" "5 in Interval[1, 10]"
                  "if 1 < 2 then 'x' else 'y'" "Coalesce(null, null, 7)"))
  (define nodes (filter-map compile-one exprs))
  (define env (make-ienv))
  (define N 20000)
  (printf "== isolated eval (~a exprs x ~a iters) ==\n" (length nodes) N)
  (define-values (_r2 cpu2 real2 gc2)
    (time-apply (lambda () (for ([_ (in-range N)]) (for ([n (in-list nodes)]) (eval-elm n env)))) '()))
  (printf "  cpu=~ams real=~ams gc=~ams  (~a evals)\n" cpu2 real2 gc2 (* N (length nodes))))
