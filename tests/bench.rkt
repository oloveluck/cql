#lang racket/base

;;; bench.rkt — a wall-clock benchmark for the CQL pipeline, to guard against performance regressions.
;;; Each measurement runs under both evaluators (current-elm-evaluator: the tree walker and the
;;; closure compiler), so the speed-up and any regression in either are visible:
;;;   (1) the full official suite end-to-end (parse + compile + run over every vendored test);
;;;   (2) the isolated eval loop: pre-compiled ELM run N times (dispatch + operator cost);
;;;   (3) real execution: the four eCQMs over their 76 test patients (tests/vendor/ecqm), R rounds —
;;;       retrieves, terminology, FHIRHelpers calls and queries, as a measure evaluator runs them.
;;; `racket tests/bench.rkt`.

(require racket/runtime-path racket/list
         (only-in "harness.rkt" run-suite)
         (only-in "measure-harness.rkt" run-measure-corpus)
         (only-in "../parser/parse.rkt" parse-cql-expr)
         (only-in "../compile.rkt" compile-expr)
         (only-in "../interp.rkt" run-elm make-ienv current-elm-evaluator)
         (only-in fhir/result/result ok? ok-value))

(define-runtime-path suite-dir "vendor/cql-tests")
(define-runtime-path ecqm "vendor/ecqm")

(define (compile-one s)
  (define pr (parse-cql-expr s))
  (and (ok? pr) (let ([cr (compile-expr (ok-value pr))]) (and (ok? cr) (ok-value cr)))))

;; timed : label thunk -> real-ms   prints cpu/real/gc and returns real
(define (timed label thunk)
  (collect-garbage)
  (define-values (_r cpu real gc) (time-apply thunk '()))
  (printf "  ~a cpu=~ams real=~ams gc=~ams\n" label cpu real gc)
  real)

(define (both title thunk)
  (printf "== ~a ==\n" title)
  (define t (parameterize ([current-elm-evaluator 'tree]) (timed "tree     " thunk)))
  (define c (parameterize ([current-elm-evaluator 'compiled]) (timed "compiled " thunk)))
  (printf "  speed-up ~ax\n" (if (zero? c) "∞" (real->decimal-string (/ t c) 2))))

(module+ main
  (define suites (for/list ([f (in-list (directory-list suite-dir))]
                            #:when (regexp-match? #px"\\.xml$" (path->string f)))
                   (build-path suite-dir f)))
  ;; warm the caches (parse and compile memoise), so every timing below is of evaluation
  (for ([s (in-list suites)]) (run-suite s))
  (void (run-measure-corpus ecqm))
  ;; (1) end-to-end over the whole suite
  (both "end-to-end (full official suite)" (lambda () (for ([s (in-list suites)]) (run-suite s))))
  ;; (2) isolated eval: compile a representative in-scope set once, then evaluate it N times
  (define exprs '("1 + 2 * 3" "3.0 / 4 + 1.5" "'a' + 'b' + 'c'" "1 = 1 and 2 < 3 or false"
                  "{1,2,3,4,5} union {3,4,5,6}" "5 in Interval[1, 10]"
                  "if 1 < 2 then 'x' else 'y'" "Coalesce(null, null, 7)"
                  "(from ({1,2,3,4,5,6,7,8}) X where X mod 2 = 0 return X * X)"))
  (define nodes (filter-map compile-one exprs))
  (define env (make-ienv))
  (define N 20000)
  (both (format "isolated eval (~a exprs x ~a iters)" (length nodes) N)
        (lambda () (for ([_ (in-range N)]) (for ([n (in-list nodes)]) (run-elm n env)))))
  ;; (3) the eCQM corpus, R rounds
  (define R 5)
  (both (format "eCQM execution (76 patients x ~a rounds)" R)
        (lambda () (for ([_ (in-range R)]) (run-measure-corpus ecqm)))))
