#lang racket/base

;;; coverage.rkt — the iteration measurement tool: run every vendored official suite and bucket the
;;; failures (parse / translate / interpret / value-mismatch / should-fail), rank the unimplemented
;;; operators, and surface grammar-gap patterns. `racket cql/tests/coverage.rkt` (or the `main`
;;; submodule) prints the report used to steer each loop iteration. Read-only; no floor/exit.

(require racket/runtime-path racket/list racket/string racket/format
         (only-in "harness.rkt" run-suite))

(define-runtime-path suite-dir "vendor/cql-tests")

;; phase-of : string -> symbol   the cql-error phase, or 'interpret for a bare exn
(define (phase-of detail)
  (cond [(regexp-match #px"struct:cql-error (\\w+)" detail) => (lambda (m) (string->symbol (cadr m)))]
        [else 'interpret]))

;; op-name : string -> string    a canonical label for an unimplemented-operator failure
(define (op-name detail)
  (cond
    [(regexp-match #px"unknown function '([^']+)'" detail) => (lambda (m) (format "fn ~a" (cadr m)))]
    [(regexp-match #px"unknown operator '?([^'\\)]+)'?" detail) => (lambda (m) (format "op ~a" (string-trim (cadr m))))]
    [(regexp-match #px"unsupported[^:]*: (.*)\\)" detail) => (lambda (m) (string-normalize-spaces (cadr m)))]
    [(regexp-match #px"cql-error \\w+ (.*)\\)" detail) => (lambda (m) (string-normalize-spaces (cadr m)))]
    [else (string-normalize-spaces detail)]))

(define (bump! h k) (hash-update! h k add1 0))

(module+ main
  (define suites (sort (for/list ([f (in-list (directory-list suite-dir))]
                                  #:when (regexp-match? #px"\\.xml$" (path->string f)))
                         (path->string f)) string<?))
  (define op-counts (make-hash))
  (define parse-ex (make-hash))
  (define G (make-hash))
  (define-values (TP TA TS) (values 0 0 0))
  (printf "~a\n~a ~a ~a ~a ~a ~a\n~a\n"
          (make-string 84 #\=)
          (~a "suite" #:width 36) (~a "pass" #:width 9) (~a "parse" #:width 6)
          (~a "transl" #:width 7) (~a "interp" #:width 7) (~a "vmism" #:width 6)
          (make-string 84 #\=))
  (for ([sf (in-list suites)])
    (define-values (p a fails skipped) (run-suite (build-path suite-dir sf)))
    (set! TP (+ TP p)) (set! TA (+ TA a)) (set! TS (+ TS skipped))
    (define b (make-hash))
    (for ([f (in-list fails)])
      (define kind (caddr f)) (define detail (cadddr f)) (define expr (cadr f))
      (cond
        [(string=? kind "error")
         (define ph (phase-of detail))
         (bump! b ph) (bump! G ph)
         (cond [(eq? ph 'translate) (bump! op-counts (op-name detail))]
               [(eq? ph 'parse) (hash-ref! parse-ex expr expr)])]
        [(string=? kind "expected failure but got") (bump! b 'shdfail) (bump! G 'shdfail)]
        [(string=? kind "bad expected-output") (bump! b 'bad) (bump! G 'bad)]
        [else (bump! b 'vmism) (bump! G 'vmism)]))
    (printf "~a ~a ~a ~a ~a ~a\n"
            (~a sf #:width 36) (~a (format "~a/~a" p a) #:width 9)
            (~a (hash-ref b 'parse 0) #:width 6) (~a (hash-ref b 'translate 0) #:width 7)
            (~a (hash-ref b 'interpret 0) #:width 7) (~a (hash-ref b 'vmism 0) #:width 6)))
  (printf "~a\nTOTAL ~a/~a  parse=~a translate=~a interpret=~a vmism=~a shdfail=~a\n"
          (make-string 84 #\=) TP TA
          (hash-ref G 'parse 0) (hash-ref G 'translate 0) (hash-ref G 'interpret 0)
          (hash-ref G 'vmism 0) (hash-ref G 'shdfail 0))
  (printf "\n### top unimplemented operators (translate errors)\n")
  (for ([kv (in-list (take (sort (hash->list op-counts) > #:key cdr) (min 30 (hash-count op-counts))))])
    (printf "  ~a  ~a\n" (~a (cdr kv) #:width 4) (car kv)))
  (printf "\n### sample parse-gap expressions (~a distinct)\n" (hash-count parse-ex))
  (for ([e (in-list (take (sort (hash-keys parse-ex) string<?) (min 40 (hash-count parse-ex))))])
    (printf "  ~s\n" e)))
