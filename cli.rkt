#lang racket/base

;;; cli.rkt — `raco cql`: the command line for the CQL package.
;;;
;;;   raco cql <file.cql> [--libraries <dir>]
;;;                            compile a CQL library to ELM and print the ELM JSON; includes are found in
;;;                            --libraries (default: beside the file), so included functions type-check
;;;                            (a leading `#lang cql` line is ignored, so both raw .cql and #lang files work)
;;;   raco cql <file.cql> --data <dir|file.json> [--valuesets <dir>] [--libraries <dir>] [--define <name>]…
;;;            [--parameter "name=<CQL expression>"]… [--now <date|DateTime>]   (Now()/Today() fixed: 2026-01-01 or 2026-01-01T09:00:00Z)
;;;                            RUN the library over FHIR data (resources or Bundles; patient context),
;;;                            includes found in --libraries (default: beside the file), terminology from
;;;                            the value sets in --valuesets; prints each definition (or the named ones)
;;;   raco cql --tck [-v]      run the vendored OFFICIAL cqframework/cql-tests suites (floor-ratcheted;
;;;                            exit 1 on regression; -v lists failures)
;;; Errors are file:line:col diagnostics on stderr; --format json|outcome prints them on stdout as
;;; {"diagnostics":[…]} or a FHIR OperationOutcome (fhir/diagnostic).
;;; Exit codes: 0 ok · 1 CQL error / regression · 2 usage.

(require (only-in racket/file file->string)
         (only-in racket/match match)
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library compile-library/errors compile-library/diagnostics current-library-resolver)
         (only-in cql/elm/json elm->json-string)
         (only-in cql/error cql-error-string)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in cql/error cql-error? cql-error-diagnostic cql-error-phase cql-error-message at-the-name)
         (only-in fhir/diagnostic diagnostic loc emit-diagnostics format-flag)
         (only-in cql/run load-runtime-library resources->data valuesets->terminology read-json-files rlib-value bundled-library cql-value)
         (only-in cql/parser/ast cql-library-defs cql-def? cql-def-name)
         (only-in cql/value render current-evaluation-time)
         (only-in json read-json))

(provide compile-file run-file)

;; run-file : file data valuesets|#f libraries|#f (listof name) [params] -> void   evaluate and print
;; definitions; params: (listof "name=<CQL expression>")
(define (run-file file data vs-dir lib-dir names [params '()])
  (define res (library-resolver file lib-dir))
  (define lib (parse-file file))
  (define resources (if (directory-exists? data) (read-json-files data)
                        (let ([j (call-with-input-file data read-json)]) (if (list? j) j (list j)))))
  (define parameters
    (for/hash ([p (in-list params)])
      (match (regexp-match #px"^([^=]+)=(.*)$" p)
        [(list _ name text)
         (values name (with-handlers ([exn:fail? (lambda (e) (eprintf "cql: --parameter ~a: ~a\n" name (exn-message e)) (exit 2))])
                        (cql-value text)))]
        [_ (eprintf "cql: --parameter takes name=<CQL expression>\n") (exit 2)])))
  (define rl (with-handlers ([exn:fail? (lambda (e) (eprintf "~a\n" (exn-message e)) (exit 1))])
               (load-runtime-library lib #:resolver res #:data (resources->data resources) #:parameters parameters
                                     #:valuesets (valuesets->terminology (if vs-dir (read-json-files vs-dir) '())))))
  (define defs (or (and (pair? names) names)
                   (for/list ([d (in-list (cql-library-defs lib))] #:when (cql-def? d)) (symbol->string (cql-def-name d)))))
  (for ([n (in-list defs)])
    (printf "~a: ~a\n" n (with-handlers ([cql-error? cql-error-string]) (render (rlib-value rl n))))))

;; parse-file : path-string -> cql-library   or exit 1 with the parse diagnostic
(define (parse-file f)
  (let* ([src (read-source f)] [r (parse-cql src #:source (if (path? f) (path->string f) f))])
    (unless (ok? r) (fail-with (err-error r) f src))
    (ok-value r)))

;; library-resolver : path-string dir|#f -> (name version -> cql-library|#f)   includes as <Name>.cql in
;; lib-dir, or beside `file`; else a bundled one (FHIRHelpers)
(define (library-resolver file lib-dir)
  (define libdir (or lib-dir (let-values ([(d _n _m) (split-path (path->complete-path file))]) d)))
  (lambda (n v) (let ([g (build-path libdir (format "~a.cql" n))]) (if (file-exists? g) (parse-file g) (bundled-library n v)))))

;; compile-file : path-string [dir|#f] -> void   print the ELM JSON of the library in `file`, or exit 1
;; the output format for errors (--format text|json|outcome)
(define current-format (make-parameter 'text))
;; fail-with : cql-error file text -> exit 1   a positioned diagnostic when the error has one, else a
;; diagnostic naming the file (translation errors carry no position yet)
(define (fail-with e file text)
  (define d (or (cql-error-diagnostic e)
                (diagnostic 'cql (case (cql-error-phase e) [(translate) 'compile] [(interpret) 'run] [else (cql-error-phase e)])
                            'error (cql-error-message e) (loc file #f #f #f #f))))
  (emit-diagnostics (list d) #:format (current-format) #:text-of (lambda (_) text))
  (exit 1))
(define (read-source file) (regexp-replace #px"^#lang[^\n]*" (file->string file) ""))   ; keep line numbers

(define (compile-file file [lib-dir #f])
  (define src (read-source file))
  (define pr (parse-cql src #:source file))
  (unless (ok? pr) (fail-with (err-error pr) file src))
  ;; every translation error, not only the first
  (define-values (elm errors warnings)
    (parameterize ([current-library-resolver (library-resolver file lib-dir)])
      (compile-library/diagnostics (ok-value pr))))
  (unless (and (null? errors) (null? warnings))
    (emit-diagnostics (append (for/list ([e (in-list errors)])
                                (or (let ([d (cql-error-diagnostic e)]) (and d (at-the-name d src)))
                                    (diagnostic 'cql 'compile 'error (cql-error-message e) (loc file #f #f #f #f))))
                              warnings)
                      #:format (current-format) #:text-of (lambda (_) src)))
  (unless (null? errors) (exit 1))
  (displayln (elm->json-string elm)))

(define (usage [code 2])                             ; --help: stdout, exit 0
  ((if (zero? code) printf eprintf) "usage: raco cql <file.cql> [--libraries <dir>]\n       raco cql <file.cql> --data <dir|file.json> [--valuesets <dir>] [--libraries <dir>] [--define <name>]…\n                [--parameter \"<name>=<CQL expression>\"]… [--now <date|DateTime>]   (2026-01-01, or 2026-01-01T09:00:00Z)\n       raco cql --tck [-v]\n")
  (exit code))

(module+ main
  (define-values (fmt args) (format-flag (vector->list (current-command-line-arguments))))
  (current-format fmt)
  ;; an unreadable input file is a one-line error (exit 2), not a stack trace
  (let ([default (uncaught-exception-handler)])
    (uncaught-exception-handler
     (lambda (e)
       (cond [(exn:fail:filesystem? e)
              (define m (regexp-match #px"path: ([^\n]*)" (exn-message e)))
              (eprintf "cql: cannot read ~a\n" (if m (cadr m) (exn-message e)))
              (exit 2)]
             [else (default e)]))))
  (cond
    [(ormap (lambda (a) (member a '("-h" "--help"))) args) (usage 0)]
    [(null? args) (usage)]
    [(equal? (car args) "--tck")
     (parameterize ([current-command-line-arguments (list->vector (cdr args))])
       (dynamic-require '(submod cql/tests/run main) #f))]
    [(regexp-match? #rx"^-" (car args)) (usage)]
    [(member "--data" args)
     (let loop ([as (cdr args)] [data #f] [vs #f] [libs #f] [names '()] [params '()])
       (cond
         [(null? as) (if data (run-file (car args) data vs libs (reverse names) (reverse params)) (usage))]
         [(and (equal? (car as) "--data") (pair? (cdr as))) (loop (cddr as) (cadr as) vs libs names params)]
         [(and (equal? (car as) "--valuesets") (pair? (cdr as))) (loop (cddr as) data (cadr as) libs names params)]
         [(and (equal? (car as) "--libraries") (pair? (cdr as))) (loop (cddr as) data vs (cadr as) names params)]
         [(and (equal? (car as) "--define") (pair? (cdr as))) (loop (cddr as) data vs libs (cons (cadr as) names) params)]
         [(and (equal? (car as) "--parameter") (pair? (cdr as))) (loop (cddr as) data vs libs names (cons (cadr as) params))]
         ;; Now()/Today() at a fixed moment: a run that can be repeated
         [(and (equal? (car as) "--now") (pair? (cdr as)))
          (with-handlers ([exn:fail:contract? (lambda (_) (eprintf "cql: --now takes a date or a DateTime, e.g. 2025-06-30 or 2025-06-30T12:00:00Z\n") (exit 2))])
            (current-evaluation-time (cadr as)))
          (loop (cddr as) data vs libs names params)]
         [else (usage)]))]
    [(null? (cdr args)) (compile-file (car args))]
    [(and (equal? (cadr args) "--libraries") (= (length args) 3)) (compile-file (car args) (caddr args))]
    [else (usage)]))
