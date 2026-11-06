#lang racket/base

;;; cli.rkt — `raco cql`: the command line for the CQL package.
;;;
;;;   raco cql <file.cql> [--libraries <dir>]
;;;                            compile a CQL library to ELM and print the ELM JSON; includes are found in
;;;                            --libraries (default: beside the file), so included functions type-check
;;;                            (a leading `#lang cql` line is ignored, so both raw .cql and #lang files work)
;;;   raco cql <file.cql> --data <dir|file.json> [--valuesets <dir>] [--libraries <dir>] [--define <name>]…
;;;                            RUN the library over FHIR data (resources or Bundles; patient context),
;;;                            includes found in --libraries (default: beside the file), terminology from
;;;                            the value sets in --valuesets; prints each definition (or the named ones)
;;;   raco cql --tck [-v]      run the vendored OFFICIAL cqframework/cql-tests suites (floor-ratcheted;
;;;                            exit 1 on regression; -v lists failures)
;;; Errors are file:line:col diagnostics on stderr; --format json|outcome prints them on stdout as
;;; {"diagnostics":[…]} or a FHIR OperationOutcome (fhir/diagnostic).
;;; Exit codes: 0 ok · 1 CQL error / regression · 2 usage.

(require (only-in racket/file file->string)
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library compile-library/errors current-library-resolver)
         (only-in cql/elm/json elm->json-string)
         (only-in cql/error cql-error-string)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in cql/error cql-error? cql-error-diagnostic cql-error-phase cql-error-message)
         (only-in fhir/diagnostic diagnostic loc emit-diagnostics format-flag)
         (only-in cql/run load-runtime-library resources->data valuesets->terminology read-json-files rlib-value)
         (only-in cql/parser/ast cql-library-defs cql-def? cql-def-name)
         (only-in cql/value render)
         (only-in json read-json))

(provide compile-file run-file)

;; run-file : file data valuesets|#f libraries|#f (listof name) -> void   evaluate and print definitions
(define (run-file file data vs-dir lib-dir names)
  (define res (library-resolver file lib-dir))
  (define lib (parse-file file))
  (define resources (if (directory-exists? data) (read-json-files data) (list (call-with-input-file data read-json))))
  (define rl (with-handlers ([exn:fail? (lambda (e) (eprintf "~a\n" (exn-message e)) (exit 1))])
               (load-runtime-library lib #:resolver res #:data (resources->data resources)
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
;; lib-dir, or beside `file`
(define (library-resolver file lib-dir)
  (define libdir (or lib-dir (let-values ([(d _n _m) (split-path (path->complete-path file))]) d)))
  (lambda (n _v) (let ([g (build-path libdir (format "~a.cql" n))]) (and (file-exists? g) (parse-file g)))))

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
  (define-values (elm errors)
    (parameterize ([current-library-resolver (library-resolver file lib-dir)])
      (compile-library/errors (ok-value pr))))
  (unless (null? errors)
    (emit-diagnostics (for/list ([e (in-list errors)])
                        (or (cql-error-diagnostic e)
                            (diagnostic 'cql 'compile 'error (cql-error-message e) (loc file #f #f #f #f))))
                      #:format (current-format) #:text-of (lambda (_) src))
    (exit 1))
  (displayln (elm->json-string elm)))

(define (usage [code 2])                             ; --help: stdout, exit 0
  ((if (zero? code) printf eprintf) "usage: raco cql <file.cql> [--libraries <dir>]\n       raco cql <file.cql> --data <dir|file.json> [--valuesets <dir>] [--libraries <dir>] [--define <name>]…\n       raco cql --tck [-v]\n")
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
     (let loop ([as (cdr args)] [data #f] [vs #f] [libs #f] [names '()])
       (cond
         [(null? as) (if data (run-file (car args) data vs libs (reverse names)) (usage))]
         [(and (equal? (car as) "--data") (pair? (cdr as))) (loop (cddr as) (cadr as) vs libs names)]
         [(and (equal? (car as) "--valuesets") (pair? (cdr as))) (loop (cddr as) data (cadr as) libs names)]
         [(and (equal? (car as) "--libraries") (pair? (cdr as))) (loop (cddr as) data vs (cadr as) names)]
         [(and (equal? (car as) "--define") (pair? (cdr as))) (loop (cddr as) data vs libs (cons (cadr as) names))]
         [else (usage)]))]
    [(null? (cdr args)) (compile-file (car args))]
    [(and (equal? (cadr args) "--libraries") (= (length args) 3)) (compile-file (car args) (caddr args))]
    [else (usage)]))
