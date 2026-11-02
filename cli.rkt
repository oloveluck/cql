#lang racket/base

;;; cli.rkt — `raco cql`: the command line for the CQL package.
;;;
;;;   raco cql <file.cql>      compile a CQL library to ELM and print the ELM JSON
;;;                            (a leading `#lang cql` line is ignored, so both raw .cql and #lang files work)
;;;   raco cql <file.cql> --data <dir|file.json> [--valuesets <dir>] [--libraries <dir>] [--define <name>]…
;;;                            RUN the library over FHIR data (resources or Bundles; patient context),
;;;                            includes found in --libraries (default: beside the file), terminology from
;;;                            the value sets in --valuesets; prints each definition (or the named ones)
;;;   raco cql --tck [-v]      run the vendored OFFICIAL cqframework/cql-tests suites (floor-ratcheted;
;;;                            exit 1 on regression; -v lists failures)
;;; Exit codes: 0 ok · 1 CQL error / regression · 2 usage.

(require (only-in racket/file file->string)
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library)
         (only-in cql/elm/json elm->json-string)
         (only-in cql/error cql-error-string)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in cql/error cql-error?)
         (only-in cql/run load-runtime-library resources->data valuesets->terminology read-json-files rlib-value)
         (only-in cql/parser/ast cql-library-defs cql-def? cql-def-name)
         (only-in cql/value render)
         (only-in json read-json))

(provide compile-file run-file)

;; run-file : file data valuesets|#f libraries|#f (listof name) -> void   evaluate and print definitions
(define (run-file file data vs-dir lib-dir names)
  (define (parse f) (let ([r (parse-cql (regexp-replace #px"^#lang[^\n]*\n" (file->string f) ""))])
                      (unless (ok? r) (eprintf "~a: ~a\n" f (cql-error-string (err-error r))) (exit 1))
                      (ok-value r)))
  (define libdir (or lib-dir (let-values ([(d _n _m) (split-path (path->complete-path file))]) d)))
  (define (res n _v) (let ([g (build-path libdir (format "~a.cql" n))]) (and (file-exists? g) (parse g))))
  (define lib (parse file))
  (define resources (if (directory-exists? data) (read-json-files data) (list (call-with-input-file data read-json))))
  (define rl (with-handlers ([exn:fail? (lambda (e) (eprintf "~a\n" (exn-message e)) (exit 1))])
               (load-runtime-library lib #:resolver res #:data (resources->data resources)
                                     #:valuesets (valuesets->terminology (if vs-dir (read-json-files vs-dir) '())))))
  (define defs (or (and (pair? names) names)
                   (for/list ([d (in-list (cql-library-defs lib))] #:when (cql-def? d)) (symbol->string (cql-def-name d)))))
  (for ([n (in-list defs)])
    (printf "~a: ~a\n" n (with-handlers ([cql-error? cql-error-string]) (render (rlib-value rl n))))))

;; compile-file : path-string -> void   print the ELM JSON of the library in `file`, or exit 1
(define (compile-file file)
  (define src (regexp-replace #px"^#lang[^\n]*\n" (file->string file) ""))
  (define pr (parse-cql src))
  (unless (ok? pr) (eprintf "~a: ~a\n" file (cql-error-string (err-error pr))) (exit 1))
  (define cr (compile-library (ok-value pr)))
  (unless (ok? cr) (eprintf "~a: ~a\n" file (cql-error-string (err-error cr))) (exit 1))
  (displayln (elm->json-string (ok-value cr))))

(define (usage)
  (eprintf "usage: raco cql <file.cql> [--data <dir|file.json> [--valuesets <dir>] [--libraries <dir>] [--define <name>]…]\n       raco cql --tck [-v]\n")
  (exit 2))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (cond
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
    [else (compile-file (car args))]))
