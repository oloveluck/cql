#lang racket/base

;;; cli.rkt — `raco cql`: the command line for the CQL package.
;;;
;;;   raco cql <file.cql>      compile a CQL library to ELM and print the ELM JSON
;;;                            (a leading `#lang cql` line is ignored, so both raw .cql and #lang files work)
;;;   raco cql --tck [-v]      run the vendored OFFICIAL cqframework/cql-tests suites (floor-ratcheted;
;;;                            exit 1 on regression; -v lists failures)
;;; Exit codes: 0 ok · 1 CQL error / regression · 2 usage.

(require (only-in racket/file file->string)
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library)
         (only-in cql/elm/json elm->json-string)
         (only-in cql/error cql-error-string)
         (only-in fhir/result/result ok? ok-value err-error))

(provide compile-file)

;; compile-file : path-string -> void   print the ELM JSON of the library in `file`, or exit 1
(define (compile-file file)
  (define src (regexp-replace #px"^#lang[^\n]*\n" (file->string file) ""))
  (define pr (parse-cql src))
  (unless (ok? pr) (eprintf "~a: ~a\n" file (cql-error-string (err-error pr))) (exit 1))
  (define cr (compile-library (ok-value pr)))
  (unless (ok? cr) (eprintf "~a: ~a\n" file (cql-error-string (err-error cr))) (exit 1))
  (displayln (elm->json-string (ok-value cr))))

(define (usage)
  (eprintf "usage: raco cql <file.cql>\n       raco cql --tck [-v]\n")
  (exit 2))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (cond
    [(null? args) (usage)]
    [(equal? (car args) "--tck")
     (parameterize ([current-command-line-arguments (list->vector (cdr args))])
       (dynamic-require '(submod cql/tests/run main) #f))]
    [(regexp-match? #rx"^-" (car args)) (usage)]
    [else (compile-file (car args))]))
