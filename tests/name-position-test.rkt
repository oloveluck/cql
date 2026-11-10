#lang racket/base

;;; name-position-test.rkt — a translation error about a name is positioned at the name, not at its
;;; `define` (a new user's `AgeInYear` was reported at 6:1, the define's first column): the command
;;; line, a `#lang cql` module's build error and the editor's diagnostics agree.

(require rackunit racket/file racket/port racket/system compiler/find-exe
         (only-in cql/support support)
         (only-in fhir/lang/support lang-support-diagnose)
         (only-in fhir/diagnostic diagnostic-loc loc-line loc-column))

(define text (string-append "library T version '1'\nusing FHIR version '4.0.1'\ncontext Patient\n\n"
                            "define \"Is 65 Or Older\":\n  AgeInYear() >= 65\n"
                            "define \"B\": \"Is 65 Or Older\" and Foo\n"))
(define dir (make-temporary-directory "cql-pos-~a"))
(define f (build-path dir "T.cql"))
(with-output-to-file f (lambda () (display text)))
;; raco cql: both errors, each at its name
(define out (open-output-string))
(parameterize ([current-error-port out] [current-output-port (open-output-nowhere)])
  (void (system* (find-exe) "-l-" "raco" "cql" (path->string f))))
(check-regexp-match #rx"T[.]cql:6:3: cql compile error: in define \"Is 65 Or Older\": unknown function 'AgeInYear'" (get-output-string out))
(check-regexp-match #rx"T[.]cql:7:34: cql compile error: in define \"B\": unknown identifier 'Foo'" (get-output-string out))
;; the editor's diagnostics
(define ds ((lang-support-diagnose support) text (path->string f)))
(check-equal? (map (lambda (d) (list (loc-line (diagnostic-loc d)) (loc-column (diagnostic-loc d)))) ds) '((6 3) (7 34)))
;; a #lang cql module: the build fails at the name
(define m (build-path dir "M.cql"))
(with-output-to-file m (lambda () (display (string-append "#lang cql\n" text))))
(define bout (open-output-string))
(parameterize ([current-error-port bout] [current-output-port (open-output-nowhere)])
  (void (system* (find-exe) (path->string m))))
(check-regexp-match #rx"M[.]cql:7:3: cql compile error: in define \"Is 65 Or Older\": unknown function 'AgeInYear'" (get-output-string bout))
(delete-directory/files dir)
