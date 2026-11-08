#lang racket/base

;;; guide-test.rkt — guide.md (the language guide agents read: fhir-mcp serves it as learn://cql,
;;; the cql skill points to it) keeps its promises: every ```cql block translates with no error,
;;; including the libraries it includes (the bundled FHIRHelpers versions), and the worked example
;;; answers what the text says it answers.

(require rackunit racket/runtime-path racket/file racket/list
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library/errors current-library-resolver)
         (only-in cql/error cql-error-string)
         (only-in cql/workspace guide)
         cql/run
         (only-in cql/value render)
         (only-in fhir/result/result ok? ok-value err-error))

(define-runtime-path guide-file "../guide.md")
(define text (file->string guide-file))

;; the bundled libraries, as a CLI run and a workspace resolve them
(define (bundled name version) (bundled-library name version))

(define blocks (map cadr (regexp-match* #px"```cql\n(.*?)```" text #:match-select values)))
(define (library-named name) (for/first ([b (in-list blocks)] #:when (regexp-match? (pregexp (string-append "^library " name " ")) b)) b))

(test-case "the guide the workspace serves is this file"
  (check-equal? guide text))

(test-case "every cql block translates"
  (check-true (>= (length blocks) 4))
  (for ([b (in-list blocks)])
    (define p (parse-cql b))
    (check-true (ok? p) (format "parse: ~a\n~a" (if (ok? p) "" (cql-error-string (err-error p))) b))
    (when (ok? p)
      (define-values (_elm errors) (parameterize ([current-library-resolver bundled]) (compile-library/errors (ok-value p))))
      (check-equal? (map cql-error-string errors) '() b))))

(test-case "the worked example answers as the text says"
  (define rl (load-runtime-library (ok-value (parse-cql (library-named "HbA1cGap"))) #:resolver bundled
                                   #:data (resources->data (list (hasheq 'resourceType "Patient" 'id "p" 'birthDate "1970-03-01")))))
  (check-equal? (rlib-value rl "Gap") #t))

(test-case "the choice-timing problem reads as the guide says"
  ;; an Observation with effectiveDateTime inside the period: the bare choice reads only a Period
  ;; (null); the cast reads the dateTime (true)
  (define lib (string-append "library T version '1'\nusing FHIR version '4.0.1'\ninclude FHIRHelpers version '4.0.1' called FHIRHelpers\n"
                             "parameter \"Measurement Period\" Interval<DateTime> default Interval[@2024-01-01T00:00:00Z, @2025-01-01T00:00:00Z)\n"
                             "context Patient\n"
                             "define Bare: [Observation] O return O.effective during \"Measurement Period\"\n"
                             "define Cast: [Observation] O return (O.effective as dateTime) during \"Measurement Period\"\n"))
  (define rl (load-runtime-library (ok-value (parse-cql lib)) #:resolver bundled
                                   #:data (resources->data (list (hasheq 'resourceType "Patient" 'id "p")
                                                                 (hasheq 'resourceType "Observation" 'id "o" 'subject (hasheq 'reference "Patient/p")
                                                                         'effectiveDateTime "2024-05-01")))))
  (check-equal? (map render (rlib-value rl "Bare")) '("null"))
  (check-equal? (rlib-value rl "Cast") '(#t)))
