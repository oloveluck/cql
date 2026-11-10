#lang racket/base

;;; guide-test.rkt — guide.md (the language guide agents read: fhir-mcp serves it as learn://cql,
;;; the cql skill points to it) keeps its promises: every ```cql block translates with no error,
;;; including the libraries it includes (the bundled FHIRHelpers versions), and the worked example
;;; answers what the text says it answers.

(require rackunit racket/runtime-path racket/file racket/list
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library/errors compile-library/diagnostics current-library-resolver)
         (only-in fhir/diagnostic diagnostic-message diagnostic-severity diagnostic-loc loc-line)
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

(test-case "the latest-value recipe answers as the guide says"
  (define (obs id date value)
    (hasheq 'resourceType "Observation" 'id id 'status "final" 'subject (hasheq 'reference "Patient/p")
            'code (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "4548-4")))
            'effectiveDateTime date 'valueQuantity (hasheq 'value value 'unit "%" 'system "http://unitsofmeasure.org" 'code "%")))
  (define rl (load-runtime-library (ok-value (parse-cql (library-named "LatestA1c"))) #:resolver bundled
                                   #:data (resources->data (list (hasheq 'resourceType "Patient" 'id "p"
                                                                         'name (list (hasheq 'given '("Ann" "Marie") 'family "Ng")))
                                                                 (obs "a" "2025-01-10" 9.6) (obs "b" "2025-06-02" 8.9)))))
  (check-regexp-match #rx"\"value\":8[.]9" (render (rlib-value rl "Latest Value")))
  (check-equal? (render (rlib-value rl "Latest Number")) "8.9")
  (check-regexp-match #rx"%" (render (rlib-value rl "Latest Unit")))
  (check-equal? (rlib-value rl "Name") "Ann Marie Ng")
  (check-equal? (rlib-value rl "Poorly Controlled") #f)
  (check-equal? (rlib-value rl "No HbA1c") #f))

(test-case "the recipe's variations answer as the guide says"
  (define (obs id date value)
    (hasheq 'resourceType "Observation" 'id id 'status "final" 'subject (hasheq 'reference "Patient/p")
            'code (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "4548-4")))
            'effectiveDateTime date 'valueQuantity (hasheq 'value value 'unit "%" 'system "http://unitsofmeasure.org" 'code "%")))
  (define rl (load-runtime-library (ok-value (parse-cql (library-named "A1cVariations"))) #:resolver bundled
                                   #:data (resources->data (list (hasheq 'resourceType "Patient" 'id "p")
                                                                 (obs "jan" "2025-01-10" 9.6) (obs "jun" "2025-06-02" 8.9)
                                                                 (obs "sep" "2025-09-15" 9.2)))))
  (check-regexp-match #rx"\"id\":\"sep\"" (render (rlib-value rl "Latest In Period")))
  (check-equal? (map (lambda (o) (cadr (regexp-match #rx"\"id\":\"([a-z]+)\"" (render o)))) (rlib-value rl "Last Two")) '("sep" "jun"))
  (check-equal? (render (rlib-value rl "Mean Of Last Two")) "9.05")
  (check-equal? (rlib-value rl "Rising") #t)
  ;; without the parentheses the error says what happened (it was a `cadr` crash)
  (define bare (regexp-replace #rx"Take[(][(](.*desc)[)], 2[)]" (library-named "A1cVariations") "Take(\\1, 2)"))
  (check-false (equal? bare (library-named "A1cVariations")))
  (define rl2 (load-runtime-library (ok-value (parse-cql bare)) #:resolver bundled
                                    #:data (resources->data (list (hasheq 'resourceType "Patient" 'id "p") (obs "jan" "2025-01-10" 9.6)))))
  (check-exn (lambda (e) (regexp-match? #rx"put the query in parentheses" (if (exn? e) (exn-message e) (format "~a" e))))
             (lambda () (rlib-value rl2 "Last Two"))))

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
  (check-equal? (rlib-value rl "Cast") '(#t))
  ;; and the translation warns about the bare one, at its define, naming the member to cast to
  (define-values (elm errors warnings)
    (parameterize ([current-library-resolver bundled]) (compile-library/diagnostics (ok-value (parse-cql lib)))))
  (check-equal? errors '())
  (check-equal? (map diagnostic-severity warnings) '(warning))
  (check-equal? (loc-line (diagnostic-loc (car warnings))) 6)
  (check-regexp-match #rx"Bare.*dateTime" (diagnostic-message (car warnings))))
