#lang racket/base

;;; helpers-test.rkt — a FHIR value meeting a System operator without FHIRHelpers is a translation
;;; error naming the include, not a quiet `false`: `Patient.gender = 'female'` answered false for a
;;; female patient when the library did not include FHIRHelpers (the reference translator refuses it).
;;; AgeInYears() reads the birth date through the model, and needs no include.

(require rackunit racket/file racket/port racket/system racket/string compiler/find-exe)

(define dir (make-temporary-directory "cql-helpers-~a"))
(define (write! name text) (with-output-to-file (build-path dir name) (lambda () (display text))) (path->string (build-path dir name)))
(define patient (write! "p.json" "{\"resourceType\":\"Patient\",\"id\":\"p\",\"gender\":\"female\",\"birthDate\":\"1970-05-01\"}"))
(define (run file . more)
  (define out (open-output-string))
  (define code (parameterize ([current-output-port out] [current-error-port out])
                 (apply system*/exit-code (find-exe) "-l-" "raco" "cql" file more)))
  (values code (get-output-string out)))

(define head "library T version '1'\nusing FHIR version '4.0.1'\n")
;; without FHIRHelpers: an error that says what to add
(let-values ([(_c out) (run (write! "NoHelpers.cql" (string-append head "context Patient\ndefine \"Female\": Patient.gender = 'female'\n")))])
  (check-regexp-match #rx"FHIR[.]AdministrativeGender meets a System operator here and is not converted: add `include FHIRHelpers version '4[.]0[.]1' called FHIRHelpers`" out)
  (check-false (regexp-match? #rx"Female: false" out)))
;; with it: the answer
(let-values ([(code out) (run (write! "Helpers.cql" (string-append head "include FHIRHelpers version '4.0.1' called FHIRHelpers\ncontext Patient\ndefine \"Female\": Patient.gender = 'female'\n"))
                              "--data" patient)])
  (check-equal? code 0)
  (check-regexp-match #rx"Female: true" out))
;; AgeInYears() needs no include
(let-values ([(code out) (run (write! "Age.cql" (string-append head "context Patient\ndefine \"Age\": AgeInYears()\n")) "--data" patient "--now" "2026-01-01")])
  (check-equal? code 0)
  (check-regexp-match #rx"Age: 55" out))
(delete-directory/files dir)
