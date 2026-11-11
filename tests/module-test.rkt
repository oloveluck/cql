#lang racket/base

;;; module-test.rkt — a `#lang cql` module as other code uses it: `evaluate` over a patient's data,
;;; `#:define` for one value (a misspelt name says which it meant), `#:now` for a repeatable run.

(require rackunit racket/file)

(define dir (make-temporary-directory "cql-module-~a"))
(define file (build-path dir "Age.cql"))
(void (call-with-output-file file
  (lambda (o) (write-string "#lang cql\nlibrary Age version '1'\nusing FHIR version '4.0.1'\ncontext Patient\ndefine \"Today\": Today()\ndefine \"Age\": AgeInYears()\ndefine \"Adult\": AgeInYears() >= 18\n" o))))
(define evaluate (dynamic-require file 'evaluate))
(define patient (hasheq 'resourceType "Patient" 'id "p" 'birthDate "2007-07-01"))

;; the clock fixed: 17 the day before the birthday, 18 on it
(check-equal? (evaluate (list patient) #:now "2025-06-30T12:00:00Z" #:define "Age") 17)
(check-equal? (evaluate (list patient) #:now "@2025-07-01T12:00:00Z" #:define "Adult") #t)
(check-equal? (hash-ref (evaluate (list patient) #:now "2025-07-01T12:00:00Z") "Age") 18)
;; a name the library does not define
(check-exn #rx"defines no `Adlut` [(]did you mean `Adult`[?][)]" (lambda () (evaluate (list patient) #:define "Adlut")))
(check-exn (lambda (e) (and (exn:fail:user? e) (regexp-match? #rx"is not a date" (exn-message e)))) (lambda () (evaluate (list patient) #:now "yesterday")))

(delete-directory/files dir)
