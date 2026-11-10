#lang racket/base

;;; calendar-test.rkt — a date the calendar does not have (February 30th) is no date: a literal is a
;;; translation error at its define, a constructor an error when it runs, a string conversion null,
;;; and FHIRPath's literal an error. A new user's birthDate 1940-02-30 got an age computed from it.

(require rackunit racket/file racket/port racket/system compiler/find-exe
         (only-in fhirpath/eval eval-fhirpath)
         (only-in fhirpath/parser/parse parse-fhirpath))

(define dir (make-temporary-directory "cql-calendar-~a"))
(define f (build-path dir "T.cql"))
(with-output-to-file f
  (lambda () (display "library T\ndefine A: @1940-02-30\ndefine B: Date(1940, 2, 30)\ndefine C: ToDate('1940-02-30')\ndefine D: @2000-02-29\n")))
(define (raco-cql . args)
  (define out (open-output-string))
  (parameterize ([current-error-port out] [current-output-port out])
    (void (apply system* (find-exe) "-l-" "raco" "cql" (path->string f) args)))
  (get-output-string out))
;; the literal: a translation error at its define
(check-regexp-match #rx"T[.]cql:2:1: cql compile error: in define \"A\": @1940-02-30 names no day: 1940-02 has 29 days"
                    (raco-cql))
;; without the literal, the constructor fails when it runs, the conversion is null, a leap day holds
(with-output-to-file f #:exists 'truncate
  (lambda () (display "library T\ndefine B: Date(1940, 2, 30)\ndefine C: ToDate('1940-02-30')\ndefine D: @2000-02-29\n")))
(define run (raco-cql "--data" (path->string (let ([d (build-path dir "data")]) (make-directory d) d))
                      "--define" "B" "--define" "C" "--define" "D"))
(check-regexp-match #rx"B: CQL interpret error: day 30 is out of range [(]1940-02 has 29 days[)]" run)
(check-regexp-match #rx"C: null" run)
(check-regexp-match #rx"D: @2000-02-29" run)
;; FHIRPath: the literal is an error, the conversion empty
(check-exn #rx"@1940-02-30 names no day: 1940-02 has 29 days" (lambda () (eval-fhirpath (parse-fhirpath "@1940-02-30") '())))
(check-equal? (eval-fhirpath (parse-fhirpath "'1940-02-30'.toDate().empty()") '()) '(#t))
