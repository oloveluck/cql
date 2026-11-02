#lang racket/base

;;; measure-harness.rkt — execute eCQMs over their test patients and compare the populations with
;;; the expected results (tests/vendor/ecqm, see its SOURCE.txt).
;;;
;;; Each test case directory is one patient's data. Expected populations come from the case's
;;; MeasureReport when it has one (group.population.count), otherwise from the repository's naming
;;; convention, which under proportion scoring fixes all four populations:
;;;   numer-…            IP 1, denominator 1, exclusion 0, numerator 1
;;;   numer-…-FAIL-…     IP 1, denominator 1, exclusion 0, numerator 0
;;;   denom-…            IP 1, denominator 1, exclusion 0, numerator 0
;;;   denomexcl-… / exclusion-…   IP 1, denominator 1, exclusion 1, numerator 0
;;;   neg-ip-… / no-ip-…  all 0
;;; The Measurement Period is the MeasureReport's year when there is one, else the measure library's
;;; default, else the year its test data is written for (`fallback-years`). Evaluation runs at offset
;;; -07:00 (`evaluation-offset`): the expected MeasureReports' periods are written at -07:00, and a
;;; DateTime without an offset (the period's literal, offset-less FHIR data) takes the request's.
;;; A case directory is the patient's record as authored: several cases reference a sibling patient
;;; id by copy-paste (numer-EXM130-sigmoidoscopy-FAIL-missing's Encounter names "…-5yr"), and their
;;; expected results count those resources, so the strict patient-context filter is off here.
;;;
;;; Data: (case-result measure case expected actual error)   expected/actual : hash code -> 0|1

(require racket/contract racket/list racket/string racket/file racket/match json
         (only-in cql/parser/parse parse-cql parse-cql-expr)
         (only-in cql/compile compile-expr)
         (only-in cql/interp eval-elm make-ienv)
         (only-in cql/error cql-error? cql-error-string)
         (only-in cql/value current-evaluation-offset)
         cql/run
         (only-in fhir/result/result ok? ok-value))

(provide (struct-out case-result)
         (contract-out [run-measure-corpus (-> path-string? (listof case-result?))]))

(struct case-result (measure case expected actual error) #:transparent)

(define fallback-years (hash "ColorectalCancerScreeningsFHIR" 2021 "DiabetesHemoglobinA1cHbA1cPoorControl9FHIR" 2019))
(define evaluation-offset -420)

(define (expected-from-name name)
  (define (pops ip d de n) (hash "initial-population" ip "denominator" d "denominator-exclusion" de "numerator" n))
  (cond [(regexp-match? #rx"^(neg-ip|no-ip)" name) (pops 0 0 0 0)]
        [(regexp-match? #rx"^(denomexcl|exclusion)" name) (pops 1 1 1 0)]
        [(regexp-match? #rx"^numer-.*FAIL" name) (pops 1 1 0 0)]
        [(regexp-match? #rx"^numer" name) (pops 1 1 0 1)]
        [(regexp-match? #rx"^denom" name) (pops 1 1 0 0)]
        [else #f]))

(define (measure-report dir)
  (for/first ([f (in-directory dir)] #:when (regexp-match? #rx"(?i:measurereport)[^/]*[.]json$" (path->string f)))
    (call-with-input-file f read-json)))
(define (expected-from-report mr)
  (for*/hash ([g (in-list (hash-ref mr 'group '()))] [p (in-list (hash-ref g 'population '()))])
    (values (hash-ref (car (hash-ref (hash-ref p 'code) 'coding)) 'code) (hash-ref p 'count 0))))

;; measurement-period : year -> CQL Interval<DateTime> value
(define (measurement-period year)
  (parameterize ([current-evaluation-offset evaluation-offset])
   (eval-elm (ok-value (compile-expr (ok-value (parse-cql-expr
                                               (format "Interval[@~a-01-01T00:00:00.0, @~a-01-01T00:00:00.0)" year (add1 year))))))
            (make-ienv))))

(define (run-measure-corpus root)
  (define (res n _v) (let ([g (build-path root "cql" (format "~a.cql" n))])
                       (and (file-exists? g) (let ([r (parse-cql (file->string g))]) (and (ok? r) (ok-value r))))))
  (define tx (valuesets->terminology (read-json-files (build-path root "valueset"))))
  (for*/list ([mfile (in-list (sort (map path->string (directory-list (build-path root "measure"))) string<?))]
              [m (in-value (path->string (path-replace-extension mfile #"")))]
              [measure (in-value (call-with-input-file (build-path root "measure" mfile) read-json))]
              [lib (in-value (res (string->symbol m) #f))]
              [c (in-list (sort (map path->string (directory-list (build-path root "tests" m))) string<?))]
              #:when (directory-exists? (build-path root "tests" m c "Patient")))
    (define dir (build-path root "tests" m c))
    (define mr (measure-report dir))
    (define expected (if mr (expected-from-report mr) (expected-from-name c)))
    (define year (or (and mr (string->number (substring (hash-ref (hash-ref mr 'period) 'start) 0 4)))
                     (hash-ref fallback-years m #f)))
    (define actual-or-error
      (with-handlers ([cql-error? (lambda (e) (cql-error-string e))]
                      [exn:fail? (lambda (e) (exn-message e))])
        (define rl (load-runtime-library lib #:resolver res
                                         #:data (resources->data (read-json-files dir) #:patient-context? #f)
                                         #:valuesets tx #:offset evaluation-offset
                                         #:parameters (if year (hash "Measurement Period" (measurement-period year)) (hash))))
        (measure-populations measure rl)))
    (if (string? actual-or-error)
        (case-result m c expected #f actual-or-error)
        (case-result m c expected actual-or-error #f))))
