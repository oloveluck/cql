#lang racket/base

;;; qicore-measure-test.rkt — four 2024 eCQMs written against QI-Core 4.1.1 (Breast Cancer
;;; Screening, Cervical Cancer Screening, Controlling High Blood Pressure, Diabetes Glycemic Status
;;; > 9%), run over their 190 MADiE test cases; populations against each case's MeasureReport
;;; (qicore-measure-harness.rkt).
;;;
;;; RATCHETED BASELINE — the agreements only rise, and every disagreement is named here with the
;;; evidence that the case, not the engine, is at odds with its expected report.

(require rackunit racket/runtime-path racket/list racket/string
         "qicore-measure-harness.rkt")

(define-runtime-path root "vendor/qicore-2024")
(define rs (run-qicore-measures root))
(define (agrees? r) (equal? (qicore-case-expected r) (qicore-case-actual r)))

;; the named disagreements: (measure . case name) -> why
(define known
  (hash
   ;; the case's description (cqfm-testCaseDescription) says "HbA1c level is 9.1%", but the exported
   ;; Observation's value is a SNOMED CodeableConcept (373066001 "Yes"): no Quantity to exceed 9%
   '("DiabetesGlycemicStatusAssessmentGreaterThan9PercentFHIR" . "NUMPass HbA1cGreaterThan9")
   "the HbA1c value is exported as a concept, not the 9.1% the case describes"
   ;; the same for the GMI case: a SNOMED concept (374166001) where the description gives a percentage
   '("DiabetesGlycemicStatusAssessmentGreaterThan9PercentFHIR" . "NUMPass GMIGreaterThan9")
   "the GMI value is exported as a concept, not the percentage the case describes"))

(for ([m (in-list (remove-duplicates (map qicore-case-measure rs)))])
  (define ms (filter (lambda (r) (equal? (qicore-case-measure r) m)) rs))
  (printf "  ~a: ~a/~a\n" m (count agrees? ms) (length ms)))
(define misses (filter (lambda (r) (not (agrees? r))) rs))
(for ([r (in-list misses)])
  (printf "    ~a ~a (~a)\n      expected ~s\n      actual   ~s\n      ~a\n" (qicore-case-measure r) (qicore-case-name r) (qicore-case-case r)
          (qicore-case-expected r) (or (qicore-case-actual r) (qicore-case-error r))
          (hash-ref known (cons (qicore-case-measure r) (qicore-case-name r)) "NOT NAMED")))

(check-equal? (length rs) 190)
(check-true (>= (count agrees? rs) 188) "fewer QI-Core cases agree than the floor")
(for ([r (in-list misses)])
  (check-true (hash-has-key? known (cons (qicore-case-measure r) (qicore-case-name r)))
              (format "a disagreement no one has named: ~a ~a" (qicore-case-measure r) (qicore-case-name r))))
