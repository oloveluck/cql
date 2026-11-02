#lang racket/base

;;; measure-test.rkt — CQL EXECUTION over real data: four eCQMs (Breast, Cervical and Colorectal
;;; Cancer Screening, Diabetes HbA1c Poor Control) run against their 76 test patients
;;; (tests/vendor/ecqm, cqframework/ecqm-content-r4-2021, CC0) with the measures' own libraries and
;;; value set expansions; populations compared with the expected results (measure-harness.rkt).
;;;
;;; RATCHETED — the floor only rises. The known disagreements are named (each a corpus problem, not
;;; an engine one, as far as the evidence goes):
;;;   - Colorectal …-FAIL-…-TZoffset-07 ×2: no evaluation offset makes both timezone variants fail
;;;     while their offset-less twin passes; the library's own comment records a TZoffset problem
;;;   - Colorectal FitDNA-FAIL-category-laboratory: identical to the passing FitDNA-3yr case except ids
;;;   - Diabetes denomexcl-CMS122-Patient: its MeasureReport has denominator 0 with exclusion 1, which
;;;     proportion scoring (exclusion ⊆ denominator) cannot produce
;;;   - Diabetes no-ip-CMS122-Patient: its MeasureReport says no IP; the data meets every IP criterion

(require rackunit racket/runtime-path racket/list "measure-harness.rkt")

(define-runtime-path corpus "vendor/ecqm")
(define rs (run-measure-corpus corpus))
(define (agree? r) (and (case-result-actual r) (case-result-expected r)
                        (for/and ([(k v) (in-hash (case-result-expected r))])
                          (equal? v (hash-ref (case-result-actual r) k #f)))))
(printf "  eCQM execution: ~a/~a test patients with the expected populations (~a measures)\n"
        (count agree? rs) (length rs) (length (remove-duplicates (map case-result-measure rs))))
(for ([r (in-list rs)] #:when (case-result-error r))
  (eprintf "     error ~a/~a: ~a\n" (case-result-measure r) (case-result-case r) (case-result-error r)))
(check-equal? (length rs) 76)
(check-equal? (filter case-result-error rs) '())
(check-true (>= (count agree? rs) 71) "fewer agreeing test patients than the floor")
(check-equal? (sort (for/list ([r (in-list rs)] #:unless (agree? r)) (case-result-case r)) string<?)
              '("denomexcl-CMS122-Patient" "no-ip-CMS122-Patient" "numer-EXM130-FecalOccult-FAIL-1yr-TZoffset-07"
                "numer-EXM130-FitDNA-FAIL-3yr-TZoffset-07" "numer-EXM130-FitDNA-FAIL-category-laboratory"))
