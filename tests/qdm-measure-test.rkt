#lang racket/base

;;; qdm-measure-test.rkt — QDM measures (cqm-models JSON: measure documents, value sets, patients)
;;; run by this engine, against two oracles (qdm-measure-harness.rkt; tests/vendor/qdm/SOURCE.txt):
;;;
;;; 1. cqm-execution's own assertions (spec/models/calculator.spec.js at the vendored commit, its
;;;    ENABLED tests), transcribed below with their line numbers — every one must hold. They cover
;;;    patient- and episode-based proportion measures, several population sets with strata,
;;;    continuous-variable observations in episode order, and ratio observations (one value per
;;;    observation per episode, null outside the observation's population).
;;; 2. the expectedValues stored in each patient document (written by Bonnie when the case was made;
;;;    observations compared as multisets, a null list as an empty one) — RATCHETED, and every
;;;    disagreement is named with the evidence that the stored value, not the engine, is out of date.

(require rackunit racket/runtime-path racket/list racket/string racket/match
         (only-in cql/value cql-null?)
         "qdm-measure-harness.rkt")

(define-runtime-path root "vendor/qdm")
(define cases (run-qdm-measures root))
(define (case-of m p) (or (findf (lambda (c) (and (equal? (qdm-case-measure c) m) (equal? (qdm-case-patient c) p))) cases)
                          (error 'qdm "no case ~a/~a" m p)))

;; ---- 1. cqm-execution's assertions ----------------------------------------------------------
;; (measure patient population-index expected) — expected: code -> count, OBSERV -> values (null as 'null)
(define (pops ip d dx n) (hash 'IPP ip 'DENOM d 'DENEX dx 'NUMER n))
(define (strat ip d dx n s) (hash 'IPP ip 'DENOM d 'DENEX dx 'NUMER n 'STRAT s))
(define spec
  (list
   ;; l.12 can handle single episodes observed (CV)
   (list "CMS903v0" "Visit_1 ED.json" 0 (hash 'IPP 1 'MSRPOPL 1 'MSRPOPLEX 0 'OBSERV '(15)))
   ;; l.32 can handle multiple episodes observed — "values are ordered when created by the calculator"
   (list "CMS903v0" "Visits_2 ED.json" 0 (hash 'OBSERV '(25 15)))
   ;; l.61, l.83 ratio observations
   (list "CMS871v2" "patient_1.json" 0 (hash 'IPP 1 'DENOM 1 'DENEX 1 'NUMER 1 'OBSERV '(null 0)))
   (list "CMS871v2" "patient_2.json" 0 (hash 'OBSERV '(null null 6 1)))
   ;; l.252 default timezone offset
   (list "CMS760v0" "Correct_Timezone.json" 0 (hash 'IPP 0))
   ;; l.265 multiple population measure
   (list "CMS160v6" "Expired_DENEX.json" 0 (pops 1 1 1 0))
   (list "CMS160v6" "Expired_DENEX.json" 1 (pops 0 0 0 0))
   (list "CMS160v6" "Expired_DENEX.json" 2 (pops 0 0 0 0))
   (list "CMS160v6" "Pass_NUM2.json" 0 (pops 0 0 0 0))
   (list "CMS160v6" "Pass_NUM2.json" 1 (pops 1 1 0 1))
   (list "CMS160v6" "Pass_NUM2.json" 2 (pops 0 0 0 0))
   ;; l.556 single population patient-based measure
   (list "CMS134v6" "Fail_Hospice_Not_Performed_Denex.json" 0 (pops 1 1 0 0))
   (list "CMS134v6" "Pass_Numer.json" 0 (pops 1 1 0 1))
   ;; l.579 multiple populations with multiple stratifications (PC1, PC1-S1, PC1-S2, PC2, PC2-S1, PC2-S2)
   (list "CMS137v7" "2YoungDependence&TX_IPPPopFail.json" 0 (pops 0 0 0 0))
   (list "CMS137v7" "2YoungDependence&TX_IPPPopFail.json" 1 (pops 0 0 0 0))
   (list "CMS137v7" "2YoungDependence&TX_IPPPopFail.json" 2 (pops 0 0 0 0))
   (list "CMS137v7" "2YoungDependence&TX_IPPPopFail.json" 3 (pops 0 0 0 0))
   (list "CMS137v7" "2YoungDependence&TX_IPPPopFail.json" 4 (pops 0 0 0 0))
   (list "CMS137v7" "2YoungDependence&TX_IPPPopFail.json" 5 (pops 0 0 0 0))
   (list "CMS137v7" "Dependency<60daysSB4_DENEXPop>18StratPass.json" 0 (pops 1 1 1 0))
   (list "CMS137v7" "Dependency<60daysSB4_DENEXPop>18StratPass.json" 1 (strat 0 0 0 0 0))
   (list "CMS137v7" "Dependency<60daysSB4_DENEXPop>18StratPass.json" 2 (strat 1 1 1 0 1))
   (list "CMS137v7" "Dependency<60daysSB4_DENEXPop>18StratPass.json" 3 (pops 1 1 1 0))
   (list "CMS137v7" "Dependency<60daysSB4_DENEXPop>18StratPass.json" 4 (strat 0 0 0 0 0))
   (list "CMS137v7" "Dependency<60daysSB4_DENEXPop>18StratPass.json" 5 (strat 1 1 1 0 1))
   (list "CMS137v7" "Therapy<14DaysDx_NUMERPop1_13-18Pass.json" 0 (pops 1 1 0 1))
   (list "CMS137v7" "Therapy<14DaysDx_NUMERPop1_13-18Pass.json" 1 (strat 1 1 0 1 1))
   (list "CMS137v7" "Therapy<14DaysDx_NUMERPop1_13-18Pass.json" 2 (strat 0 0 0 0 0))
   (list "CMS137v7" "Therapy<14DaysDx_NUMERPop1_13-18Pass.json" 3 (pops 1 1 0 0))
   (list "CMS137v7" "Therapy<14DaysDx_NUMERPop1_13-18Pass.json" 4 (pops 1 1 0 0))
   (list "CMS137v7" "Therapy<14DaysDx_NUMERPop1_13-18Pass.json" 5 (strat 0 0 0 0 0))))

(define (value->datum v) (if (cql-null? v) 'null (if (and (number? v) (exact? v)) v (string->number (format "~a" v)))))
(define (actual-at c i)
  (define rs (qdm-case-results c))
  (and rs (< i (length rs)) (list-ref rs i)))
(test-case "cqm-execution's own assertions hold"
  (for ([s (in-list spec)])
    (match-define (list m p i want) s)
    (define c (case-of m p))
    (define got (actual-at c i))
    (check-false (qdm-case-error c) (format "~a/~a: ~a" m p (qdm-case-error c)))
    (when got
      (for ([(k v) (in-hash want)])
        (define g (hash-ref got k 'absent))
        (check-equal? (if (eq? k 'OBSERV) (if (list? g) (map value->datum g) g) g) v (format "~a/~a [~a] ~a" m p i k))))))

;; ---- 2. the stored expectedValues ------------------------------------------------------------
(define known
  (hash
   ;; QDM 5.3 measures over QDM 5.5 data: the measures read Encounter.principalDiagnosis, which QDM
   ;; 5.5 removed (the data hold diagnoses with a rank); cqm-execution's own tests of these cases are
   ;; disabled (CMS53v7 l.192 xit, CMS32v7 l.326 xit)
   '("CMS53v7" . "PCIat60min_NUMERPass.json") "principalDiagnosis, absent from QDM 5.5 data"
   '("CMS529v0" . "Pass_IPP-DENOM-NUMER.json") "principalDiagnosis, absent from QDM 5.5 data"
   '("CMS32v7" . "Visit_1ED.json") "strata read principalDiagnosis; stored observations include excluded episodes"
   '("CMS32v7" . "Visits_1Excl_2ED.json") "the stored observation includes the excluded episode (cqm-execution l.398, a disabled test, asserts [25])"
   '("CMS32v7" . "Visits_2ED.json") "strata read principalDiagnosis"
   '("CMS32v7" . "Visits_2Excl_2ED.json") "the stored observations include both excluded episodes"
   ;; the data: encounter 5d5af8da… has a rank-1 diagnosis (10278007) in "Psychiatric/Mental Health
   ;; Diagnosis", so Strat1 holds for it; the stored stratum 1 says none (cqm-execution asserts only
   ;; population set 0 for this patient, l.32)
   '("CMS903v0" . "Visits_2 ED.json") "stored stratum 1 contradicts the data's rank-1 psychiatric diagnosis"
   ;; the stored values give observations as [0, 0] / [0, 0, 1, 6]; cqm-execution asserts [null, 0] and
   ;; [null, null, 6, 1] (l.61, l.83), which this engine gives
   '("CMS871v2" . "patient_1.json") "stored observations predate cqm-execution's (l.61)"
   '("CMS871v2" . "patient_2.json") "stored observations predate cqm-execution's (l.83)"
   ;; StatinTherapy's "LDL in past 2 years" names its query alias inside `sort by`, which CQL does not
   ;; allow: the reference translator failed on it too (its ELM holds Null there); here the definition
   ;; raises its translation error when evaluated
   '("CMS735v0" . "first_last.json") "a definition the reference translator could not translate either"))

(define (stored-agrees? c)
  (and (qdm-case-results c)
       (for/and ([ev (in-list (qdm-case-stored c))])
         (define got (or (actual-at c (hash-ref ev 'population_index 0)) (hash)))
         (for/and ([(k v) (in-hash ev)] #:unless (memq k '(population_index measure_id OBSERV_UNIT)))
           (define g (hash-ref got k '()))
           (if (memq k '(OBSERV DENOM_OBSERV NUMER_OBSERV))
               (equal? (sort (map (lambda (x) (format "~a" (value->datum x))) (if (list? g) g '())) string<?)
                       (sort (map (lambda (x) (format "~a" x)) (if (list? v) v '())) string<?))
               (equal? g v))))))
(define with-stored (filter (lambda (c) (or (pair? (qdm-case-stored c)) (qdm-case-error c))) cases))
(define agreeing (filter stored-agrees? with-stored))
(printf "  QDM measures: ~a patients run, ~a with errors; ~a/~a agree with their stored expectedValues; ~a cqm-execution assertions\n"
        (length cases) (count qdm-case-error cases) (length agreeing) (length with-stored) (length spec))
(for ([c (in-list with-stored)] #:unless (stored-agrees? c))
  (printf "    ~a/~a: ~a\n" (qdm-case-measure c) (qdm-case-patient c)
          (hash-ref known (cons (qdm-case-measure c) (qdm-case-patient c)) "NOT NAMED")))

(test-case "stored expectedValues, ratcheted, disagreements named"
  (check-equal? (length cases) 33)
  (check-true (>= (length agreeing) 16) "fewer QDM patients agree with their stored values than the floor")
  (for ([c (in-list with-stored)] #:unless (stored-agrees? c))
    (check-true (hash-has-key? known (cons (qdm-case-measure c) (qdm-case-patient c)))
                (format "a disagreement no one has named: ~a/~a" (qdm-case-measure c) (qdm-case-patient c))))
  ;; every patient but the named translation failure runs
  (check-equal? (map (lambda (c) (qdm-case-measure c)) (filter qdm-case-error cases)) '("CMS735v0")))
