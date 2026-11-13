#lang racket/base

;;; cqm.rkt — measures in the cqm-models format (Bonnie, MADiE's QDM side, cqm-execution): a patient
;;; document's records, and a measure document's populations over one patient.
;;;
;;; The data are QDM's JSON as cqm-models writes it: a patient document is {birthDatetime,
;;; dataElements: [...]}, each data element naming its class under _type (QDM's representation,
;;; model/qdm.representation.rktd, says how the rest reads). A measure document names its main
;;; library, its scoring (PROPORTION, RATIO, CONTINUOUS_VARIABLE, COHORT), its calculation method
;;; (PATIENT or EPISODE_OF_CARE), and its population sets: each population code (IPP, DENOM, DENEX,
;;; NUMER, NUMEX, DENEXCEP, MSRPOPL, MSRPOPLEX) names a define, observations name a function over the
;;; measure population's episodes, and stratifications name a define each.
;;;
;;; A Population is a set of members: for a patient-based measure, the patient when its define is true
;;; (or a non-empty list); for an episode-based one, the define's list of episodes (records), distinct.
;;; The populations depend on each other as cqm-execution computes them:
;;;   proportion  DENOM ⊆ IPP, DENEX ⊆ DENOM, NUMER ⊆ DENOM−DENEX, NUMEX ⊆ NUMER,
;;;               DENEXCEP ⊆ DENOM−DENEX−NUMER
;;;   ratio       DENOM ⊆ IPP, DENEX ⊆ DENOM, NUMER ⊆ IPP, NUMEX ⊆ NUMER
;;;   CV          MSRPOPL ⊆ IPP, MSRPOPLEX ⊆ MSRPOPL; OBSERV = the observation of each member of
;;;               MSRPOPL−MSRPOPLEX
;;; A stratum restricts every population of its set to the members of the stratification define (and
;;; adds STRAT, the stratum's own count). The result for a population set, then each stratum, is a
;;; PopulationResult: (hash code -> count), OBSERV -> (listof value).

(require racket/contract racket/list racket/match racket/string
         "interp.rkt" "value.rkt")

(provide
 (contract-out
  ;; cqm-patient-records : patient document -> (listof record)   the patient (as a QDM::Patient record)
  ;; and its data elements; a document wrapping its QDM patient (Bonnie's {qdmPatient: …}) is unwrapped
  [cqm-patient-records (-> hash? (listof hash?))]
  ;; cqm-measure-period : measure document -> (values start end)   the measure period's bounds as
  ;; DateTime strings (its TS values: "201201010000" -> "2012-01-01T00:00:00.000")
  [cqm-measure-period (-> hash? (values string? string?))]
  ;; cqm-measure-populations : measure document rlib -> (listof PopulationResult)   per population
  ;; set, then per stratum, in the measure document's order (cqm-execution's population_index)
  [cqm-measure-populations (-> hash? rlib? (listof hash?))]))

(define (cqm-patient-records doc)
  (define p (hash-ref doc 'qdmPatient doc))
  (cons (hash-set (hash-remove p 'dataElements) '_type "QDM::Patient")
        (filter hash? (hash-ref p 'dataElements '()))))

(define (cqm-measure-period m)
  (define mp (hash-ref m 'measure_period))
  (define (ts k) (let ([v (hash-ref (hash-ref mp k) 'value)])
                   (format "~a-~a-~aT~a:~a:00.000" (substring v 0 4) (substring v 4 6) (substring v 6 8)
                           (substring v 8 10) (substring v 10 12))))
  (values (ts 'low) (string-append (substring (ts 'high) 0 17) "59.999")))

(define (cqm-measure-populations m rl)
  (define episodes? (equal? (hash-ref m 'calculation_method #f) "EPISODE_OF_CARE"))
  (define scoring (hash-ref m 'measure_scoring "PROPORTION"))
  (append*
   (for/list ([ps (in-list (hash-ref m 'population_sets '()))])
     (define pops (hash-ref ps 'populations (hash)))
     (define (members code)
       (define p (hash-ref pops code #f))
       (and (hash? p) (hash-ref p 'statement_name #f) (members-of (rlib-value rl (hash-ref p 'statement_name)) episodes?)))
     (define base (populations scoring members))
     ;; observations: CV observes each member of MSRPOPL−MSRPOPLEX; a ratio measure gives each IPP
     ;; member one value per observation, null where the member is not in that observation's
     ;; population (DENOM−DENEX, NUMER−NUMEX) — cqm-execution's observation_values
     (define obs (for/list ([o (in-list (hash-ref ps 'observations '()))])
                   (cons (hash-ref (hash-ref o 'observation_function) 'statement_name)
                         (code-of pops (hash-ref (hash-ref o 'observation_parameter (hash)) 'statement_name #f)))))
     (define (observe1 fn e) (rlib-call rl fn (if episodes? (list e) '())))
     (define (observe sets)
       (cond
         [(null? obs) sets]
         [(hash-ref sets 'MSRPOPL #f)
          (hash-set sets 'OBSERV (for/list ([e (in-list (set-minus (hash-ref sets 'MSRPOPL) (hash-ref sets 'MSRPOPLEX '())))])
                                   (observe1 (car (car obs)) e)))]
         [else
          (define (net code) (set-minus (hash-ref sets code '()) (hash-ref sets (hash-ref exclusion-of code 'none) '())))
          (hash-set sets 'OBSERV (for*/list ([e (in-list (hash-ref sets 'IPP '()))] [o (in-list obs)])
                                   (if (and (cdr o) (memf (lambda (y) (member-equal? e y)) (net (cdr o)))) (observe1 (car o) e) cql-null)))]))
     (cons (counts (observe base))
           (for/list ([st (in-list (hash-ref ps 'stratifications '()))])
             (define in (members-of (rlib-value rl (hash-ref (hash-ref st 'statement) 'statement_name)) episodes?))
             (define restricted (for/hash ([(k v) (in-hash base)]) (values k (set-and v in))))
             (counts (hash-set (observe restricted) 'STRAT (set-and (hash-ref base 'IPP '()) in))))))))

;; code-of : populations statement-name -> code|#f   the population a define is
(define (code-of pops name)
  (for/first ([(k v) (in-hash pops)] #:when (and (hash? v) (equal? (hash-ref v 'statement_name #f) name))) k))
(define exclusion-of (hasheq 'DENOM 'DENEX 'NUMER 'NUMEX 'MSRPOPL 'MSRPOPLEX))

;; populations : scoring (code -> members|#f) -> (hash code -> members)   the dependent populations
(define (populations scoring members)
  (define (m code) (members code))
  (define ipp (or (m 'IPP) '()))
  (define h (hash 'IPP ipp))
  (match scoring
    ["CONTINUOUS_VARIABLE"
     (define msrpopl (set-and (or (m 'MSRPOPL) ipp) ipp))
     (define msrpoplex (set-and (or (m 'MSRPOPLEX) '()) msrpopl))
     (with h 'MSRPOPL msrpopl (m 'MSRPOPL) 'MSRPOPLEX msrpoplex (m 'MSRPOPLEX))]
    ["RATIO"
     (define denom (set-and (or (m 'DENOM) ipp) ipp))
     (define denex (set-and (or (m 'DENEX) '()) denom))
     (define numer (set-and (or (m 'NUMER) '()) ipp))
     (define numex (set-and (or (m 'NUMEX) '()) numer))
     (with h 'DENOM denom (m 'DENOM) 'DENEX denex (m 'DENEX) 'NUMER numer (m 'NUMER) 'NUMEX numex (m 'NUMEX))]
    ["COHORT" h]
    [_
     (define denom (set-and (or (m 'DENOM) ipp) ipp))
     (define denex (set-and (or (m 'DENEX) '()) denom))
     (define numer (set-and (or (m 'NUMER) '()) (set-minus denom denex)))
     (define numex (set-and (or (m 'NUMEX) '()) numer))
     (define denexcep (set-and (or (m 'DENEXCEP) '()) (set-minus (set-minus denom denex) numer)))
     (with h 'DENOM denom (m 'DENOM) 'DENEX denex (m 'DENEX) 'NUMER numer (m 'NUMER) 'NUMEX numex (m 'NUMEX)
           'DENEXCEP denexcep (m 'DENEXCEP))]))
;; with : hash (code members declared)... -> hash   the populations the measure declares
(define (with h . kvs)
  (let loop ([h h] [kvs kvs])
    (match kvs
      ['() h]
      [(list* k v declared rest) (loop (if declared (hash-set h k v) h) rest)])))

;; members-of : value episodes? -> (listof member)   a patient-based define: '(patient) or '();
;; an episode-based one: its distinct records
(define (members-of v episodes?)
  (cond [episodes? (if (list? v) (remove-duplicates (filter (lambda (x) (not (cql-null? x))) v) member-equal?) '())]
        [(or (eq? v #t) (pair? v)) '(patient)]
        [else '()]))
(define (member-equal? a b)
  (if (and (model-node? a) (model-node? b)) (eq? (model-node-data a) (model-node-data b)) (equal? a b)))
(define (set-and a b) (filter (lambda (x) (memf (lambda (y) (member-equal? x y)) b)) a))
(define (set-minus a b) (filter (lambda (x) (not (memf (lambda (y) (member-equal? x y)) b))) a))

;; counts : (hash code -> members | OBSERV values) -> PopulationResult
(define (counts sets)
  (for/hash ([(k v) (in-hash sets)])
    (values k (if (eq? k 'OBSERV) v (length v)))))

(module+ test
  (require rackunit)
  (define-values (s e) (cqm-measure-period (hasheq 'measure_period (hasheq 'low (hasheq 'value "201201010000") 'high (hasheq 'value "201212312359")))))
  (check-equal? (list s e) '("2012-01-01T00:00:00.000" "2012-12-31T23:59:59.999"))
  (check-equal? (map (lambda (r) (hash-ref r '_type)) (cqm-patient-records (hasheq 'qdmPatient (hasheq 'birthDatetime "x" 'dataElements (list (hasheq '_type "QDM::Diagnosis"))))))
                '("QDM::Patient" "QDM::Diagnosis"))
  ;; proportion: numerator only inside denominator minus exclusions
  (define (pops scoring h) (counts (populations scoring (lambda (c) (hash-ref h c #f)))))
  (check-equal? (pops "PROPORTION" (hasheq 'IPP '(patient) 'DENOM '(patient) 'DENEX '(patient) 'NUMER '(patient)))
                (hash 'IPP 1 'DENOM 1 'DENEX 1 'NUMER 0))
  ;; ratio: the numerator is not limited by the denominator's exclusions
  (check-equal? (pops "RATIO" (hasheq 'IPP '(a b) 'DENOM '(a b) 'DENEX '(a) 'NUMER '(a)))
                (hash 'IPP 2 'DENOM 2 'DENEX 1 'NUMER 1)))
