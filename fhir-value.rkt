#lang racket/base

;;; fhir-value.rkt — FHIR data as CQL values, navigated through the data model.
;;;
;;; A FhirNode is (fhir-node type json): a model type name ("FHIR.Observation", "FHIR.CodeableConcept",
;;; "FHIR.dateTime", "FHIR.Encounter.Hospitalization") and the resource's JSON at that point — a hash
;;; for a complex value, the raw JSON value for a primitive. Property access follows the ModelInfo
;;; (model/modelinfo.rkt): the element's declared type wraps the child; a choice element `value` reads
;;; whichever `valueX` key is present and takes that member's type; a list-typed element is a list
;;; (absent = empty); `.value` of a FHIR primitive is its System value (FHIR.date -> Date,
;;; FHIR.decimal -> Decimal, FHIR.code -> String, …). FHIRHelpers, written in CQL, does the rest.

(require racket/contract racket/match racket/string racket/list
         "model/modelinfo.rkt"
         "value.rkt"
         (only-in fhirpath/decimal parse-decimal)
         (only-in fhirpath/temporal-ops parse-fp-date parse-fp-datetime parse-fp-time
                  moment moment-kind moment-year moment-month moment-day moment-hour moment-minute moment-second
                  moment-frac moment-tzmin moment-prec))

(provide (contract-out
          [fhir-property  (-> fhir-node? string? any/c)]
          [fhir-is?       (-> fhir-node? string? boolean?)]
          [resource->node (-> hash? fhir-node?)]
          [fhir-model     (-> model?)]))


(define (fhir-model) (find-model "FHIR" "4.0.1"))

;; resource->node : jsexpr -> FhirNode   a resource, typed by its resourceType
(define (resource->node j) (fhir-node (string-append "FHIR." (hash-ref j 'resourceType "Resource")) j))

(define (local t) (let ([m (regexp-match #rx"[.]([^.]+)$" t)]) (if m (cadr m) t)))
(define (cap s) (if (string=? s "") s (string-append (string-upcase (substring s 0 1)) (substring s 1))))

;; fhir-property : FhirNode string -> CqlValue
(define (fhir-property n name)
  (define m (fhir-model))
  (define j (fhir-node-json n))
  (cond
    [(not (hash? j))                                  ; a primitive: only .value is data
     (if (equal? name "value") (primitive-value m (fhir-node-type n) j) cql-null)]
    [else
     (define t (model-element-type m (fhir-node-type n) name))
     (match t
       [#f (let ([v (hash-ref j (string->symbol name) #f)]) (if v (wrap "Any" v) cql-null))]
       [(? choice-ctype?)
        (or (for/or ([c (in-list (choice-members t))])
              (define v (hash-ref j (string->symbol (string-append name (cap (local c)))) #f))
              (and v (wrap c v)))
            cql-null)]
       [(? list-ctype?)
        (define el (ctype-element t))
        (define v (hash-ref j (string->symbol name) #f))
        (cond [(list? v) (for/list ([x (in-list v)]) (wrap el x))]
              [v (list (wrap el v))]
              [else '()])]
       [_ (let ([v (hash-ref j (string->symbol name) #f)]) (if v (wrap t v) cql-null))])]))

;; wrap : CType jsexpr -> CqlValue   a System-typed element is converted, a model-typed one wrapped;
;; an abstract Resource takes the JSON's resourceType
(define (wrap t v)
  (cond
    [(eq? v 'null) cql-null]
    [(and (hash? v) (hash-ref v 'resourceType #f)) (resource->node v)]
    [(and (string? t) (regexp-match? #rx"[.]" t)) (fhir-node t v)]
    [(string? t) (system-value t v)]
    [else (fhir-node "Any" v)]))

;; primitive-value : model type json -> System value   by the type's `value` element
(define (primitive-value m type j)
  (define st (model-element-type m type "value"))
  (system-value (if (string? st) st "String") j))

;; with-offset : moment -> moment   a time without an offset takes the evaluation's
(define (with-offset m)
  (define o (current-evaluation-offset))
  (if (and o (moment-hour m) (not (moment-tzmin m)))
      (moment (moment-kind m) (moment-year m) (moment-month m) (moment-day m) (moment-hour m) (moment-minute m)
              (moment-second m) (moment-frac m) o (moment-prec m))
      m))
(define (system-value st v)
  (match st
    ["Boolean" (if (boolean? v) v cql-null)]
    ["Integer" (if (exact-integer? v) v cql-null)]
    ["Decimal" (cond [(number? v) (or (parse-decimal (number->string (if (and (inexact? v) (integer? v)) (inexact->exact v) v))) cql-null)]
                     [(string? v) (or (parse-decimal v) cql-null)]
                     [else cql-null])]
    ["Date" (or (and (string? v) (parse-fp-date v)) cql-null)]
    ["DateTime" (or (and (string? v) (let ([m (or (parse-fp-datetime v) (parse-fp-date v))]) (and m (with-offset m)))) cql-null)]
    ["Time" (or (and (string? v) (parse-fp-time v)) cql-null)]
    [_ (cond [(string? v) v] [(number? v) v] [(boolean? v) v] [else cql-null])]))

;; fhir-is? : FhirNode qualified-name -> boolean   "{http://hl7.org/fhir}Period" / "FHIR.Period"
(define (fhir-is? n qn)
  (define want (cond [(regexp-match #rx"^{[^}]*}(.*)$" qn) => (lambda (m) (string-append "FHIR." (cadr m)))]
                     [(regexp-match? #rx"[.]" qn) qn]
                     [else (string-append "FHIR." qn)]))
  (model-subtype? (fhir-model) (fhir-node-type n) want))

(module+ test
  (require rackunit (only-in fhirpath/temporal-ops moment?))
  (define obs (resource->node (hasheq 'resourceType "Observation" 'status "final"
                                      'code (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "1-1")))
                                      'valueQuantity (hasheq 'value 7.5 'unit "%")
                                      'effectiveDateTime "2021-03-04T10:00:00-07:00")))
  (check-equal? (fhir-node-type (fhir-property obs "status")) "FHIR.ObservationStatus")
  (check-equal? (fhir-property (fhir-property obs "status") "value") "final")
  (check-equal? (fhir-node-type (fhir-property obs "value")) "FHIR.Quantity")         ; choice by key
  (check-true (moment? (fhir-property (fhir-property obs "effective") "value")))
  (check-equal? (length (fhir-property (fhir-property obs "code") "coding")) 1)
  (check-equal? (fhir-property obs "category") '())                                  ; absent list
  (check-true (fhir-is? (fhir-property obs "value") "{http://hl7.org/fhir}Quantity"))
  (check-false (fhir-is? (fhir-property obs "value") "{http://hl7.org/fhir}Period")))
