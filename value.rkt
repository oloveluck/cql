#lang racket/base

;;; value.rkt — the CQL runtime value model for the ELM interpreter.
;;;
;;; CQL is single-value-with-null (three-valued), unlike FHIRPath's always-a-collection model. We
;;; reuse FHIRPath's System scalar carriers (fp-decimal, fp-long, fp-quantity, temporal `moment`) and
;;; add the CQL-specific structured carriers: null, Interval, Tuple, Ratio, Code, Concept. Lists are
;;; plain Racket lists. `cql-null` is a unique singleton distinct from Boolean #f and the empty list.

(require racket/contract
         (only-in racket/string string-join)
         (only-in fhirpath/decimal fp-decimal? decimal->string)
         (only-in fhirpath/value fp-long? fp-long fp-long-v fp-quantity? fp-quantity fp-quantity-value fp-quantity-unit)
         (only-in fhirpath/temporal-ops moment? moment->string parse-fp-datetime)
         (only-in json jsexpr->string))

(provide (struct-out cql-interval) (struct-out cql-tuple) (struct-out cql-ratio) (struct-out model-node)
         current-evaluation-offset
         current-evaluation-time
         (struct-out cql-code) (struct-out cql-concept) (struct-out cql-obj)
         (contract-out
          [cql-null   any/c]
          [cql-null?  (-> any/c boolean?)]
          [cql-null-list  any/c]
          [cql-null-list? (-> any/c boolean?)]
          [cql-value? (-> any/c boolean?)]
          [render     (-> any/c string?)]))     ; a debug/round-trip rendering (not canonical CQL)

;; the null singleton
(struct cql-null-type () #:transparent
  ;; printed as what it is (a REPL showed `(cql-null-type)` in a hash of defines)
  #:property prop:custom-write (lambda (v out mode) (write-string "#<cql:null>" out)))
(define cql-null (cql-null-type))
(define (cql-null? x) (cql-null-type? x))

;; a null CAST TO a list type (`null as List<T>`): still null-ish, but list operators know it is a
;; list-typed null — Length treats it as empty (0) while list-list inclusion stays null.
(struct cql-null-list-type () #:transparent)
(define cql-null-list (cql-null-list-type))
(define (cql-null-list? x) (cql-null-list-type? x))

;; structured CQL values
(struct cql-interval (low high low-closed? high-closed?) #:transparent)
(struct cql-tuple    (fields)  #:transparent) ; fields: hash of symbol -> value
(struct cql-ratio    (numerator denominator) #:transparent) ; both fp-quantity
(struct cql-code     (code system version display) #:transparent)
(struct cql-concept  (codes display) #:transparent)
;; a typed instance value from a class selector (e.g. System.ValueSet{…}): carries its class name so
;; `is` can test the class hierarchy; fields: hash of symbol -> value
(struct cql-obj      (class fields) #:transparent)
;; A model's data at a point in a record: the model (model/modelinfo.rkt), its class there
;; ("FHIR.Observation", a qualified name) and the data there — navigated by representation.rkt. Two
;; nodes are equal when they are of the same model (eq?), class and data; the model is not hashed or
;; printed (it is large, and shared by every node of it).
(struct model-node (model type data)
  #:property prop:equal+hash
  (list (lambda (a b rec) (and (eq? (model-node-model a) (model-node-model b))
                               (rec (model-node-type a) (model-node-type b)) (rec (model-node-data a) (model-node-data b))))
        (lambda (a rec) (rec (cons (model-node-type a) (model-node-data a))))
        (lambda (a rec) (rec (cons (model-node-type a) (model-node-data a)))))
  #:property prop:custom-write
  (lambda (n port mode) (fprintf port "#<model-node ~a ~s>" (model-node-type n) (model-node-data n))))
;; current-evaluation-offset : (parameter (or minutes #f))   the evaluation request's timezone offset:
;; a DateTime with a time but no offset (a literal, or FHIR data) takes it (cql.hl7.org: "the timezone
;; offset of the evaluation request"). #f = leave such values offset-less (the expression TCK).
(define current-evaluation-offset (make-parameter #f))
;; current-evaluation-time : (parameter (or moment #f))   the moment Now(), Today() and TimeOfDay()
;; read, so a run is repeatable: a DateTime string (`"2025-06-30T12:00:00Z"`, a leading `@` allowed)
;; or a moment; #f = the clock. Measures evaluated "as of" a date, tests, explanations to compare.
(define current-evaluation-time
  (make-parameter #f (lambda (v)
                       (cond [(or (not v) (moment? v)) v]
                             [(and (string? v) (parse-fp-datetime (regexp-replace #rx"^@" v ""))) => values]
                             [else (raise-argument-error 'current-evaluation-time "a DateTime like \"2025-06-30T12:00:00Z\"" v)]))))

;; A CqlValue is one of:
;;  - cql-null                          the null singleton (distinct from #f and '())
;;  - cql-null-list                     a null cast to a list type (`null as List<T>`)
;;  - Boolean                           System.Boolean
;;  - exact-integer                     System.Integer
;;  - fp-long                           System.Long        (from fhirpath/value)
;;  - fp-decimal                        System.Decimal     (precision-preserving; from fhirpath/decimal)
;;  - String                            System.String
;;  - moment                            System.Date/DateTime/Time (from fhirpath/temporal-ops)
;;  - fp-quantity                       System.Quantity    (from fhirpath/value)
;;  - (Listof CqlValue)                 System.List<T>
;;  - cql-interval                      System.Interval<T> (also an uncertainty range)
;;  - cql-tuple                         System.Tuple / structured value
;;  - cql-ratio / cql-code / cql-concept
;;  - cql-obj                           a typed class instance (System.ValueSet{…} etc)
;;  - model-node                        a model's value: a record, an element of one, a primitive
;; cql-value? recognises the domain; `render` and the interpreter template against it.
(define (cql-value? x)
  (or (cql-null? x) (cql-null-list? x) (boolean? x) (exact-integer? x) (fp-long? x) (fp-decimal? x)
      (string? x) (moment? x) (fp-quantity? x) (list? x)
      (cql-interval? x) (cql-tuple? x) (cql-ratio? x) (cql-code? x) (cql-concept? x) (cql-obj? x) (model-node? x)))

;; a stable textual rendering, used for debugging and (indirectly) test diagnostics
(define (render v)
  (cond
    [(cql-null? v) "null"]
    [(cql-null-list? v) "null"]
    [(boolean? v) (if v "true" "false")]
    [(exact-integer? v) (number->string v)]
    [(fp-long? v) (string-append (number->string (fp-long-v v)) "L")]
    [(fp-decimal? v) (decimal->string v)]
    [(string? v) v]
    [(moment? v) (moment->string v)]
    [(fp-quantity? v) (format "~a '~a'" (let ([q (fp-quantity-value v)]) (if (fp-decimal? q) (decimal->string q) q))
                              (fp-quantity-unit v))]
    [(list? v) (string-append "{" (string-join (map render v) ", ") "}")]
    [(cql-interval? v)
     (define (bound b) (if b (render b) "null"))     ; a #f bound is unbounded -> render as null
     (format "~a~a, ~a~a"
             (if (cql-interval-low-closed? v) "[" "(")
             (bound (cql-interval-low v)) (bound (cql-interval-high v))
             (if (cql-interval-high-closed? v) "]" ")"))]
    [(cql-tuple? v)
     (string-append "Tuple{" (string-join (for/list ([(k val) (in-hash (cql-tuple-fields v))])
                                            (format "~a: ~a" k (render val))) ", ") "}")]
    [(cql-ratio? v) (format "~a : ~a" (render (cql-ratio-numerator v)) (render (cql-ratio-denominator v)))]
    [(cql-code? v) (format "Code '~a' from ~a" (cql-code-code v) (cql-code-system v))]
    [(cql-concept? v) (format "Concept{~a}" (string-join (map render (cql-concept-codes v)) ", "))]
    ;; a model's data (a FHIR Quantity, a resource) is the JSON it is, not a Racket struct
    [(model-node? v) (with-handlers ([exn:fail? (lambda (_) (format "~a" v))]) (jsexpr->string (model-node-data v)))]
    [else (format "~a" v)]))

(module+ test
  (require rackunit)
  ;; a null prints as one (it printed as `(cql-null-type)` in a hash of defines)
  (check-equal? (format "~s" (hash "Is 65 Or Older" cql-null)) "#hash((\"Is 65 Or Older\" . #<cql:null>))")
  (check-true (cql-null? cql-null))
  (check-false (cql-null? #f))
  (check-false (cql-null? '()))
  (check-true (cql-value? 5))
  (check-true (cql-value? (cql-interval 1 10 #t #t)))
  (check-equal? (render (list 1 2 3)) "{1, 2, 3}")
  (check-equal? (render (cql-interval 1 10 #t #f)) "[1, 10)"))
