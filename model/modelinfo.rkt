#lang racket/base

;;; modelinfo.rkt — the data models a CQL library's `using` names, as the translator sees them.
;;;
;;; A model (from a ModelInfo, see gen-modelinfo.rkt) answers the questions translation asks:
;;;   * what is the type of element `e` on class `T`?            (property typing)
;;;   * is `T` a subtype of `U`?                                  (overloads, choices)
;;;   * how is a model type implicitly converted to a System type? (FHIRHelpers.ToString, ToConcept, …)
;;;   * which classes are retrievable, by which code path, under which profile url?  (Retrieve)
;;;   * what does `context Patient` mean?                         (the implicit context definition)
;;;
;;; A CType (the compiler's type descriptor, shared with compile.rkt) is one of:
;;;   - "Integer" | "String" | …     a System type, by simple name
;;;   - "FHIR.Encounter" | …         a model type, namespace-qualified
;;;   - "Any"                        unknown
;;;   - (list-ctype CType) | (interval-ctype CType)      (vector 'list CType) | (vector 'interval CType)
;;;   - (choice-ctype (listof CType))                     (vector 'choice (listof CType))
;;;   - (tuple-ctype (listof (cons symbol CType)))        (vector 'tuple (listof (cons symbol CType)))
;;; The vectors are the representation (equal? compares them, the harnesses print them); the
;;; constructors, predicates and accessors below are how code reads and writes them.

(require racket/contract racket/match racket/runtime-path racket/list)

;; ---- CType ----------------------------------------------------------------------------------
(provide list-ctype interval-ctype choice-ctype tuple-ctype
         list-ctype? interval-ctype? choice-ctype? tuple-ctype? compound-ctype?
         ctype-element choice-members tuple-fields)
;; list-ctype / interval-ctype : CType -> CType ; choice-ctype : (listof CType) -> CType ;
;; tuple-ctype : (listof (cons symbol CType)) -> CType
(define (list-ctype el) (vector 'list el))
(define (interval-ctype el) (vector 'interval el))
(define (choice-ctype members) (vector 'choice members))
(define (tuple-ctype fields) (vector 'tuple fields))
;; the predicates: one per compound variant, and the compound kinds together
(define (ctype-tag? t tag) (and (vector? t) (eq? (vector-ref t 0) tag)))
(define (list-ctype? t) (ctype-tag? t 'list))
(define (interval-ctype? t) (ctype-tag? t 'interval))
(define (choice-ctype? t) (ctype-tag? t 'choice))
(define (tuple-ctype? t) (ctype-tag? t 'tuple))
(define (compound-ctype? t) (vector? t))
;; ctype-element : CType -> CType | #f   the element type of a list or interval
(define (ctype-element t) (and (or (list-ctype? t) (interval-ctype? t)) (vector-ref t 1)))
;; choice-members : CType -> (listof CType)   a choice's members; any other type is its own one member
(define (choice-members t) (if (choice-ctype? t) (vector-ref t 1) (list t)))
;; tuple-fields : CType -> (listof (cons symbol CType))
(define (tuple-fields t) (if (tuple-ctype? t) (vector-ref t 1) '()))

(provide
 (struct-out model) (struct-out class-info)
 (contract-out
  [find-model          (-> string? (or/c string? #f) (or/c model? #f))]
  [model-class         (-> model? string? (or/c class-info? #f))]
  [model-element-type  (-> model? string? string? (or/c any/c #f))]
  [model-subtype?      (-> model? string? string? boolean?)]
  [model-conversion    (-> model? string? (or/c (cons/c any/c string?) #f))]
  [model-context-type  (-> model? string? (or/c string? #f))]
  [model-resolve-type  (-> model? string? (or/c string? #f))]))

;; class-info: one ClassInfo — elements: hash name(string) -> CType
(struct class-info (name base identifier primary-code-path retrievable? elements) #:transparent)
;; model: name "FHIR", url "http://hl7.org/fhir", version, classes: hash qualified-name -> class-info,
;; conversions: hash from-type -> (cons to-CType function-name), contexts: hash name -> type name
(struct model (name url version patient-class birth-date-path classes conversions contexts) #:transparent)

(define-runtime-path fhir-401 "fhir-4.0.1.rktd")

;; spec->ctype : model-info type-spec -> CType
(define (spec->ctype s)
  (match s
    [(? string? s) (system-simple s)]
    [(list 'list t) (list-ctype (spec->ctype t))]
    [(list 'interval t) (interval-ctype (spec->ctype t))]
    [(list 'choice ts ...) (choice-ctype (map spec->ctype ts))]
    [_ "Any"]))
;; "System.String" -> "String" ; "Interval<System.DateTime>" -> (vector 'interval "DateTime")
(define (system-simple s)
  (cond [(regexp-match #rx"^Interval<(.*)>$" s) => (lambda (m) (interval-ctype (system-simple (cadr m))))]
        [(regexp-match #rx"^System[.](.*)$" s) => cadr]
        [else s]))

;; load-model : path -> model
(define (load-model path)
  (match (call-with-input-file path read)
    [(list 'model name url version patient birth (list 'types types ...) (list 'conversions convs ...)
           (list 'contexts ctxs ...))
     (model name url version patient birth
            (for/hash ([t (in-list types)])
              (match t
                [(list 'type n base id pcp retr elts)
                 (values n (class-info n base id pcp retr
                                       (for/hash ([e (in-list elts)]) (values (car e) (spec->ctype (cadr e))))))]))
            (for/hash ([c (in-list convs)]) (values (car c) (cons (system-simple (cadr c)) (caddr c))))
            (for/hash ([c (in-list ctxs)]) (values (car c) (caddr c))))]))

(define cache (make-hash))
;; find-model : name version|#f -> model|#f   the models we ship (FHIR 4.0.1; 4.0.0 shares it)
(define (find-model name version)
  (and (equal? name "FHIR")
       (member version '(#f "4.0.1" "4.0.0"))
       (hash-ref! cache 'fhir-401 (lambda () (load-model fhir-401)))))

(define (model-class m name) (hash-ref (model-classes m) name #f))

;; model-element-type : model type-name element-name -> CType|#f   searching the base chain
(define (model-element-type m type elt)
  (let loop ([t type])
    (define ci (and t (model-class m t)))
    (cond [(not ci) #f]
          [(hash-ref (class-info-elements ci) elt #f)]
          [else (loop (class-info-base ci))])))

;; model-subtype? : model a b -> boolean   a = b or b is an ancestor of a
(define (model-subtype? m a b)
  (let loop ([t a])
    (cond [(not t) #f]
          [(equal? t b) #t]
          [else (define ci (model-class m t)) (and ci (loop (class-info-base ci)))])))

;; model-conversion : model type-name -> (cons to-CType function-name)|#f   its implicit conversion
;; (FHIR.id has none of its own: it converts as its base FHIR.string does)
(define (model-conversion m type)
  (let loop ([t type])
    (cond [(not t) #f]
          [(hash-ref (model-conversions m) t #f)]
          [else (define ci (model-class m t)) (and ci (loop (class-info-base ci)))])))

(define (model-context-type m name) (hash-ref (model-contexts m) name #f))

;; model-resolve-type : model name -> qualified-name|#f   "Encounter" or "FHIR.Encounter" -> "FHIR.Encounter"
(define (model-resolve-type m name)
  (define q (if (regexp-match? #rx"[.]" name) name (string-append (model-name m) "." name)))
  (and (model-class m q) q))

(module+ test
  (require rackunit)
  (define m (find-model "FHIR" "4.0.1"))
  (check-equal? (model-url m) "http://hl7.org/fhir")
  (check-equal? (class-info-primary-code-path (model-class m "FHIR.Encounter")) "type")
  (check-equal? (model-element-type m "FHIR.Encounter" "status") "FHIR.EncounterStatus")
  (check-equal? (model-element-type m "FHIR.Encounter" "id") "FHIR.id")          ; inherited from Resource
  (check-equal? (model-element-type m "FHIR.Patient" "name") (vector 'list "FHIR.HumanName"))
  (check-true (choice-ctype? (model-element-type m "FHIR.Observation" "effective")))
  (check-equal? (ctype-element (list-ctype "Integer")) "Integer")
  (check-false (ctype-element "Integer"))
  (check-equal? (choice-members (choice-ctype '("Integer" "String"))) '("Integer" "String"))
  (check-equal? (choice-members "Integer") '("Integer"))
  (check-equal? (tuple-fields (tuple-ctype (list (cons 'a "Integer")))) (list (cons 'a "Integer")))
  (check-true (compound-ctype? (interval-ctype "Date"))) (check-false (compound-ctype? "Date"))
  (check-equal? (model-conversion m "FHIR.EncounterStatus") (cons "String" "FHIRHelpers.ToString"))
  (check-equal? (model-conversion m "FHIR.Period") (cons (vector 'interval "DateTime") "FHIRHelpers.ToInterval"))
  (check-true (model-subtype? m "FHIR.Encounter" "FHIR.Resource"))
  (check-equal? (model-conversion m "FHIR.id") (cons "String" "FHIRHelpers.ToString"))
  (check-equal? (model-resolve-type m "Encounter") "FHIR.Encounter")
  (check-equal? (model-context-type m "Patient") "FHIR.Patient"))
