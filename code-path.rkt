#lang racket/base

;;; code-path.rkt — a retrieve's code property resolved against the data model, once.
;;;
;;; A retrieve `[Condition: "Diabetes"]` keeps the resources of its data type whose codes, at its code
;;; property (the ModelInfo's primaryCodePath: `Condition.code`), are in the value set. The data type
;;; and the property are fixed when the library is compiled, so the ModelInfo fixes the path's shape
;;; too: the JSON key each step reads, which steps are lists, the key each choice member is written
;;; under, and whether the codes sit in a CodeableConcept, a Coding or a bare code. `resolve-code-path`
;;; works that out once; `code-path-codes` then reads a resource's codes with no model lookups and no
;;; per-value type tests.
;;;
;;; The reference semantics is the generic navigation in interp.rkt (`navigate-path`, then `codes-of`,
;;; over fhir-value.rkt's `fhir-property`); a CodePath reads exactly the codes it reads, in the same
;;; order. Two escapes keep that exact:
;;; - no CodePath (#f) where the shape is not statically known: an element the model lacks, a
;;;   System-typed element, a `value` step, a compound type other than a list or a choice of model
;;;   types, a Coding element not typed as a string primitive. The caller navigates generically.
;;; - 'unshaped where one resource's JSON departs from its declared shape in a way the generic
;;;   navigation would read differently: a value carrying its own `resourceType` (typed by it, not by
;;;   the model), an object where a Coding's string element is declared (read through its `value`),
;;;   a resource of another type. The caller reads that resource generically.
;;; Every other departure reads as the generic navigation reads it: a non-object where an object is
;;; declared has no elements, JSON null and absent elements contribute nothing, a single value where a
;;; list is declared is a list of one. tests/code-path-test.rkt holds the two to the same codes.

(require racket/contract racket/match racket/list racket/string
         "model/modelinfo.rkt"
         (only-in "fhir-value.rkt" fhir-model choice-key)
         (only-in "value.rkt" cql-code cql-code?))

(provide (contract-out
          [resolve-code-path (-> string? string? (or/c code-path? #f))]   ; "FHIR.Condition" "code"
          [code-path-codes   (-> code-path? any/c (or/c (listof cql-code?) 'unshaped))])
         code-path?)

;; ---- data -------------------------------------------------------------------------------------
;; A CodePath is (code-path resource-type reader): the resourceType a resource must carry, and how
;; its codes are read.
;; A Reader is how the codes of a value of a known model type are read, one of:
;;   (field key many? next)   an element: the object's `key`, an array when many?, each value read by
;;                            next (a model-typed element; a list's elements are model-typed too)
;;   (choice alternatives)    a choice element: alternatives is (listof (cons key Reader)), the member
;;                            types' keys in the model's order; the first key present is read
;;   (coding)                 a Coding: one code from its code, system, version and display strings
;;   (bare)                   any other model type: a JSON string is a code with no system
(struct code-path (resource-type reader) #:prefab)
(struct field (key many? next) #:prefab)
(struct choice (alternatives) #:prefab)
(struct coding () #:prefab)
(struct bare () #:prefab)

;; ---- resolution: the model fixes the shape ----------------------------------------------------
(define resolved (make-hash))   ; (cons type property) -> CodePath | #f (a pure function of the model)

;; resolve-code-path : qualified-type property -> CodePath | #f
(define (resolve-code-path type property)
  (hash-ref! resolved (cons type property)
             (lambda ()
               (define r (reader-for (fhir-model) type (string-split property ".")))
               (and r (code-path (regexp-replace #rx"^FHIR[.]" type "") r)))))

;; model-type? : CType -> boolean   a model type name (a System type, "String", has no namespace)
(define (model-type? t) (and (string? t) (regexp-match? #rx"[.]" t)))

;; reader-for : model type (listof step) -> Reader | #f   how a value of `type` yields its codes once
;; the steps are read: the steps as elements, then the codes by the last value's type — a
;; CodeableConcept is read through its `coding` element (codes-of's order: CodeableConcept, then
;; Coding, then anything else)
(define (reader-for m type steps)
  (match steps
    ['()
     (cond [(model-subtype? m type "FHIR.CodeableConcept") (element-reader m type "coding" '())]
           [(model-subtype? m type "FHIR.Coding") (and (coding-elements-are-strings? m type) (coding))]
           [else (bare)])]
    [(cons "value" _) #f]                ; a primitive's System value: left to the generic navigation
    [(cons step rest) (element-reader m type step rest)]))

;; element-reader : model type element (listof step) -> Reader | #f   fhir-property's cases, statically
(define (element-reader m type element rest)
  (define t (model-element-type m type element))
  (define key (string->symbol element))
  (cond
    [(model-type? t) (let ([next (reader-for m t rest)]) (and next (field key #f next)))]
    [(list-ctype? t)
     (define el (ctype-element t))
     (and (model-type? el) (let ([next (reader-for m el rest)]) (and next (field key #t next))))]
    [(choice-ctype? t)
     (define alternatives
       (for/list ([c (in-list (choice-members t))])
         (cons (choice-key element c) (and (model-type? c) (reader-for m c rest)))))
     (and (andmap cdr alternatives) (choice alternatives))]
    [else #f]))                          ; unknown to the model, a System type, another compound type

;; coding-elements-are-strings? : model type -> boolean   code, system, version and display are each
;; a model primitive whose value is a System String (so a JSON string reads as itself)
(define (coding-elements-are-strings? m type)
  (for/and ([e (in-list '("code" "system" "version" "display"))])
    (define t (model-element-type m type e))
    (and (model-type? t) (equal? (model-element-type m t "value") "String"))))

;; ---- extraction: the JSON read along the resolved shape ---------------------------------------
;; code-path-codes : CodePath json -> (listof cql-code) | 'unshaped
(define (code-path-codes p j)
  (let/ec escape
    (define (unshaped) (escape 'unshaped))
    (unless (and (hash? j) (equal? (hash-ref j 'resourceType #f) (code-path-resource-type p))) (unshaped))
    ;; value : Reader json-value -> (listof cql-code)   one element value (wrap's cases)
    (define (value r v)
      (cond [(eq? v 'null) '()]
            [(and (hash? v) (hash-ref v 'resourceType #f)) (unshaped)]
            [else (read r v)]))
    ;; read : Reader json -> (listof cql-code)   a value of the reader's type
    (define (read r j)
      (match r
        [(field key many? next)
         (define v (and (hash? j) (hash-ref j key #f)))
         (cond [(not v) '()]
               [(and many? (list? v)) (append-map (lambda (x) (value next x)) v)]
               [else (value next v)])]
        [(choice alternatives)
         (or (and (hash? j)
                  (for/or ([a (in-list alternatives)])
                    (define v (hash-ref j (car a) #f))
                    (and v (value (cdr a) v))))
             '())]
        [(coding)
         (define (string-element k)
           (define v (and (hash? j) (hash-ref j k #f)))
           (cond [(string? v) v] [(hash? v) (unshaped)] [else #f]))
         (list (cql-code (string-element 'code) (string-element 'system) (string-element 'version) (string-element 'display)))]
        [(bare) (if (string? j) (list (cql-code j #f #f #f)) '())]))
    (read (code-path-reader p) j)))

;; ---- living examples --------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (define (codes type prop j) (code-path-codes (resolve-code-path type prop) j))
  (define cc (hasheq 'coding (list (hasheq 'system "http://snomed.info/sct" 'code "1") (hasheq 'code "2" 'display "two"))))
  ;; a CodeableConcept: its codings, in order
  (check-equal? (codes "FHIR.Condition" "code" (hasheq 'resourceType "Condition" 'code cc))
                (list (cql-code "1" "http://snomed.info/sct" #f #f) (cql-code "2" #f #f "two")))
  ;; a list of CodeableConcepts; absent and null contribute nothing; one value where a list is declared
  (check-equal? (length (codes "FHIR.Observation" "category" (hasheq 'resourceType "Observation" 'category (list cc 'null cc)))) 4)
  (check-equal? (codes "FHIR.Observation" "category" (hasheq 'resourceType "Observation")) '())
  (check-equal? (length (codes "FHIR.Observation" "category" (hasheq 'resourceType "Observation" 'category cc))) 2)
  ;; a choice: the key of the member present (medicationCodeableConcept), a Reference has no codes
  (check-equal? (codes "FHIR.MedicationRequest" "medication" (hasheq 'resourceType "MedicationRequest" 'medicationCodeableConcept cc))
                (codes "FHIR.Condition" "code" (hasheq 'resourceType "Condition" 'code cc)))
  (check-equal? (codes "FHIR.MedicationRequest" "medication" (hasheq 'resourceType "MedicationRequest" 'medicationReference (hasheq 'reference "Medication/m"))) '())
  ;; a Coding (Encounter.class in R4), a bare code (Observation.status)
  (check-equal? (codes "FHIR.Encounter" "class" (hasheq 'resourceType "Encounter" 'class (hasheq 'system "s" 'code "AMB")))
                (list (cql-code "AMB" "s" #f #f)))
  (check-equal? (codes "FHIR.Observation" "status" (hasheq 'resourceType "Observation" 'status "final")) (list (cql-code "final" #f #f #f)))
  ;; the escapes: no CodePath for what the model does not fix; 'unshaped for JSON typed by itself
  (check-false (resolve-code-path "FHIR.Condition" "noSuchElement"))
  (check-false (resolve-code-path "FHIR.Observation" "status.value"))
  (check-equal? (codes "FHIR.Condition" "code" (hasheq 'resourceType "Observation" 'code cc)) 'unshaped)
  (check-equal? (codes "FHIR.Condition" "code" (hasheq 'resourceType "Condition" 'code (hasheq 'resourceType "Basic"))) 'unshaped)
  (check-equal? (codes "FHIR.Encounter" "class" (hasheq 'resourceType "Encounter" 'class (hasheq 'code (hasheq 'value "x")))) 'unshaped)
  ;; resolved once
  (check-eq? (resolve-code-path "FHIR.Condition" "code") (resolve-code-path "FHIR.Condition" "code")))
