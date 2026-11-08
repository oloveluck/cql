#lang racket/base

;;; structure-definitions.rkt — a data model made from FHIR logical models.
;;;
;;; A logical model (a StructureDefinition of kind `logical`: what FSH's `Logical:` compiles to, or
;;; any IG's) already says what a model's classes and elements are. This turns a set of them into a
;;; Model (model/modelinfo.rkt) and its representation, so a model designed in FSH is queried in CQL
;;; with `using <Name> version '<v>'` and nothing else written by hand.
;;;
;;; What it makes:
;;; - a class `<Name>.<SD name>` per logical model, retrievable, with its base when the base is in the
;;;   set; an element with children of its own (a BackboneElement) becomes a class `<Name>.<SD>.<Elem>`;
;;; - element types: the FHIR datatypes CQL has as System types become them (string, code, uri, id …
;;;   -> String; boolean; integer -> Integer; decimal; date; dateTime and instant -> DateTime; time;
;;;   Coding -> Code; CodeableConcept -> Concept; Quantity and its kinds -> Quantity; Period ->
;;;   Interval<DateTime>; Range -> Interval<Quantity>), so the model stands alone: no FHIR model, no
;;;   FHIRHelpers. A Reference is the class `<Name>.Reference`; another logical model of the set is its
;;;   class; anything else is Any. A repeating element is a list; `x[x]` is a choice `x`.
;;; - the primary code path of a class: the one given, else its element `code` when that holds codes,
;;;   else its first element that does;
;;; - the Patient context, when a patient class is named: keyed by its element (default `id`), its
;;;   birth date the element given (if any), and every class relating to it by each Reference element
;;;   whose target is the patient class (matched by canonical or by name);
;;; - the representation of FHIR JSON, which is how a logical model's instances are written:
;;;   `resourceType`, `whenDateTime`, `{"reference": "Member/p1"}`, a Period's start and end, a
;;;   CodeableConcept's coding and text, a Quantity's code (UCUM) or unit.

(require racket/contract racket/list racket/string racket/match
         "modelinfo.rkt")

(provide
 (contract-out
  ;; structure-definitions->model : sds #:name #:version #:url … -> model
  [structure-definitions->model
   (->* ((listof hash?) #:name string? #:version string? #:url string?)
        (#:patient (or/c string? #f) #:patient-key string? #:birth-date (or/c string? #f)
         #:code-paths (hash/c string? string?))
        model?)]
  [logical-model? (-> hash? boolean?)]
  [logical-representation list?]))

(define (logical-model? sd)
  (and (equal? (hash-ref sd 'resourceType #f) "StructureDefinition") (equal? (hash-ref sd 'kind #f) "logical")))

;; the FHIR datatypes CQL has as System types
(define system-types
  (hash "string" "System.String" "code" "System.String" "id" "System.String" "uri" "System.String"
        "url" "System.String" "canonical" "System.String" "oid" "System.String" "uuid" "System.String"
        "markdown" "System.String" "base64Binary" "System.String" "xhtml" "System.String"
        "boolean" "System.Boolean" "integer" "System.Integer" "positiveInt" "System.Integer"
        "unsignedInt" "System.Integer" "integer64" "System.Long" "decimal" "System.Decimal"
        "date" "System.Date" "dateTime" "System.DateTime" "instant" "System.DateTime" "time" "System.Time"
        "Coding" "System.Code" "CodeableConcept" "System.Concept"
        "Quantity" "System.Quantity" "SimpleQuantity" "System.Quantity" "Age" "System.Quantity"
        "Duration" "System.Quantity" "Distance" "System.Quantity" "Count" "System.Quantity"
        "Period" '(interval "System.DateTime") "Range" '(interval "System.Quantity")))

(define (last-segment url) (last (string-split (car (string-split url "|")) "/")))
(define (cap s) (if (string=? s "") s (string-append (string-upcase (substring s 0 1)) (substring s 1))))

;; the suffixes a member of a choice is written with, by its CQL type: the FHIR datatypes that map to it
(define choice-suffixes
  (let ([by-type (for/fold ([h (hash)]) ([(code t) (in-hash system-types)])
                   (define key (match t [(? string? s) (regexp-replace #rx"^System[.]" s "")]
                                        [(list 'interval x) (list 'interval (regexp-replace #rx"^System[.]" x ""))]))
                   (hash-update h key (lambda (l) (cons (cap code) l)) '()))])
    (sort (for/list ([(k v) (in-hash by-type)]) (cons k (sort v string<?))) string<? #:key (lambda (p) (format "~a" (car p))))))
(define logical-representation
  `((type-key . resourceType) (choice . suffixed) (reference . typed)
    (shapes (interval-low . start) (interval-high . end) (concept-codes . coding) (concept-display . text)
            (quantity-units code unit))
    (choice-suffixes ,@choice-suffixes)))


(define (structure-definitions->model sds #:name name #:version version #:url url
                                      #:patient [patient #f] #:patient-key [patient-key "id"]
                                      #:birth-date [birth-date #f] #:code-paths [code-paths (hash)])
  (define logicals (filter logical-model? sds))
  (define (sd-name sd) (or (hash-ref sd 'name #f) (hash-ref sd 'id #f) (last-segment (hash-ref sd 'url ""))))
  ;; a logical model of the set by its canonical, its type, or its name
  (define by-key (for*/hash ([sd (in-list logicals)]
                             [k (in-list (filter string? (list (hash-ref sd 'url #f) (hash-ref sd 'type #f) (sd-name sd) (hash-ref sd 'id #f))))])
                   (values k sd)))
  (define (class-of sd) (string-append name "." (sd-name sd)))
  (define (find-sd ref) (or (hash-ref by-key ref #f) (hash-ref by-key (last-segment ref) #f)))
  (define patient-sd (and patient (find-sd patient)))
  (when (and patient (not patient-sd))
    (error 'structure-definitions->model "no logical model ~a among ~a" patient (map sd-name logicals)))
  (define reference-class (string-append name ".Reference"))

  ;; type-spec : type-entry -> TypeSpec   one ElementDefinition.type entry
  (define (type-spec t)
    (define code (hash-ref t 'code ""))
    (cond [(hash-ref system-types code #f)]
          [(equal? code "Reference") reference-class]
          [(find-sd code) => class-of]
          [else "System.Any"]))
  ;; references-patient? : element -> boolean   a Reference whose target is the patient class
  (define (references-patient? e)
    (and patient-sd
         (for*/or ([t (in-list (hash-ref e 'type '()))] #:when (equal? (hash-ref t 'code #f) "Reference")
                   [p (in-list (hash-ref t 'targetProfile '()))])
           (eq? (find-sd p) patient-sd))))

  ;; classes-of : sd -> (listof TypeForm)   the logical model's class and its nested classes
  (define (classes-of sd)
    (define els (let ([x (or (hash-ref sd 'snapshot #f) (hash-ref sd 'differential (hasheq)))]) (hash-ref x 'element '())))
    (define root (if (pair? els) (hash-ref (car els) 'path) (sd-name sd)))
    (define (children-of path) (filter (lambda (e) (equal? (parent-path (hash-ref e 'path "")) path)) els))
    (define (class-form cname path base retrievable? label)
      (define elements
        (for/list ([e (in-list (children-of path))])
          (define raw (last (string-split (hash-ref e 'path) ".")))
          (define elem (regexp-replace #rx"\\[x\\]$" raw ""))
          (define nested? (pair? (children-of (hash-ref e 'path))))
          (define types (hash-ref e 'type '()))
          (define one (cond [nested? (string-append cname "." (cap elem))]
                            [(> (length types) 1) (cons 'choice (map type-spec types))]
                            [(pair? types) (type-spec (car types))]
                            [else "System.Any"]))
          (define many? (let ([mx (hash-ref e 'max "1")]) (not (member mx '("0" "1")))))
          (list elem (if many? (list 'list one) one) #f (and (references-patient? e) elem) (and nested? (hash-ref e 'path)) (cap elem))))
      (define pcp (or (hash-ref code-paths (sd-name sd) #f) (and retrievable? (primary-code-path elements))))
      (cons (list 'type cname base #f label pcp retrievable?
                  (map (lambda (x) (list (car x) (cadr x) #f)) elements)
                  (for/list ([x (in-list elements)] #:when (cadddr x)) (list "Patient" (cadddr x)))
                  '())
            (append* (for/list ([x (in-list elements)] #:when (list-ref x 4))
                       (class-form (string-append cname "." (list-ref x 5)) (list-ref x 4) #f #f (list-ref x 5))))))
    (define base (let ([b (hash-ref sd 'baseDefinition #f)]) (and b (find-sd b) (class-of (find-sd b)))))
    (class-form (class-of sd) root base #t (sd-name sd)))

  (define types (append (append-map classes-of logicals)
                        (list (list 'type reference-class #f #f "Reference" #f #f
                                    '(("reference" "System.String" #f) ("display" "System.String" #f)) '() '()))))
  (define patient-class (and patient-sd (class-of patient-sd)))
  (form->model
   (list 'model name url version '(required)
         (list 'patient patient-class birth-date)
         (cons 'types types)
         '(conversions)
         (cons 'contexts (if patient-class (list (list "Patient" patient-key birth-date patient-class)) '())))
   logical-representation))

(define (parent-path p) (let ([parts (string-split p ".")]) (and (> (length parts) 1) (string-join (drop-right parts 1) "."))))

;; primary-code-path : (listof element-info) -> name|#f   `code` when it holds codes, else the first that does
(define (codes-type? t) (match t [(or "System.Code" "System.Concept") #t] [(list 'list x) (codes-type? x)] [(cons 'choice xs) (ormap codes-type? xs)] [_ #f]))
(define (primary-code-path elements)
  (or (for/first ([x (in-list elements)] #:when (and (equal? (car x) "code") (codes-type? (cadr x)))) "code")
      (for/first ([x (in-list elements)] #:when (codes-type? (cadr x))) (car x))))

(module+ test
  (require rackunit)
  ;; the two logical models FSH compiles from:
  ;;   Logical: Member  * id 1..1 string  * born 0..1 date
  ;;   Logical: Visit   * reason 1..1 CodeableConcept  * when[x] 0..1 dateTime or Period
  ;;                    * member 1..1 Reference(Member)  * tag 0..* string  * part 0..* BackboneElement
  ;;                      * kind 1..1 code
  (define (el path . kvs) (apply hasheq 'path path kvs))
  (define (sd nm els) (hasheq 'resourceType "StructureDefinition" 'kind "logical" 'name nm 'id nm
                              'url (string-append "http://example.org/clinic/StructureDefinition/" nm)
                              'type (string-append "http://example.org/clinic/StructureDefinition/" nm)
                              'baseDefinition "http://hl7.org/fhir/StructureDefinition/Base" 'differential (hasheq 'element els)))
  (define member (sd "Member" (list (el "Member") (el "Member.id" 'min 1 'max "1" 'type (list (hasheq 'code "string")))
                                    (el "Member.born" 'max "1" 'type (list (hasheq 'code "date"))))))
  (define visit (sd "Visit" (list (el "Visit")
                                  (el "Visit.reason" 'max "1" 'type (list (hasheq 'code "CodeableConcept")))
                                  (el "Visit.when[x]" 'max "1" 'type (list (hasheq 'code "dateTime") (hasheq 'code "Period")))
                                  (el "Visit.member" 'max "1" 'type (list (hasheq 'code "Reference" 'targetProfile '("http://hl7.org/fhir/StructureDefinition/Member"))))
                                  (el "Visit.tag" 'max "*" 'type (list (hasheq 'code "string")))
                                  (el "Visit.part" 'max "*" 'type (list (hasheq 'code "BackboneElement")))
                                  (el "Visit.part.kind" 'max "1" 'type (list (hasheq 'code "code"))))))
  (define m (structure-definitions->model (list member visit) #:name "Clinic" #:version "1.0" #:url "http://example.org/clinic"
                                          #:patient "Member" #:birth-date "born"))
  (check-equal? (model-element-type m "Clinic.Visit" "reason") "Concept")
  (check-true (choice-ctype? (model-element-type m "Clinic.Visit" "when")))
  (check-equal? (choice-members (model-element-type m "Clinic.Visit" "when")) (list "DateTime" (interval-ctype "DateTime")))
  (check-equal? (model-element-type m "Clinic.Visit" "tag") (list-ctype "String"))
  (check-equal? (model-element-type m "Clinic.Visit" "part") (list-ctype "Clinic.Visit.Part"))
  (check-equal? (model-element-type m "Clinic.Visit.Part" "kind") "String")
  (check-equal? (model-element-type m "Clinic.Visit" "member") "Clinic.Reference")
  (check-equal? (class-info-primary-code-path (model-class m "Clinic.Visit")) "reason")
  ;; the patient: matched by name though FSH wrote FHIR's own canonical for Member
  (check-equal? (model-relationships m "Clinic.Visit" "Patient") '("member"))
  (check-equal? (model-context-type m "Patient") "Clinic.Member")
  (check-equal? (model-birth-date-path m) "born")
  (check-equal? (model-representation-settings m) logical-representation)
  (check-exn #rx"no logical model Nobody" (lambda () (structure-definitions->model (list member) #:name "C" #:version "1" #:url "u" #:patient "Nobody"))))
