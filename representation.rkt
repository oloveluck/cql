#lang racket/base

;;; representation.rkt — any model's data as CQL values, navigated through the model.
;;;
;;; A model (model/modelinfo.rkt) says what its classes and elements ARE; a representation says how its
;;; data LOOKS, so the same engine reads FHIR, a model built on FHIR, or a model designed tomorrow. Data
;;; is JSON-shaped — objects, arrays and scalars, which is how FHIR JSON, NDJSON, a SQL row or a CSV
;;; record arrive — and a representation is a declaration about that shape, data rather than code:
;;;
;;; A Representation is (representation type-key choice scalar-element reference shapes)
;;;   type-key       : symbol | #f        the key a record names its own class under (FHIR: resourceType);
;;;                                       #f: a record has the type it is declared or retrieved as
;;;   choice         : 'suffixed | 'plain how a choice element is written: under name+Type
;;;                                       (FHIR: valueQuantity) or under its own name
;;;   choice-suffixes : hash CType -> (listof suffix)   for 'suffixed: the suffixes a member type is
;;;                                       written with, when not its own class name — a logical model's
;;;                                       Interval<DateTime> member is written whenPeriod, its String
;;;                                       member whenString, whenCode or whenUri
;;;   scalar-element : string | #f        the element through which a class may be written as a bare
;;;                                       scalar (FHIR's primitives: a JSON string is a FHIR.string's
;;;                                       `value`); #f: classes are always objects
;;;;   reference      : 'typed | 'key      how a value names another record: 'typed — an object whose
;;;                                       `reference` is "Type/id" (any prefix) or a string of that
;;;                                       form; 'key — the value (or its `id`) is the other record's key
;;;   shapes         : Shapes             how System values that are not scalars are written
;;; A Shapes is (shapes interval-low interval-high concept-codes concept-display quantity-units):
;;;   the keys of an interval's bounds (CQL's: low, high; FHIR's Period: start, end), of a Concept's
;;;   codes and display (CQL's: codes, display; FHIR's CodeableConcept: coding, text), and the keys a
;;;   Quantity's unit is read from, first present wins (CQL's: unit; FHIR's Quantity: code, unit). A
;;;   Code is written with CQL's own names: code, system, version, display.
;;; A model's representation is declared beside its model file (`<model>.representation.rktd`, the
;;; settings below as data; FHIR's is model/fhir-4.0.1.representation.rktd) or registered for its name
;;; by a program; else it is the representation of a model it builds on; else `default-representation`.
;;;
;;; A ModelNode (value.rkt) is (model-node model type data): a value of the model's class `type` (a
;;; qualified name: "FHIR.Observation") and its data at that point. Property access follows the model:
;;; the element's declared type wraps the child; a choice reads whichever member is present and takes
;;; that member's type; a list-typed element is a list (absent = empty); the scalar element of a class
;;; written as a scalar is that scalar as a System value. An element the model maps elsewhere (a
;;; `target`) is an error: mapping expressions are not implemented, and reading nothing would be wrong.
;;;
;;; Codes are read the model's way too: a class with an implicit conversion to System.Code holds a
;;; code in its `code`, `system`, `version` and `display` elements (System.Code's own fields); a class
;;; converting to System.Concept holds its codes in its element that is a list of such a class.

(require racket/contract racket/match racket/string racket/list
         "model/modelinfo.rkt"
         "value.rkt"
         (only-in fhirpath/decimal parse-decimal)
         (only-in fhirpath/quantity make-quantity)
         (only-in fhirpath/temporal-ops parse-fp-date parse-fp-datetime parse-fp-time
                  moment moment-kind moment-year moment-month moment-day moment-hour moment-minute moment-second
                  moment-frac moment-tzmin moment-prec))

(provide
 (struct-out representation) (struct-out shapes)
 (contract-out
  [default-representation representation?]
  [register-representation! (-> string? representation? void?)]
  [model-representation (-> model? representation?)]
  [settings->representation (-> list? representation?)]
  [cql-shapes shapes?]
  [record->node   (->* (model? any/c) ((or/c string? #f)) model-node?)]
  [node-property  (-> model-node? string? any/c)]
  [node-is?       (-> model-node? string? boolean?)]
  [node-codes     (-> any/c (listof cql-code?))]
  [qualified-type (-> model? string? string?)]
  [model-type-name? (-> string? boolean?)]
  [choice-key     (-> representation? string? any/c (or/c symbol? #f))]
  [choice-keys    (-> representation? string? any/c (listof symbol?))]
  [code-class?    (-> model? string? boolean?)]
  [concept-codings-element (-> model? string? (or/c string? #f))]
  [reference-key  (-> representation? any/c (or/c (cons/c (or/c string? #f) string?) #f))]
  [record-type-name (-> model? hash? (or/c string? #f))]))

(struct representation (type-key choice scalar-element reference shapes choice-suffixes) #:transparent)
(struct shapes (interval-low interval-high concept-codes concept-display quantity-units) #:transparent)
(define cql-shapes (shapes 'low 'high 'codes 'display '(unit)))
(define default-representation (representation #f 'plain #f 'key cql-shapes (hash)))

;; settings->representation : (listof (cons key value)) -> Representation   the declaration as data
;; (a model directory's `<model>.representation.rktd`): type-key, choice, scalar-element, reference
(define (settings->representation s)
  (define (get k d) (let ([p (assq k s)]) (if p (cdr p) d)))
  (define tk (get 'type-key #f))
  (define sh (get 'shapes '()))
  (define (sget k d) (let ([p (assq k sh)]) (if p (cdr p) d)))
  (representation (and tk (if (symbol? tk) tk (string->symbol tk)))
                  (get 'choice 'plain) (get 'scalar-element #f) (get 'reference 'key)
                  (shapes (sget 'interval-low 'low) (sget 'interval-high 'high) (sget 'concept-codes 'codes)
                          (sget 'concept-display 'display) (sget 'quantity-units '(unit)))
                  (for/hash ([e (in-list (get 'choice-suffixes '()))]) (values (settings-ctype (car e)) (cdr e)))))
;; settings-ctype : datum -> CType   "DateTime" | (interval "DateTime") | (list "String") in a settings file
(define (settings-ctype d)
  (match d
    [(list 'interval x) (interval-ctype (settings-ctype x))]
    [(list 'list x) (list-ctype (settings-ctype x))]
    [(? string? x) x]))

(define representations (make-hash))   ; model name -> Representation
(define (register-representation! name r) (hash-set! representations name r))
(define by-model (make-weak-hasheq))
;; model-representation : model -> Representation   registered, else inherited, else the default
(define (model-representation m)
  (hash-ref! by-model m (lambda () (or (own-representation m) default-representation))))
(define (own-representation m)
  (or (let ([s (model-representation-settings m)]) (and s (settings->representation s)))
      (hash-ref representations (model-name m) #f)
      (for/or ([r (in-list (model-required-models m))]) (own-representation r))))

;; ---- types ------------------------------------------------------------------------------------
;; model-type-name? : CType-string -> boolean   a model class name ("FHIR.Period"), not a System one
(define (model-type-name? t) (regexp-match? #rx"[.]" t))
(define (local t) (let ([m (regexp-match #rx"[.]([^.]+)$" t)]) (if m (cadr m) t)))

;; qualified-type : model name -> qualified name   "{url}X" | "Ns.X" | "X" (the model's own X)
(define qualified-names (make-weak-hasheq))   ; model -> (hash name -> qualified name)
(define (qualified-type m qn)
  (hash-ref! (hash-ref! qualified-names m make-hash) qn (lambda () (qualified-type* m qn))))
(define (qualified-type* m qn)
  (cond [(regexp-match #rx"^{([^}]*)}(.*)$" qn)
         => (lambda (x)
              (define target (if (equal? (cadr x) (model-url m)) m (or (find-model-by-url (cadr x)) m)))
              (string-append (model-name target) "." (caddr x)))]
        [(regexp-match? #rx"[.]" qn) qn]
        [else (or (model-resolve-type m qn) (string-append (model-name m) "." qn))]))

;; record-type-name : model json -> qualified name|#f   the class a record names under the type key
(define (record-type-name m j)
  (define tk (representation-type-key (model-representation m)))
  (define v (and tk (hash-ref j tk #f)))
  (and (string? v) (or (model-resolve-type m v) (string-append (model-name m) "." v))))

;; record->node : model json [declared] -> ModelNode   a record, typed by its own type key, else as declared
(define (record->node m j [declared #f])
  (model-node m (or (and (hash? j) (record-type-name m j)) declared (string-append (model-name m) ".Any")) j))

;; choice-keys : rep element member-type -> (listof symbol)   the keys a choice member may be written
;; under: name + each suffix the representation gives the type, else name + the class's own name
(define (cap s) (if (string=? s "") s (string-append (string-upcase (substring s 0 1)) (substring s 1))))
(define (choice-keys r name member)
  (cond [(not (eq? (representation-choice r) 'suffixed)) (list (string->symbol name))]
        [(hash-ref (representation-choice-suffixes r) member #f)
         => (lambda (sfx) (for/list ([x (in-list sfx)]) (string->symbol (string-append name x))))]
        [(string? member) (list (string->symbol (string-append name (cap (local member)))))]
        [else '()]))
;; choice-key : rep element member-type -> symbol|#f   the one key, when the member has exactly one
(define (choice-key r name member)
  (define ks (choice-keys r name member))
  (and (= 1 (length ks)) (car ks)))

;; ---- properties ---------------------------------------------------------------------------------
;; node-property : ModelNode string -> CqlValue
(define (node-property n name)
  (define m (model-node-model n))
  (define r (model-representation m))
  (define j (model-node-data n))
  (define type (model-node-type n))
  (cond
    [(not (hash? j))                                   ; a class written as a scalar: only its scalar element
     (if (equal? name (representation-scalar-element r)) (scalar-value m type j) cql-null)]
    [else
     (define t (model-element-type m type name))
     (define target (and t (model-element-target m type name)))
     (when target
       (error 'cql (format "~a.~a maps to `~a`: element mappings are not supported" type name target)))
     (match t
       [#f (let ([v (hash-ref j (string->symbol name) #f)]) (if v (wrap m r "Any" v) cql-null))]
       [(? choice-ctype?)
        (if (eq? (representation-choice r) 'suffixed)
            (or (for*/or ([c (in-list (choice-members t))] [k (in-list (choice-keys r name c))])
                  (define v (hash-ref j k #f))
                  (and v (wrap m r c v)))
                cql-null)
            (let ([v (hash-ref j (string->symbol name) #f)])
              (if v (wrap m r (plain-member m r t v) v) cql-null)))]
       [(? list-ctype?)
        (define el (ctype-element t))
        (define v (hash-ref j (string->symbol name) #f))
        (cond [(list? v) (for/list ([x (in-list v)]) (wrap m r el x))]
              [v (list (wrap m r el v))]
              [else '()])]
       [_ (let ([v (hash-ref j (string->symbol name) #f)]) (if v (wrap m r t v) cql-null))])]))

;; plain-member : model rep choice-ctype json -> member CType   which member a plain choice's value is:
;; a record naming its class, else the first member whose shape the value has
(define (plain-member m r t v)
  (define members (choice-members t))
  (or (and (hash? v) (record-type-name m v))
      (for/first ([c (in-list members)] #:when (fits? c v)) c)
      (car members)))
(define (fits? c v)
  (match c
    [(? string? s) #:when (model-type-name? s) (hash? v)]
    ["Boolean" (boolean? v)]
    [(or "Integer" "Long") (exact-integer? v)]
    ["Decimal" (number? v)]
    [(or "String" "Date" "DateTime" "Time") (string? v)]
    [(? interval-ctype?) (hash? v)]
    [_ #f]))

;; wrap : model rep CType json -> CqlValue   a System-typed element is converted, a model-typed one
;; wrapped; a record naming its own class takes that class
(define (wrap m r t v)
  (cond
    [(eq? v 'null) cql-null]
    [(and (hash? v) (representation-type-key r) (hash-ref v (representation-type-key r) #f)) (record->node m v)]
    [(and (string? t) (model-type-name? t)) (model-node m t v)]
    [(string? t) (system-value t v (representation-shapes r))]
    [(and (interval-ctype? t) (hash? v)) (interval-value m r t v)]
    [else (model-node m (string-append (model-name m) ".Any") v)]))

;; interval-value : model rep interval-ctype json -> Interval   a System interval written as {low, high}
(define (interval-value m r t v)
  (define pt (ctype-element t))
  (define sh (representation-shapes r))
  (define (bound k) (let ([x (hash-ref v k #f)]) (if x (wrap m r pt x) cql-null)))
  (cql-interval (bound (shapes-interval-low sh)) (bound (shapes-interval-high sh)) (hash-ref v 'lowClosed #t) (hash-ref v 'highClosed #t)))

;; scalar-value : model type json -> System value   by the type's scalar element
(define (scalar-value m type j)
  (define st (model-element-type m type (representation-scalar-element (model-representation m))))
  (system-value (if (string? st) st "String") j (representation-shapes (model-representation m))))

;; with-offset : moment -> moment   a time without an offset takes the evaluation's
(define (with-offset mo)
  (define o (current-evaluation-offset))
  (if (and o (moment-hour mo) (not (moment-tzmin mo)))
      (moment (moment-kind mo) (moment-year mo) (moment-month mo) (moment-day mo) (moment-hour mo) (moment-minute mo)
              (moment-second mo) (moment-frac mo) o (moment-prec mo))
      mo))
(define (system-value st v sh)
  (match st
    ["Boolean" (if (boolean? v) v cql-null)]
    ["Integer" (if (exact-integer? v) v cql-null)]
    ["Decimal" (cond [(number? v) (or (parse-decimal (number->string (if (and (inexact? v) (integer? v)) (inexact->exact v) v))) cql-null)]
                     [(string? v) (or (parse-decimal v) cql-null)]
                     [else cql-null])]
    ["Date" (or (and (string? v) (parse-fp-date v)) cql-null)]
    ["DateTime" (or (and (string? v) (let ([mo (or (parse-fp-datetime v) (parse-fp-date v))]) (and mo (with-offset mo)))) cql-null)]
    ["Time" (or (and (string? v) (parse-fp-time v)) cql-null)]
    ["Code" (if (hash? v) (code-value v) cql-null)]
    ["Concept" (if (hash? v)
                   (let ([cs (hash-ref v (shapes-concept-codes sh) '())])
                     (cql-concept (for/list ([c (in-list (if (list? cs) cs (list cs)))] #:when (hash? c)) (code-value c))
                                  (let ([d (hash-ref v (shapes-concept-display sh) #f)]) (and (string? d) d))))
                   cql-null)]
    ["Quantity" (if (hash? v) (quantity-value v sh) cql-null)]
    [_ (cond [(string? v) v] [(number? v) v] [(boolean? v) v] [else cql-null])]))
;; quantity-value : json Shapes -> Quantity|null   {value, unit}: the unit from the first key present
(define (quantity-value v sh)
  (define n (let ([x (hash-ref v 'value #f)]) (cond [(number? x) (parse-decimal (number->string (if (and (inexact? x) (integer? x)) (inexact->exact x) x)))]
                                                    [(string? x) (parse-decimal x)] [else #f])))
  (define unit (for/or ([k (in-list (shapes-quantity-units sh))]) (let ([u (hash-ref v k #f)]) (and (string? u) u))))
  (if n (make-quantity n (or unit "1")) cql-null))
;; code-value : json -> Code   a System.Code written as an object of its own fields
(define (code-value v)
  (define (s k) (let ([x (hash-ref v k #f)]) (and (string? x) x)))
  (cql-code (s 'code) (s 'system) (s 'version) (s 'display)))

;; node-is? : ModelNode name -> boolean   "{url}Period" / "FHIR.Period" / "Period"
(define (node-is? n qn)
  (define m (model-node-model n))
  (model-subtype? m (model-node-type n) (qualified-type m qn)))

;; ---- codes --------------------------------------------------------------------------------------
;; code-class? : model type -> boolean   converts implicitly to System.Code
(define (code-class? m type) (let ([c (model-conversion m type)]) (and c (equal? (car c) "Code"))))
(define (concept-class? m type) (let ([c (model-conversion m type)]) (and c (equal? (car c) "Concept"))))
;; concept-codings-element : model type -> element|#f   a Concept class's element that lists its codes
(define concept-elements (make-hash))
(define (concept-codings-element m type)
  (hash-ref! concept-elements (cons m type)
             (lambda ()
               (and (concept-class? m type)
                    (let loop ([t type])
                      (define ci (and t (model-class m t)))
                      (and ci
                           (or (for/first ([(e et) (in-hash (class-info-elements ci))]
                                           #:when (and (list-ctype? et) (string? (ctype-element et)) (code-class? m (ctype-element et))))
                                 e)
                               (loop (class-info-base ci)))))))))

;; node-codes : value -> (listof Code)   a Concept class -> its codes; a Code class -> one code; a
;; class written as a bare scalar -> a code with no system; System Codes and Concepts as they are
(define (node-codes v)
  (cond
    [(list? v) (append-map node-codes v)]
    [(cql-code? v) (list v)]
    [(cql-concept? v) (cql-concept-codes v)]
    [(model-node? v)
     (define m (model-node-model v))
     (define t (model-node-type v))
     (define codings (concept-codings-element m t))
     (cond [codings (node-codes (node-property v codings))]
           [(code-class? m t)
            (define (s k) (string-of (node-property v k)))
            (list (cql-code (s "code") (s "system") (s "version") (s "display")))]
           [(string? (model-node-data v)) (list (cql-code (model-node-data v) #f #f #f))]
           [else '()])]
    [(string? v) (list (cql-code v #f #f #f))]
    [else '()]))
;; string-of : value -> string|#f   a String, or a class written as a scalar whose scalar is one
(define (string-of x)
  (cond [(string? x) x]
        [(model-node? x)
         (define se (representation-scalar-element (model-representation (model-node-model x))))
         (define y (and se (node-property x se)))
         (and (string? y) y)]
        [else #f]))

;; ---- references ---------------------------------------------------------------------------------
;; reference-key : rep json -> (cons type|#f key) | #f   the record a value names
(define (reference-key r v)
  (match (representation-reference r)
    ['typed
     (define ref (cond [(string? v) v] [(hash? v) (let ([x (hash-ref v 'reference #f)]) (and (string? x) x))] [else #f]))
     (and ref (match (regexp-match #px"(?:^|/)([A-Za-z][A-Za-z0-9]*)/([^/]+)$" ref)
                [(list _ type id) (cons type id)]
                [_ (cons #f ref)]))]
    [_ (cond [(string? v) (cons #f v)]
             [(and (hash? v) (string? (hash-ref v 'id #f))) (cons #f (hash-ref v 'id))]
             [(exact-integer? v) (cons #f (number->string v))]
             [else #f])]))

(module+ test
  (require rackunit (only-in fhirpath/temporal-ops moment?))
  ;; a model designed here and now: a Person and a Visit, data in its own JSON shape
  (define clinic
    (form->model
     '(model "Clinic" "http://example.org/clinic" "1.0" (required) (patient "Clinic.Person" "born")
             (types (type "Clinic.Person" #f #f "Person" #f #t (("id" "System.String" #f) ("born" "System.Date" #f)) () ())
                    (type "Clinic.Visit" #f #f "Visit" "reason" #t
                          (("reason" "System.Code" #f) ("when" (choice "System.DateTime" (interval "System.DateTime")) #f)
                           ("tags" (list "System.String") #f) ("person" "System.String" #f) ("note" "System.String" "%parent.text"))
                          (("Patient" "person")) ()))
             (conversions) (contexts ("Patient" "id" "born" "Clinic.Person")))))
  (check-eq? (model-representation clinic) default-representation)
  (define visit (record->node clinic (hasheq 'reason (hasheq 'code "A1" 'system "http://x") 'when (hasheq 'low "2025-01-01T09:00:00Z" 'high "2025-01-01T10:00:00Z")
                                             'tags '("a" "b") 'person "p1")
                              "Clinic.Visit"))
  (check-equal? (model-node-type visit) "Clinic.Visit")
  (check-equal? (node-codes (node-property visit "reason")) (list (cql-code "A1" "http://x" #f #f)))
  (check-true (cql-interval? (node-property visit "when")))                 ; a plain choice: the interval member
  (check-true (moment? (node-property (record->node clinic (hasheq 'when "2025-01-01") "Clinic.Visit") "when")))
  (check-equal? (node-property visit "tags") '("a" "b"))
  (check-equal? (node-property visit "nothing") cql-null)
  (check-exn #rx"maps to `%parent.text`" (lambda () (node-property visit "note")))
  (check-equal? (reference-key default-representation "p1") (cons #f "p1"))
  (check-equal? (reference-key (representation #f 'plain #f 'typed cql-shapes (hash)) (hasheq 'reference "http://s/fhir/Patient/p1")) (cons "Patient" "p1"))
  (check-true (node-is? visit "Visit"))
  (check-true (node-is? visit "{http://example.org/clinic}Visit"))
  ;; FHIR, through the same machinery: its representation is the declaration beside its model
  (define fhir (find-model "FHIR" "4.0.1"))
  (check-equal? (model-representation fhir) (representation 'resourceType 'suffixed "value" 'typed cql-shapes (hash)))
  (define obs (record->node fhir (hasheq 'resourceType "Observation" 'status "final"
                                         'code (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "1-1")))
                                         'valueQuantity (hasheq 'value 7.5 'unit "%")
                                         'effectiveDateTime "2021-03-04T10:00:00-07:00")))
  (check-equal? (model-node-type (node-property obs "status")) "FHIR.ObservationStatus")
  (check-equal? (node-property (node-property obs "status") "value") "final")
  (check-equal? (model-node-type (node-property obs "value")) "FHIR.Quantity")         ; choice by key
  (check-true (moment? (node-property (node-property obs "effective") "value")))
  (check-equal? (node-property obs "category") '())                                  ; absent list
  (check-true (node-is? (node-property obs "value") "{http://hl7.org/fhir}Quantity"))
  (check-false (node-is? (node-property obs "value") "{http://hl7.org/fhir}Period"))
  (check-equal? (node-codes (node-property obs "code")) (list (cql-code "1-1" "http://loinc.org" #f #f)))
  (check-equal? (concept-codings-element fhir "FHIR.CodeableConcept") "coding")
  (check-true (code-class? fhir "FHIR.Coding")))
