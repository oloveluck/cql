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

(require racket/contract racket/match racket/runtime-path racket/list racket/string (for-syntax racket/base))

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

;; ---- models -------------------------------------------------------------------------------
;;; A Model is (model name url version required patient-class birth-date-path classes conversions contexts
;;;                   representation-settings)
;;;   required  : (listof (cons name version))   the models it builds on (QI-Core on FHIR); a class
;;;               it does not have is looked up in them, so its classes may extend theirs
;;;   classes   : hash qualified-name -> ClassInfo
;;;   conversions : hash from-type -> (cons to-CType function-name)   the implicit conversions to System
;;;   contexts  : hash name -> ContextInfo
;;;   representation-settings : (listof (cons key value)) | #f   how its data looks (representation.rkt), from
;;;               a `<model file>.representation.rktd` beside the model file; #f: inherited or default
;;; A ClassInfo is (class-info name base identifier label primary-code-path retrievable? elements
;;;                            relationships targets)
;;;   elements      : hash element-name -> CType
;;;   relationships : (listof (cons context-name related-key-element))   how an instance relates to a
;;;                   context (the Patient compartment of FHIR: Observation by subject and performer);
;;;                   the key element is an element, or names one of the class's searches
;;;   searches      : hash search-name -> path   (FHIR's: `patient` -> `subject.where(resolve() is Patient)`)
;;;   targets       : hash element-name -> mapping expression   where the element's value really is
;;; A ContextInfo is (context-info name key-element birth-date-element type)
;;; Models come from a registry: the model files shipped here (`shipped-model-files`: FHIR 4.0.1's),
;;; read when a model is first asked for; any registered by a program
;;; (`register-model!`), any a provider makes on request (`current-model-providers`: a workspace makes
;;; one from logical models), and any ModelInfo file in `current-model-directories` (and the
;;; directories named by CQL_MODEL_PATH): `<anything>.xml`, `.xml.gz` or a ModelForm `.rktd`, each
;;; with its representation, when it has one, in `<the same name>.representation.rktd`.

(provide
 (struct-out model) (struct-out class-info) (struct-out context-info)
 (contract-out
  [find-model          (-> string? (or/c string? #f) (or/c model? #f))]
  [find-model-by-url   (-> string? (or/c model? #f))]
  [register-model!     (->* (model?) (#:versions (listof string?)) void?)]
  [installed-models    (-> (listof model?))]
  [current-model-providers   (parameter/c (listof procedure?))]
  [current-model-directories (parameter/c (listof path-string?))]
  [form->model         (->* (list?) ((or/c list? #f)) model?)]
  [load-model          (->* (path-string?) (#:representation (or/c path-string? #f)) model?)]
  [model-required-models (-> model? (listof model?))]
  [model-class         (-> model? string? (or/c class-info? #f))]
  [model-element-type  (-> model? string? string? (or/c any/c #f))]
  [model-element-target (-> model? string? string? (or/c string? #f))]
  [model-subtype?      (-> model? string? string? boolean?)]
  [model-conversion    (-> model? string? (or/c (cons/c any/c string?) #f))]
  [model-context-type  (-> model? string? (or/c string? #f))]
  [model-context       (-> model? string? (or/c context-info? #f))]
  [model-relationships (-> model? string? string? (listof string?))]
  [model-resolve-type  (-> model? string? (or/c string? #f))]))

(struct class-info (name base identifier label primary-code-path retrievable? elements relationships targets searches) #:transparent)
(struct context-info (name key-element birth-date-element type) #:transparent)
;; A model is equal only to itself: models are shared (the registry hands out one per name and
;; version), so identity is equality, and hashing or printing one never walks its classes.
(struct model (name url version required patient-class birth-date-path classes conversions contexts representation-settings)
  #:property prop:equal+hash
  (list (lambda (a b rec) (eq? a b)) (lambda (a rec) (eq-hash-code a)) (lambda (a rec) (eq-hash-code a)))
  #:property prop:custom-write
  (lambda (m port mode) (fprintf port "#<model ~a ~a>" (model-name m) (model-version m))))

;; the models shipped with the engine: ModelForm files in this directory, each with its representation
;; — a list of files, so the engine's code names no model. Both files of a pair are runtime paths, so
;; a standalone build (raco distribute) carries the representation with the model.
(define-runtime-path-list shipped-model-files (list "fhir-4.0.1.rktd" "fhir-4.0.1.representation.rktd"))

;; spec->ctype : TypeSpec -> CType
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
        [(regexp-match #rx"^List<(.*)>$" s) => (lambda (m) (list-ctype (system-simple (cadr m))))]
        [(regexp-match #rx"^System[.](.*)$" s) => cadr]
        [else s]))

;; form->model : ModelForm (modelinfo-xml.rkt) [representation settings] -> Model
(define (form->model f [representation #f])
  (match f
    [(list 'model name url version (list 'required reqs ...) (list 'patient patient birth)
           (list 'types types ...) (list 'conversions convs ...) (list 'contexts ctxs ...))
     (model name url version (for/list ([r (in-list reqs)]) (cons (car r) (cadr r))) patient birth
            (for/hash ([t (in-list types)])
              (match t
                [(list 'type n base id label pcp retr elts rels searches)
                 (values n (class-info n base id label pcp retr
                                       (for/hash ([e (in-list elts)]) (values (car e) (spec->ctype (cadr e))))
                                       (for/list ([r (in-list rels)]) (cons (car r) (cadr r)))
                                       (for/hash ([e (in-list elts)] #:when (caddr e)) (values (car e) (caddr e)))
                                       (for/hash ([x (in-list searches)]) (values (car x) (cadr x)))))]))
            (for/hash ([c (in-list convs)]) (values (car c) (cons (system-simple (cadr c)) (caddr c))))
            (for/hash ([c (in-list ctxs)]) (values (car c) (apply context-info c)))
            representation)]
    [_ (error 'form->model "not a ModelForm: ~a" (if (pair? f) (car f) f))]))
;; load-model : path [#:representation path] -> Model   a ModelForm (.rktd) or ModelInfo (.xml,
;; .xml.gz) file, with its representation: the file given, else `<same name>.representation.rktd`
(define (load-model path #:representation [rep-file #f])
  (define p (if (path? path) (path->string path) path))
  (define form (if (regexp-match? #rx"[.]rktd$" p)
                   (call-with-input-file p read)
                   ((dynamic-require 'cql/model/modelinfo-xml 'read-modelinfo-xml) p)))
  (define sidecar (or rep-file (string-append (regexp-replace #rx"[.](rktd|xml|xml[.]gz)$" p "") ".representation.rktd")))
  (form->model form (and (file-exists? sidecar) (call-with-input-file sidecar read))))

;; ---- the registry ---------------------------------------------------------------------------
;; (cons name version) -> model | promise ; models by url
(define registry (make-hash))
(define (register-model! m #:versions [aliases '()])
  (for ([v (in-list (cons (model-version m) aliases))]) (hash-set! registry (cons (model-name m) v) m)))
;; the shipped models, registered the first time the registry is asked
(define shipped-loaded? #f)
(define (load-shipped!)
  (unless shipped-loaded?
    (set! shipped-loaded? #t)
    ;; a model file, then (when the next file is one) its representation
    (let loop ([fs shipped-model-files])
      (match fs
        [(list* f r more) #:when (regexp-match? #rx"[.]representation[.]rktd$" (path->string r))
         (register-model! (load-model f #:representation r)) (loop more)]
        [(cons f more) (register-model! (load-model f)) (loop more)]
        ['() (void)]))))

(define current-model-providers (make-parameter '()))
(define current-model-directories
  (make-parameter (let ([e (getenv "CQL_MODEL_PATH")]) (if e (filter (lambda (s) (not (string=? s ""))) (regexp-split #rx":" e)) '()))))

(define (entry-model e) e)
;; version-key : version-string -> (listof integer|string)   "4.0.10" sorts after "4.0.9"
(define (version-key v) (map (lambda (p) (or (string->number p) p)) (regexp-split #rx"[.]" (or v ""))))
(define (version<? a b)
  (let loop ([a (version-key a)] [b (version-key b)])
    (cond [(null? a) (pair? b)] [(null? b) #f]
          [(and (number? (car a)) (number? (car b)) (not (= (car a) (car b)))) (< (car a) (car b))]
          [(equal? (car a) (car b)) (loop (cdr a) (cdr b))]
          [else (string<? (format "~a" (car a)) (format "~a" (car b)))])))

;; directory-models : -> (listof model)   every model file in the model directories, read once per file
(define file-models (make-hash))
(define (directory-models)
  (for*/list ([d (in-list (current-model-directories))] #:when (directory-exists? d)
              [f (in-list (directory-list d #:build? #t))]
              #:when (regexp-match? #rx"[.](xml|xml[.]gz|rktd)$" (path->string f))
              #:unless (regexp-match? #rx"[.]representation[.]rktd$" (path->string f))
              [m (in-value (hash-ref! file-models (path->string (simplify-path (path->complete-path f)))
                                      (lambda () (with-handlers ([exn:fail? (lambda (_) #f)]) (load-model f)))))]
              #:when m)
    m))

;; find-model : name version|#f -> model|#f   registered (version #f: the newest), then a provider,
;; then the model directories
(define (find-model name version)
  (load-shipped!)
  (define (newest ms) (and (pair? ms) (argmax* ms)))
  (define (argmax* ms) (for/fold ([best (car ms)]) ([m (in-list (cdr ms))]) (if (version<? (model-version best) (model-version m)) m best)))
  (or (if version
          (let ([e (hash-ref registry (cons name version) #f)]) (and e (entry-model e)))
          (newest (for/list ([(k e) (in-hash registry)] #:when (equal? (car k) name)) (entry-model e))))
      (for/or ([p (in-list (current-model-providers))]) (p name version))
      (newest (filter (lambda (m) (and (equal? (model-name m) name) (or (not version) (equal? (model-version m) version))))
                      (directory-models)))))

(define (installed-models)
  (load-shipped!)
  (remove-duplicates (append (for/list ([e (in-hash-values registry)]) (entry-model e)) (directory-models)) eq?))

;; find-model-by-url : url -> model|#f   the model a `{url}Type` name belongs to
(define by-url (make-hash))
(define (find-model-by-url url)
  (or (hash-ref by-url url #f)
      (let ([m (for/or ([m (in-list (installed-models))]) (and (equal? (model-url m) url) m))])
        (when m (hash-set! by-url url m))
        m)))

;; model-required-models : model -> (listof model)   the models it builds on, found in the registry
(define (model-required-models m)
  (for*/list ([r (in-list (model-required m))] [x (in-value (find-model (car r) (cdr r)))] #:when x) x))

;; ---- questions about a model ----------------------------------------------------------------
;; model-class : model qualified-name -> class-info|#f   its own classes, then those of the models it builds on
(define (model-class m name)
  (or (hash-ref (model-classes m) name #f)
      (for/or ([r (in-list (model-required-models m))]) (model-class r name))))

;; model-element-type : model type-name element-name -> CType|#f   searching the base chain
(define (model-element-type m type elt)
  (let loop ([t type])
    (define ci (and t (model-class m t)))
    (cond [(not ci) #f]
          [(hash-ref (class-info-elements ci) elt #f)]
          [else (loop (class-info-base ci))])))
;; model-element-target : model type-name element-name -> mapping expression|#f   (#f at once for a
;; model none of whose classes, nor those of the models it builds on, maps an element: FHIR's)
(define has-targets (make-weak-hasheq))
(define (model-has-targets? m)
  (hash-ref! has-targets m (lambda () (or (for/or ([ci (in-hash-values (model-classes m))]) (positive? (hash-count (class-info-targets ci))))
                                          (ormap model-has-targets? (model-required-models m))))))
(define (model-element-target m type elt)
  (and (model-has-targets? m) (model-element-target* m type elt)))
(define (model-element-target* m type elt)
  (let loop ([t type])
    (define ci (and t (model-class m t)))
    (cond [(not ci) #f]
          [(hash-has-key? (class-info-elements ci) elt) (hash-ref (class-info-targets ci) elt #f)]
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
          [(model-own-conversion m t)]
          [else (define ci (model-class m t)) (and ci (loop (class-info-base ci)))])))
(define (model-own-conversion m t)
  (or (hash-ref (model-conversions m) t #f)
      (for/or ([r (in-list (model-required-models m))]) (model-own-conversion r t))))

(define (model-context m name)
  (or (hash-ref (model-contexts m) name #f)
      (for/or ([r (in-list (model-required-models m))]) (model-context r name))))
(define (model-context-type m name) (let ([c (model-context m name)]) (and c (context-info-type c))))

;; model-relationships : model type-name context -> (listof dotted-path)   the paths by which an
;; instance of the type relates to the context (its compartment), up the base chain. A relationship
;; names an element (`subject`), or a search whose path gives the elements (`patient` ->
;; `subject.where(resolve() is Patient)` -> `subject`; `agent.who|entity.what` -> both); a
;; `where(…)` step only narrows by the referenced type, which matching the reference decides anyway.
(define (model-relationships m type ctx)
  (let loop ([t type])
    (define ci (and t (model-class m t)))
    (cond [(not ci) '()]
          [else (define here (for/list ([r (in-list (class-info-relationships ci))] #:when (equal? (car r) ctx)) (cdr r)))
                (if (pair? here)
                    (remove-duplicates (append-map (lambda (k) (relationship-paths m t k)) here))
                    (loop (class-info-base ci)))])))
(define (relationship-paths m type key)
  (cond [(model-element-type m type key) (list key)]
        [(search-path m type key) => search-path->paths]
        [else (or (for/or ([p (in-list (all-search-paths m type))])
                    (define ps (search-path->paths p))
                    (define hit (filter (lambda (x) (equal? (last (string-split x ".")) key)) ps))
                    (and (pair? hit) hit))
                  (list key))]))
(define (search-path m type name)
  (let loop ([t type])
    (define ci (and t (model-class m t)))
    (and ci (or (hash-ref (class-info-searches ci) name #f) (loop (class-info-base ci))))))
(define (all-search-paths m type)
  (let loop ([t type])
    (define ci (and t (model-class m t)))
    (if ci (append (hash-values (class-info-searches ci)) (loop (class-info-base ci))) '())))
;; search-path->paths : FHIRPath-ish path -> (listof dotted-path)   the element steps, without filters
(define (search-path->paths p)
  (for*/list ([alt (in-list (string-split p "|"))]
              [clean (in-value (string-join (filter (lambda (step) (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" step))
                                                    (string-split (strip-calls (string-trim alt)) "."))
                                            "."))]
              #:unless (string=? clean ""))
    clean))
;; strip-calls : path -> path   every `.name(…)` step removed, its parentheses balanced
(define (strip-calls s)
  (let loop ([cs (string->list s)] [depth 0] [out '()])
    (cond [(null? cs) (regexp-replace* #px"[.]?[A-Za-z_]+\\(\\)" (list->string (reverse out)) "")]
          [(char=? (car cs) #\() (loop (cdr cs) (add1 depth) (if (= depth 0) (cons #\( out) out))]
          [(char=? (car cs) #\)) (loop (cdr cs) (sub1 depth) (if (= depth 1) (cons #\) out) out))]
          [(> depth 0) (loop (cdr cs) depth out)]
          [else (loop (cdr cs) depth (cons (car cs) out))])))

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
  (check-equal? (model-context-type m "Patient") "FHIR.Patient")
  (check-equal? (context-info-birth-date-element (model-context m "Patient")) "birthDate.value")
  ;; the Patient compartment, from the ModelInfo
  (check-equal? (model-relationships m "FHIR.Observation" "Patient") '("subject" "performer"))
  (check-equal? (model-relationships m "FHIR.Encounter" "Patient") '("subject"))            ; by the search `patient`
  (check-equal? (model-relationships m "FHIR.Coverage" "Patient") '("policyHolder" "subscriber" "beneficiary" "payor"))
  (check-equal? (model-relationships m "FHIR.Patient" "Patient") '("link.other"))            ; `other`, through search `link`
  (check-equal? (search-path->paths "agent.who.where(resolve() is Patient)|entity.what.where(resolve() is Patient)") '("agent.who" "entity.what"))
  (check-equal? (find-model "FHIR" #f) m)
  (check-equal? (find-model-by-url "http://hl7.org/fhir") m)
  (check-false (find-model "Nope" "1"))
  ;; a model built on another: its own classes extend FHIR's, which it finds through `required`
  (define ext (form->model '(model "Ext" "http://example.org/ext" "1.0" (required ("FHIR" "4.0.1")) (patient "FHIR.Patient" "birthDate.value")
                                   (types (type "Ext.Visit" "FHIR.Encounter" #f "Visit" "type" #t (("note" "System.String" "%parent.text")) () ()))
                                   (conversions) (contexts))))
  (check-equal? (model-representation-settings m) '((type-key . resourceType) (choice . suffixed) (scalar-element . "value") (reference . typed)))
  (check-equal? (model-element-type ext "Ext.Visit" "status") "FHIR.EncounterStatus")     ; inherited from FHIR
  (check-equal? (model-element-target ext "Ext.Visit" "note") "%parent.text")
  (check-true (model-subtype? ext "Ext.Visit" "FHIR.Resource"))
  (check-equal? (model-relationships ext "Ext.Visit" "Patient") '("subject"))              ; FHIR.Encounter's
  (check-equal? (model-context-type ext "Patient") "FHIR.Patient")
  (check-equal? (model-conversion ext "FHIR.EncounterStatus") (cons "String" "FHIRHelpers.ToString"))
  ;; a provider makes a model on request
  (parameterize ([current-model-providers (list (lambda (n v) (and (equal? n "Ext") ext)))])
    (check-eq? (find-model "Ext" "1.0") ext))
  (check-false (find-model "Ext" "1.0")))
