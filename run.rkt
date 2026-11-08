#lang racket/base

;;; run.rkt — run CQL over a data model's data: libraries with their includes, a data source,
;;; terminology, and measure populations.
;;;
;;;   (define rl (load-runtime-library ast #:resolver r #:data (resources->data rs)
;;;                                    #:valuesets (valuesets->terminology vss)))
;;;   (rlib-value rl "Initial Population")            ; one definition
;;;   (measure-populations measure-json rl)           ; proportion scoring for one patient (first group)
;;;   (measure-group-populations measure-json rl)     ; … for every Measure.group
;;;
;;; Data: a Resources is (listof jsexpr) — records that name their class under the model's type key
;;; (FHIR resources, Bundles of them) — or (hash class-name -> (listof jsexpr)) for a model whose
;;; records do not; all for one context patient. The retrieve source answers a class's records for a
;;; model, scoped to the patient by the model's own relationships (its ModelInfo's Patient
;;; compartment). Terminology: value set url -> its expansion (system, code) pairs.
;;; Parameters (e.g. "Measurement Period") apply to every library in the evaluation, the way a
;;; measure evaluator passes them; a library's own default is used otherwise.

(require racket/contract racket/match racket/list racket/string racket/file json
         "parser/ast.rkt"
         (only-in "parser/parse.rkt" parse-cql)
         (only-in "compile.rkt" compile-library current-library-resolver)
         (only-in "interp.rkt" make-rlib rlib-value runtime rlib? eval-elm make-ienv)
         (only-in "elm/ast.rkt" elm-library-parameters elm-parameter-def-name elm-parameter-def-default)
         (only-in "value.rkt" current-evaluation-offset)
         (only-in "error.rkt" cql-error? cql-error-string)
         (only-in "model/modelinfo.rkt" model? model-name model-context context-info-type context-info-key-element model-relationships)
         (only-in "representation.rkt" model-representation record-type-name record->node node-property reference-key)
         (only-in "value.rkt" model-node? model-node-data)
         (only-in fhir/data-source bundle-resources)
         (only-in fhir/result/result ok? ok-value err-error))

(provide
 (contract-out
  ;; caching-resolver : resolver -> resolver   the same answers, each (name, version) resolved once.
  ;; For one evaluation over many patients, where the libraries do not change: a resolver typically
  ;; reads a file or a data source and parses it, which otherwise happens for every include of every
  ;; patient. A resolution that raises is not stored.
  [caching-resolver (-> procedure? procedure?)]
  [load-runtime-library (->* (cql-library?)
                             (#:resolver procedure? #:data procedure? #:valuesets procedure? #:parameters hash?
                              #:offset (or/c exact-integer? #f))
                             rlib?)]
  ;; resources->data : Resources -> (class-name model -> (listof jsexpr))   the retrieve source: a
  ;; class's records, scoped to the context patient by that model
  [resources->data      (->* ((or/c (listof hash?) hash?)) (#:patient-context? boolean?) (-> string? model? (listof hash?)))]
  ;; patient-data : (class-name -> records) patient-key -> (class-name model -> records)   every record
  ;; of a class, scoped to one patient by the model's relationships (the records read once per class)
  [patient-data         (-> procedure? string? (-> string? model? (listof hash?)))]
  ;; patient-class : model -> (values class-name key-element) | (values #f #f)   the Patient context's
  ;; class (its unqualified name, as data names it) and the element that keys it
  [patient-class        (-> model? (values (or/c string? #f) (or/c string? #f)))]
  [valuesets->terminology (-> (listof hash?) procedure?)]
  [measure-populations  (-> hash? rlib? (hash/c string? exact-nonnegative-integer?))]
  [measure-group-populations (-> hash? rlib? (listof (hash/c string? exact-nonnegative-integer?)))]
  [read-json-files      (-> path-string? (listof hash?))]
  ;; evaluate-library : the library over these resources -> define-name -> value (every define)
  [evaluate-library     (->* (cql-library? (listof hash?))
                             (#:parameters hash? #:valuesets (listof hash?) #:offset (or/c exact-integer? #f))
                             hash?)])
 rlib-value)

;; load-runtime-library : cql-library … -> rlib   compiles the library and (recursively, once each)
;; the libraries it includes, binding include aliases to their runtime libraries
(define (load-runtime-library lib #:resolver [resolver (lambda (n v) #f)] #:data [data (lambda (t) '())]
                              #:valuesets [valuesets (lambda (u) #f)] #:parameters [params (hash)]
                              #:offset [offset #f])
  ;; offset: the evaluation request's timezone offset in minutes (DateTimes written without one take it)
  (define rt (runtime data valuesets offset))
  (define memo (make-hash))
  (define (compile1 ast)
    (hash-ref! compiled-libraries ast (lambda () (compile1* ast))))
  (define (compile1* ast)
    (parameterize ([current-library-resolver resolver])
      (let ([r (compile-library ast)])
        (if (ok? r) (ok-value r)
            (error 'cql-run "~a: ~a" (cql-library-id ast)
                   (let ([e (err-error r)]) (if (cql-error? e) (cql-error-string e) e)))))))
  ;; the main library's parameter defaults apply to the libraries it includes, the way a measure
  ;; evaluator passes the evaluation's parameters to every library (an include often declares
  ;; "Measurement Period" without a default)
  (define main-defaults
    (parameterize ([current-evaluation-offset offset])
      (for/hash ([p (in-list (elm-library-parameters (compile1 lib)))]
                 #:when (elm-parameter-def-default p)
                 #:unless (hash-has-key? params (elm-parameter-def-name p)))
        (values (elm-parameter-def-name p) (eval-elm (elm-parameter-def-default p) (make-ienv))))))
  (define all-params (for/fold ([h params]) ([(k v) (in-hash main-defaults)]) (hash-set h k v)))
  (define (build ast)
    (define key (cons (cql-library-id ast) (cql-library-version ast)))
    (hash-ref! memo key
               (lambda ()
                 (define elm (compile1 ast))
                 (define includes
                   (for/hash ([d (in-list (cql-library-defs ast))] #:when (cql-include? d))
                     (define src (or (resolver (cql-include-lib d) (cql-include-version d))
                                     (error 'cql-run "library ~a not found" (cql-include-lib d))))
                     (values (symbol->string (or (cql-include-alias d) (cql-include-lib d))) (build src))))
                 (make-rlib elm includes rt all-params))))
  (build lib))

;; caching-resolver : (name version -> cql-library|#f) -> (name version -> cql-library|#f)
(define (caching-resolver resolver)
  (define memo (make-hash))
  (lambda (name version) (hash-ref! memo (cons name version) (lambda () (resolver name version)))))

;; compiled ELM by library AST (parse-cql memoises ASTs by source text, so the same CQL is the same
;; object): evaluating a measure for many patients compiles each library once
(define compiled-libraries (make-weak-hasheq))

;; resources->data : Resources [#:patient-context?] -> (class-name model -> (listof jsexpr))
;; Patient context: when the data holds exactly one record of the model's Patient context class, a
;; record that references a patient through any of its relationships to that context (the model's
;; Patient compartment: an Observation's subject or performer, a Coverage's beneficiary, subscriber,
;; policyHolder or payor) is that patient's only when one of them is this patient; a record that
;; references no patient is everyone's.
(define (resources->data rs #:patient-context? [ctx? #t])
  (define flat (if (hash? rs) #f (append-map bundle-resources rs)))
  (define memo (make-hash))
  (define (records-of m type)
    (cond [(hash? rs) (hash-ref rs type (hash-ref rs (string->symbol type) '()))]
          [else (define want (string-append (model-name m) "." type))
                (filter (lambda (r) (equal? (record-type-name m r) want)) flat)]))
  (lambda (type m)
    (hash-ref! memo (cons type m)
               (lambda ()
                 (define rs* (records-of m type))
                 (if ctx? (filter (in-patient-context m (single-patient m (lambda (t) (records-of m t)))) rs*) rs*)))))

;; single-patient : model (class-name -> records) -> key|#f   the patient's key when the data holds one
(define (single-patient m records-of)
  (define-values (pclass key) (patient-class m))
  (define patients (if pclass (records-of pclass) '()))
  (and (= 1 (length patients)) (key-string (hash-ref (car patients) (string->symbol key) #f))))

(define (patient-class m)
  (define ctx (model-context m "Patient"))
  (if ctx (values (local-name (context-info-type ctx)) (context-info-key-element ctx)) (values #f #f)))

(define (patient-data all-of pid)
  (define memo (make-hash))
  (lambda (type m)
    (hash-ref! memo (cons type m)
               (lambda ()
                 (define-values (pclass key) (patient-class m))
                 (define rs (all-of type))
                 (if (equal? type pclass)
                     (filter (lambda (r) (equal? (key-string (hash-ref r (string->symbol key) #f)) pid)) rs)
                     (filter (in-patient-context m pid) rs))))))

;; in-patient-context : model key|#f -> (record -> boolean)
(define (in-patient-context m pid)
  (define ctx (model-context m "Patient"))
  (define ctype (and ctx (context-info-type ctx)))
  (define rep (model-representation m))
  (cond
    [(not pid) (lambda (r) #t)]
    [else
     (lambda (r)
       (define type (record-type-name m r))
       (define paths (if type (model-relationships m type "Patient") '()))
       (define keys (for*/list ([p (in-list paths)]
                                [v (in-list (values-at (record->node m r) (string-split p ".")))]
                                [k (in-value (reference-key rep (if (model-node? v) (model-node-data v) v)))] #:when k)
                      k))
       (or (equal? type ctype)
           (null? keys)
           (for/or ([k (in-list keys)]) (and (equal? (cdr k) pid) (or (not (car k)) (equal? (car k) (local-name ctype)))))))]))
(define (local-name t) (let ([x (regexp-match #rx"[.]([^.]+)$" t)]) (if x (cadr x) t)))
(define (key-string v) (cond [(string? v) v] [(exact-integer? v) (number->string v)] [else #f]))
;; values-at : node steps -> (listof value)   the element values along a path, lists fanned out
(define (values-at v steps)
  (cond [(null? steps) (if (list? v) v (list v))]
        [(list? v) (append-map (lambda (x) (values-at x steps)) v)]
        [(model-node? v) (with-handlers ([exn:fail? (lambda (_) '())]) (values-at (node-property v (car steps)) (cdr steps)))]
        [else '()]))

;; valuesets->terminology : (listof ValueSet jsexpr) -> (url -> (hash (cons system code) -> #t) | #f)
;; from each value set's expansion (else its enumerated compose concepts)
(define (valuesets->terminology vss)
  (define idx
    (for/hash ([v (in-list vss)] #:when (hash-ref v 'url #f))
      (define from-expansion
        (for/list ([c (in-list (hash-ref (hash-ref v 'expansion (hash)) 'contains '()))])
          (cons (hash-ref c 'system #f) (hash-ref c 'code #f))))
      (define from-compose
        (for*/list ([inc (in-list (hash-ref (hash-ref v 'compose (hash)) 'include '()))]
                    [c (in-list (hash-ref inc 'concept '()))])
          (cons (hash-ref inc 'system #f) (hash-ref c 'code #f))))
      (values (hash-ref v 'url) (for/hash ([p (in-list (if (pair? from-expansion) from-expansion from-compose))]) (values p #t)))))
  ;; by url (a |version is ignored), memoised: membership asks for every value set of every patient
  (define by-url (make-hash))
  (lambda (url) (hash-ref! by-url url (lambda () (hash-ref idx (car (string-split url "|")) #f)))))

;; read-json-files : dir -> (listof jsexpr)   every *.json under dir (MeasureReports excluded)
(define (read-json-files dir)
  (for/list ([f (in-directory dir)]
             #:when (regexp-match? #rx"[.]json$" (path->string f))
             #:unless (regexp-match? #rx"(?i:measurereport)" (path->string f))
             [j (in-value (call-with-input-file f read-json))]
             #:when (hash? j))
    j))

;; measure-group-populations : Measure jsexpr rlib -> (listof (hash code -> 0|1))   one patient, one
;; hash per Measure.group (a measure may have several: BCS-E's general and Medicare groups share
;; population codes with different criteria)
(define (measure-group-populations measure rl)
  (for/list ([g (in-list (hash-ref measure 'group '()))]) (group-populations g rl)))

;; measure-populations : Measure jsexpr rlib -> (hash code -> 0|1)   the first group's populations
(define (measure-populations measure rl)
  (match (hash-ref measure 'group '())
    [(cons g _) (group-populations g rl)]
    ['() (hash)]))

;; group-populations : Measure.group jsexpr rlib -> (hash code -> 0|1)   one patient, proportion
;; scoring (patient-based): IP; denominator ⊆ IP; exclusion ⊆ denominator; numerator ⊆ denominator
;; minus exclusions; exception ⊆ denominator minus exclusions and numerator
(define (group-populations g rl)
  (define crit
    (for/hash ([p (in-list (hash-ref g 'population '()))])
      (values (hash-ref (car (hash-ref (hash-ref p 'code) 'coding)) 'code)
              (hash-ref (hash-ref p 'criteria) 'expression))))
  (define (holds? code) (let ([e (hash-ref crit code #f)]) (and e (truthy? (rlib-value rl e)))))
  (define ip (holds? "initial-population"))
  (define den (and ip (if (hash-has-key? crit "denominator") (holds? "denominator") #t)))
  (define excl (and den (holds? "denominator-exclusion")))
  (define num (and den (not excl) (holds? "numerator")))
  (define exc (and den (not excl) (not num) (holds? "denominator-exception")))
  (for/hash ([(code _e) (in-hash crit)])
    (values code (if (case code
                       [("initial-population") ip] [("denominator") den] [("denominator-exclusion") excl]
                       [("numerator") num] [("denominator-exception") exc] [else #f])
                     1 0))))
;; a population definition is boolean (patient-based) or a list (non-empty = in)
(define (truthy? v) (or (eq? v #t) (and (pair? v) #t)))

;; evaluate-library : library resources [#:parameters #:valuesets #:offset] -> (hash name -> value)
;; the library run over the given FHIR resources (in a Patient context: the Patient among them and
;; their data), each `define` evaluated — what #lang cql's `evaluate` is
(define (evaluate-library library resources #:parameters [params (hash)] #:valuesets [vss '()] #:offset [offset #f])
  (define rl (load-runtime-library library #:data (resources->data resources) #:parameters params
                                   #:valuesets (valuesets->terminology vss) #:offset offset))
  (for/hash ([d (in-list (cql-library-defs library))] #:when (cql-def? d))
    (define name (format "~a" (cql-def-name d)))
    (values name (rlib-value rl name))))

;; ---- living examples ------------------------------------------------------------------------
(module+ test
  (require rackunit)
  ;; data: a Bundle flattens; in a Patient context another patient's resources are not retrieved
  (define p1 (hasheq 'resourceType "Patient" 'id "p1"))
  (define mine (hasheq 'resourceType "Observation" 'id "o1" 'subject (hasheq 'reference "Patient/p1")))
  (define theirs (hasheq 'resourceType "Observation" 'id "o2" 'subject (hasheq 'reference "Patient/p2")))
  (define nobodys (hasheq 'resourceType "Observation" 'id "o3"))
  (define bundle (hasheq 'resourceType "Bundle" 'entry (list (hasheq 'resource theirs) (hasheq 'resource nobodys))))
  (define fhir ((dynamic-require 'cql/model/modelinfo 'find-model) "FHIR" "4.0.1"))
  (define data (resources->data (list p1 mine bundle)))
  (check-equal? (map (lambda (r) (hash-ref r 'id)) (data "Observation" fhir)) '("o1" "o3"))
  (check-equal? (map (lambda (r) (hash-ref r 'id)) (data "Patient" fhir)) '("p1"))
  (check-equal? (data "Condition" fhir) '())
  (check-equal? (map (lambda (r) (hash-ref r 'id)) ((resources->data (list p1 mine theirs) #:patient-context? #f) "Observation" fhir))
                '("o1" "o2"))
  ;; an absolute reference to the patient counts as the patient's
  (check-equal? (length ((resources->data (list p1 (hash-set mine 'subject (hasheq 'reference "http://s/fhir/Patient/p1")))) "Observation" fhir)) 1)
  ;; the compartment comes from the model: a Coverage is the patient's through any of its relationships
  (define cov (hasheq 'resourceType "Coverage" 'id "c1" 'beneficiary (hasheq 'reference "Patient/p2") 'subscriber (hasheq 'reference "Patient/p1")))
  (check-equal? (length ((resources->data (list p1 cov)) "Coverage" fhir)) 1)
  (check-equal? (length ((resources->data (list p1 (hash-remove cov 'subscriber))) "Coverage" fhir)) 0)
  ;; a model whose records do not name their class: the data keyed by class name
  (check-equal? ((resources->data (hash "Visit" (list (hasheq 'id "v1"))) #:patient-context? #f) "Visit" fhir) (list (hasheq 'id "v1")))
  ;; terminology: an expansion, else the enumerated compose; |version ignored; unknown -> #f
  (define vs-exp (hasheq 'url "http://x/vs1" 'expansion (hasheq 'contains (list (hasheq 'system "http://x/cs" 'code "a")))))
  (define vs-cmp (hasheq 'url "http://x/vs2" 'compose (hasheq 'include (list (hasheq 'system "http://x/cs" 'concept (list (hasheq 'code "b")))))))
  (define tx (valuesets->terminology (list vs-exp vs-cmp)))
  (check-true (hash-ref (tx "http://x/vs1") (cons "http://x/cs" "a") #f))
  (check-true (hash-ref (tx "http://x/vs2|1.0") (cons "http://x/cs" "b") #f))
  (check-false (hash-ref (tx "http://x/vs2") (cons "http://x/cs" "a") #f))
  (check-false (tx "http://x/none"))
  (check-true (truthy? (list 1))) (check-false (truthy? '())) (check-false (truthy? #f))
  ;; a caching resolver asks the underlying one once per (name, version)
  (define asked 0)
  (define r (caching-resolver (lambda (n v) (set! asked (add1 asked)) (and (eq? n 'A) 'library-a))))
  (check-equal? (list (r 'A "1") (r 'A "1") (r 'B "1") (r 'B "1")) '(library-a library-a #f #f))
  (check-equal? asked 2))
