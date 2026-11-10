#lang racket/base

;;; workspace.rkt — what CQL offers a workspace (fhir/lang/workspace): its libraries as Library
;;; resources with every translation error, a library run over every patient of the data source, and
;;; the explanation of one patient's answer, define by define.
;;;
;;; Libraries resolve across the workspace: an include names another .cql file's library, else a
;;; Library in the data source or registered by a module, else one bundled here (`libraries/`,
;;; FHIRHelpers 4.0.1). Value sets resolve from what the workspace defines (fhir/registry — a VCL file,
;;; an FSH ValueSet; expanded through $expand when it has filters and fhir-tx is installed), then the
;;; data source, then the packages in context. Parameters are CQL expressions (workspace.yaml's
;;; `parameters`, then the run's own).
;;;
;;; Any data model: `using <Name>` finds the installed models (FHIR is shipped) and the ones
;;; workspace.yaml's `models` makes from the workspace's own logical models (an FSH `Logical:`), so a
;;; model designed in the workspace is queried as soon as it is written. The patients a run covers,
;;; and the records that are a patient's, follow the library's model: its Patient context's class
;;; and key, and its classes' relationships to that context.
;;;
;;; A Row of a run is {patient, <define>: value …}; a value is JSON: null, a boolean, a number, a
;;; string, a list, a record as "Type/id", a tuple as an object, anything else as CQL renders it.
;;; An explanation is a Step: {define, line, value, retrieves: [Retrieve], uses: [Step]}, where a
;;; Retrieve is {type, filter, line, patientHas, found: ["Type/id" …]}: how many resources of the type
;;; the patient has, and which the retrieve kept.

(require racket/contract racket/list racket/string racket/match racket/runtime-path racket/file racket/promise
         net/base64 json
         fhir/lang/workspace
         (only-in fhir/diagnostic diagnostic loc exn:fail:diagnostic exn:fail:diagnostic? exn:fail:diagnostic-diagnostic diagnostic-message)
         (only-in fhir/suggest closest-names)
         (only-in fhir/data-source current-data-source data-source-all data-source-search)
         (only-in fhir/registry registered-resources)
         (only-in fhir/result/result ok? ok-value err? err-error)
         (only-in "parser/parse.rkt" parse-cql parse-cql-expr)
         (only-in "parser/ast.rkt" cql-library? cql-library-id cql-library-version cql-library-defs cql-using?
                  cql-using-model cql-using-version cql-def? cql-def-name)
         (only-in "compile.rkt" compile-library compile-library/errors compile-library/diagnostics compile-expr current-library-resolver)
         (only-in "elm/ast.rkt" elm-library-statements elm-def? elm-def-name elm-def-expression elm-def-context
                  elm-retrieve? elm-retrieve-data-type elm-retrieve-codes elm-retrieve-code-property
                  elm-expression-ref? elm-expression-ref-name elm-expression-ref-library elm-term-ref? elm-term-ref-name)
         (only-in "interp.rkt" eval-elm make-ienv rlib-value rlib-eval)
         (only-in "run.rkt" load-runtime-library valuesets->terminology caching-resolver patient-data patient-class bundled-library)
         (only-in "model/modelinfo.rkt" find-model current-model-providers)
         (only-in "model/structure-definitions.rkt" structure-definitions->model logical-model?)
         (only-in "elm/ast.rkt" elm-library-usings elm-using-local-id elm-using-version)
         (only-in "representation.rkt" record-type-name)
         (only-in "measure.rkt" packages-valuesets)
         (only-in "value.rkt" cql-code? cql-code-code cql-code-system cql-concept? cql-concept-codes cql-null? cql-null-list? model-node? model-node-data model-node-model cql-tuple? cql-tuple-fields render
                  current-evaluation-offset)
         (only-in "error.rkt" cql-error? cql-error-diagnostic cql-error-message cql-error-phase cql-error-string))

(provide (contract-out [workspace-support ws-support?]
                       ;; guide : the language guide (guide.md) a hub serves to agents (learn://cql)
                       [guide string?]
                       [value->json (-> any/c jsexpr?)]))

(define-runtime-path guide-file "guide.md")
(define guide (file->string guide-file))

(define (path-string p) (if (path? p) (path->string p) p))

;; ---- data models ----------------------------------------------------------------------------------
;; model-provider : workspace -> (name version -> model|#f)   the models workspace.yaml names, each made
;; from the registered logical models under its canonical (the same model while they do not change)
(define built-models (make-hash))
(define (model-provider ws)
  (define specs (ws-config-models (workspace-config ws)))
  (lambda (name version)
    (define spec (hash-ref specs name #f))
    (and spec (workspace-model name version spec))))
(define (workspace-model name version spec)
  (define parts (string-split spec))
  (define url (car parts))
  (define opts (for/hash ([p (in-list (cdr parts))] #:when (regexp-match? #rx"=" p))
                 (let ([kv (string-split p "=")]) (values (car kv) (cadr kv)))))
  (define v (or version (hash-ref opts "version" "1.0.0")))
  (and (or (not (hash-ref opts "version" #f)) (equal? v (hash-ref opts "version")))
       (let ([sds (filter (lambda (sd) (and (logical-model? sd) (string-prefix? (hash-ref sd 'url "") url)))
                          (registered-resources "StructureDefinition"))])
         (and (pair? sds)
              (hash-ref! built-models (list name v spec sds)
                         (lambda ()
                           (structure-definitions->model sds #:name name #:version v #:url url
                                                         #:patient (hash-ref opts "patient" #f)
                                                         #:birth-date (hash-ref opts "birthDate" #f)
                                                         #:patient-key (hash-ref opts "key" "id"))))))))
;; with-models : workspace thunk -> any   the workspace's models findable by `using`
(define (with-models ws thunk)
  (parameterize ([current-model-providers (cons (model-provider ws) (current-model-providers))]) (thunk)))

;; ---- libraries ----------------------------------------------------------------------------------
;; query-text : text -> text   an expression, not a library, wrapped as one: its value is "Query"
(define (library-text? text) (regexp-match? #px"^\\s*(?:(?://[^\n]*\n|/\\*.*?\\*/)\\s*)*library\\b" text))
;; The wrapper includes FHIRHelpers and every library of the workspace over FHIR 4.0.1, each called
;; by its own name, so a query reads a library's defines as `Diabetes."Latest Value"`.
(define (as-library text [ws #f])
  (if (library-text? text) text
      (string-append "library Query\nusing FHIR version '4.0.1'\ninclude FHIRHelpers version '4.0.1' called FHIRHelpers\n"
                     (apply string-append (map (lambda (i) (string-append i "\n")) (query-includes ws)))
                     "context Patient\ndefine \"Query\":\n" text "\n")))

;; query-includes : workspace|#f -> (listof string)   an include per workspace library a query can use
(define (query-includes ws)
  (if (not ws) '()
      (for/list ([(name lib) (in-hash (workspace-libraries ws))]
                 #:when (regexp-match? #px"^[A-Za-z_][A-Za-z0-9_]*$" name)
                 #:unless (member name '("Query" "FHIRHelpers"))
                 #:when (for/and ([u (in-list (cql-library-defs lib))] #:when (cql-using? u))
                          (or (not (equal? (format "~a" (cql-using-model u)) "FHIR")) (member (cql-using-version u) '(#f "4.0.1"))))
                 #:when (for/or ([u (in-list (cql-library-defs lib))]) (cql-using? u)))
        (define v (cql-library-version lib))
        (format "include ~a~a called ~a" name (if v (format " version '~a'" v) "") name))))

;; query-error : exn workspace -> exn   an ad-hoc query's translation error, about the query's own text:
;; positioned in it (not at the wrapper's `define "Query"`), with the workspace's defines as suggestions
(define (query-error e ws)
  (define d (and (exn:fail:diagnostic? e) (exn:fail:diagnostic-diagnostic e)))
  (define msg (regexp-replace #px"^in (?:define )?\"?Query\"?: "(if d (diagnostic-message d) (exn-message e)) ""))
  (define unknown (let ([m (regexp-match #px"unknown identifier '([^']+)'" msg)]) (and m (cadr m))))
  (define qualified                                     ; Lib."Define" for every workspace library's define
    (if ws
        (for*/list ([(name lib) (in-hash (workspace-libraries ws))]
                    [d (in-list (cql-library-defs lib))] #:when (cql-def? d))
          (cons (format "~a" (cql-def-name d)) (format "~a.\"~a\"" name (cql-def-name d))))
        '()))
  (define near (if unknown (closest-names unknown (map car qualified)) '()))
  (define hint (cond [(regexp-match? #rx"did you mean" msg) ""]
                     [(pair? near) (format " — a workspace library defines it: ~a"
                                           (string-join (for/list ([n (in-list near)]) (cdr (assoc n qualified))) " or "))]
                     [(and unknown (pair? qualified)) " — the workspace's libraries are included by name (Library.\"Define\")"]
                     [else ""]))
  (define text (string-append msg hint))
  (raise (exn:fail:diagnostic (string-append "cql: " text) (current-continuation-marks)
                              (diagnostic 'cql 'compile 'error text (loc "query" 1 1 #f #f)))))

;; parse : text path|#f -> cql-library   raises the positioned diagnostic
(define (parse text path)
  (define r (parse-cql (blank-lang text) #:source (and path (path-string path))))
  (if (ok? r) (ok-value r) (raise-cql (err-error r) path)))
(define (blank-lang text) (regexp-replace #px"^#lang[^\n]*" text ""))

(define (->diagnostic e path)
  (or (cql-error-diagnostic e)
      (diagnostic 'cql (case (cql-error-phase e) [(translate) 'compile] [(interpret) 'run] [else (cql-error-phase e)])
                  'error (cql-error-message e) (loc (and path (path-string path)) #f #f #f #f))))
(define (raise-cql e path)
  (define d (->diagnostic e path))
  (raise (exn:fail:diagnostic (cql-error-string e) (current-continuation-marks) d)))

;; the workspace's libraries: name -> cql-library (each .cql file that parses), read per call
(define (workspace-libraries ws)
  (for*/hash ([f (in-list (workspace-files ws))]
              #:when (equal? (workspace-language-of f) "cql")
              [r (in-value (parse-cql (blank-lang (file->string f)) #:source (path->string f)))]
              #:when (ok? r)
              #:when (cql-library-id (ok-value r)))
    (values (format "~a" (cql-library-id (ok-value r))) (ok-value r))))

;; library-content : Library resource -> text|#f
(define (library-content lib)
  (for/first ([c (in-list (hash-ref lib 'content '()))]
              #:when (and (equal? (hash-ref c 'contentType #f) "text/cql") (string? (hash-ref c 'data #f))))
    (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 (hash-ref c 'data))))))

;; make-resolver : workspace -> (name version -> cql-library|#f)
(define (make-resolver ws)
  (define own (workspace-libraries ws))
  ;; the data source's Libraries by name (searched where it searches), none when it cannot answer
  (define (source-libraries name)
    (define ds (current-data-source))
    (with-handlers ([exn:fail? (lambda (_) '())])
      (if (data-source-search ds) ((data-source-search ds) "Library" (list (cons "name" name))) ((data-source-all ds) "Library"))))
  (define (from-resources name)
    (define libs (append (registered-resources "Library") (source-libraries name)))
    (for/or ([l (in-list libs)] #:when (equal? (hash-ref l 'name #f) name))
      (define t (library-content l))
      (and t (let ([r (parse-cql t)]) (and (ok? r) (ok-value r))))))
  (caching-resolver
   (lambda (name version)
     (define n (format "~a" name))
     ;; last, a library shipped in libraries/ (run.rkt's bundled-library: FHIRHelpers 4.0.1 and 4.4.000)
     (or (hash-ref own n #f) (from-resources n) (bundled-library n version)))))

;; version-diagnostics : workspace library text path -> diagnostics   CQL's FHIR model here is 4.0.1
(define (version-diagnostics ws lib text path)
  (for/list ([u (in-list (cql-library-defs lib))]
             #:when (and (cql-using? u) (equal? (format "~a" (cql-using-model u)) "FHIR"))
             #:unless (and (workspace-r4? ws) (member (cql-using-version u) '(#f "4.0.1"))))
    (define line (line-matching text #px"^\\s*using\\s+FHIR"))
    (diagnostic 'cql 'compile 'error
                (format "CQL runs against the FHIR 4.0.1 model here; this library uses FHIR ~a and the workspace is FHIR ~a"
                        (or (cql-using-version u) "(no version)") (ws-config-fhir-version (workspace-config ws)))
                (loc (path-string path) line (and line 1) #f #f))))

;; line-matching : text regexp -> 1-based line|#f
(define (line-matching text rx)
  (for/first ([l (in-list (regexp-split #rx"\n" text))] [i (in-naturals 1)] #:when (regexp-match? rx l)) i))
(define (define-line text name)
  (line-matching text (pregexp (string-append "^\\s*(?:public\\s+|private\\s+)?define\\s+(?:fluent\\s+)?(?:function\\s+)?(?:\""
                                              (regexp-quote name) "\"|" (regexp-quote name) "\\b)"))))

(define (library-resource ws lib text path)
  (define name (format "~a" (cql-library-id lib)))
  (define version (cql-library-version lib))
  (define base (hasheq 'resourceType "Library" 'id (regexp-replace* #rx"[^A-Za-z0-9.-]" name "-")
                       'url (string-append (ws-config-canonical (workspace-config ws)) "/Library/" name)
                       'name name 'status "active"
                       'type (hasheq 'coding (list (hasheq 'system "http://terminology.hl7.org/CodeSystem/library-type" 'code "logic-library")))
                       'content (list (hasheq 'contentType "text/cql" 'data (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 text) #""))))))
  (if version (hash-set base 'version version) base))

;; artifacts : workspace files -> (values Library resources diagnostics)
(define (artifacts ws files) (with-models ws (lambda () (artifacts* ws files))))
(define (artifacts* ws files)
  (define resolver (make-resolver ws))
  (define seen (make-hash))                              ; library name -> the file that declared it first
  (for/fold ([rs '()] [ds '()] #:result (values (reverse rs) ds)) ([f (in-list files)])
    (define text (blank-lang (cdr f)))
    (define r (parse-cql text #:source (path-string (car f))))
    (cond
      [(err? r) (values rs (append ds (list (->diagnostic (err-error r) (car f)))))]
      [else
       (define lib (ok-value r))
       (define name (and (cql-library-id lib) (format "~a" (cql-library-id lib))))
       (define first (and name (hash-ref seen name #f)))
       (when (and name (not first)) (hash-set! seen name (car f)))
       (define-values (_elm errors warnings) (parameterize ([current-library-resolver resolver]) (compile-library/diagnostics lib)))
       (define twice
         (if first
             (list (diagnostic 'cql 'compile 'error (format "the library ~a is also declared in ~a: an include could mean either" name (path-string first))
                               (loc (path-string (car f)) (line-matching text #px"^\\s*library\\b") 1 #f #f)))
             '()))
       (values (if (and name (not first)) (cons (library-resource ws lib (cdr f) (car f)) rs) rs)
               (append ds twice (version-diagnostics ws lib text (car f)) (map (lambda (e) (->diagnostic e (car f))) errors)
                       warnings))])))

;; ---- what a run needs ---------------------------------------------------------------------------
;; terminology : -> (url -> members|#f)   the workspace's value sets, the data source's, the packages'
(define (terminology)
  (define own (valuesets->terminology (map expanded (registered-resources "ValueSet"))))
  (define source (source-valuesets (current-data-source)))
  (define pkgs (packages-valuesets))
  (lambda (url) (or (own url) (source url) (pkgs url))))
;; source-valuesets : data-source -> (url -> members|#f)   asked only for a url the workspace does not
;; define: searched by url where the source searches (a FHIR server), else its ValueSets read once; a
;; source that cannot answer (a server without ValueSet search) holds none
(define (source-valuesets ds)
  (define memo (make-hash))
  (define all (delay (with-handlers ([exn:fail? (lambda (_) (lambda (u) #f))])
                       (valuesets->terminology ((data-source-all ds) "ValueSet")))))
  (lambda (url)
    (hash-ref! memo url
               (lambda ()
                 (define key (car (string-split url "|")))
                 (cond [(data-source-search ds)
                        (with-handlers ([exn:fail? (lambda (_) #f)])
                          ((valuesets->terminology ((data-source-search ds) "ValueSet" (list (cons "url" key)))) url))]
                       [else ((force all) url)])))))
;; expanded : ValueSet -> ValueSet   with its expansion when the compose is more than enumerated codes
;; and the terminology service is installed; else as it is (the enumerated codes are its members)
(define (expanded vs)
  (define includes (append (hash-ref (hash-ref vs 'compose (hasheq)) 'include '()) (hash-ref (hash-ref vs 'compose (hasheq)) 'exclude '())))
  (define enumerated? (for/and ([i (in-list includes)]) (and (pair? (hash-ref i 'concept '())) (null? (hash-ref i 'filter '())) (null? (hash-ref i 'valueSet '())))))
  (cond
    [(or (hash-has-key? vs 'expansion) enumerated?) vs]
    [else
     (with-handlers ([exn:fail? (lambda (_) vs)])
       (dynamic-require 'fhir/terminology/operations #f)
       ((dynamic-require 'fhir/operation 'apply-operation) "$expand" vs))]))

;; parameters : workspace options -> (hash name -> CqlValue)   each a CQL expression, evaluated
(define (parameters ws opts)
  (define texts (for/fold ([h (ws-config-parameters (workspace-config ws))]) ([(k v) (in-hash (hash-ref opts 'parameters (hash)))])
                  (hash-set h (format "~a" k) v)))
  (for/hash ([(name text) (in-hash texts)])
    (define p (parse-cql-expr (format "~a" text)))
    (define c (and (ok? p) (compile-expr (ok-value p))))
    (unless (and c (ok? c))
      (error 'cql (format "the parameter ~s: ~a is not a CQL expression (~a)" name text
                          (cql-error-message (err-error (if (and c (err? c)) c p))))))
    (values name (parameterize ([current-evaluation-offset 0]) (eval-elm (ok-value c) (make-ienv))))))

;; prepared : workspace path text options -> (values library elm defines (pid -> rlib) data-of)
(define (prepare ws path text opts)
  (define src (if path text (as-library text ws)))
  (define lib (parse src path))
  (define resolver (make-resolver ws))
  (define c (parameterize ([current-library-resolver resolver]) (compile-library lib)))
  (when (err? c)
    (if (or path (library-text? text))
        (raise-cql (err-error c) path)
        (with-handlers ([exn:fail? (lambda (e) (query-error e ws))]) (raise-cql (err-error c) path))))
  (define vdiags (version-diagnostics ws lib src (or path "query")))
  (when (pair? vdiags) (raise (exn:fail:diagnostic "FHIR version" (current-continuation-marks) (car vdiags))))
  (define elm (ok-value c))
  (define defines (for/list ([s (in-list (elm-library-statements elm))]
                             #:when (elm-def? s)
                             #:unless (equal? (elm-def-name s) "Patient"))
                    (elm-def-name s)))
  (define vs (terminology))
  (define params (parameters ws opts))
  (define ds (current-data-source))
  (define m (library-model elm))
  ;; a type's records read once for the run, then scoped to each patient by the model
  (define all (make-hash))
  (define (all-of type) (hash-ref! all type (lambda () ((data-source-all ds) type))))
  (define (data-of pid) (patient-data all-of pid))
  (define (rlib-for pid)
    (load-runtime-library lib #:resolver resolver #:data (data-of pid) #:valuesets vs #:parameters params #:offset 0))
  (values src elm defines rlib-for data-of m))

;; library-model : elm-library -> model|#f   the data model its `using` names (System aside)
(define (library-model elm)
  (for/or ([u (in-list (elm-library-usings elm))] #:unless (member (format "~a" (elm-using-local-id u)) '("System")))
    (find-model (format "~a" (elm-using-local-id u)) (elm-using-version u))))

;; patient-exists? : model key -> boolean   a record of the Patient context class has that key
(define (patient-exists? m pid)
  (define-values (pclass key) (patient-class m))
  (for/or ([p (in-list ((data-source-all (current-data-source)) pclass))])
    (equal? (format "~a" (hash-ref p (string->symbol key) "")) pid)))

;; patients : model options -> (values class-name (listof key))   the patients a run covers: `over`
;; (<Class>/<key>, or Patient/<key>), else every record of the model's Patient context class
(define (patients m opts)
  (define-values (pclass key) (if m (patient-class m) (values #f #f)))
  (unless pclass (error 'cql "the library's data model has no Patient context to run over"))
  (define over (hash-ref opts 'over #f))
  (define prefix (pregexp (string-append "^(?:Patient|" (regexp-quote pclass) ")/(.+)$")))
  (values pclass
          (cond [(and over (regexp-match prefix over)) => (lambda (x) (list (cadr x)))]
                [(and over (not (member over (list "Patient" pclass))))
                 (error 'cql (format "a CQL library runs over patients: give `over` as ~a/<id>, or leave it out for every patient (got ~a)" pclass over))]
                [else (for*/list ([p (in-list ((data-source-all (current-data-source)) pclass))]
                                  [k (in-value (hash-ref p (string->symbol key) #f))] #:when k)
                        (format "~a" k))])))

;; value->json : CqlValue -> jsexpr
(define (value->json v)
  (cond [(or (cql-null? v) (cql-null-list? v)) 'null]
        [(boolean? v) v]
        [(exact-integer? v) v]
        [(string? v) v]
        [(list? v) (map value->json v)]
        [(model-node? v)
         (define j (model-node-data v))
         (define rt (and (hash? j) (record-type-name (model-node-model v) j)))
         (cond [(and rt (string? (hash-ref j 'id #f)))
                (format "~a/~a" (regexp-replace #rx"^.*[.]" rt "") (hash-ref j 'id))]
               [(jsexpr? j) j]
               [else (render v)])]
        [(cql-tuple? v) (for/hasheq ([(k x) (in-hash (cql-tuple-fields v))]) (values (string->symbol (format "~a" k)) (value->json x)))]
        [else (render v)]))

;; ---- run ----------------------------------------------------------------------------------------
(define (run ws path text opts) (with-models ws (lambda () (run* ws path text opts))))
(define (run* ws path text opts)
  (define-values (src elm defines rlib-for _data m) (prepare ws path text opts))
  (define-values (_pclass pids) (patients m opts))
  (define entry (hash-ref opts 'entry #f))
  (when (and entry (not (member entry defines)))
    (error 'cql (format "no define ~s in the library (defines: ~a)" entry (string-join defines ", "))))
  (define shown (if entry (list entry) defines))
  (define rows
    (for/list ([pid (in-list pids)])
      (define rl (rlib-for pid))
      (for/fold ([row (hasheq 'patient pid)]) ([d (in-list shown)])
        (hash-set row (string->symbol d)
                  (with-handlers ([cql-error? (lambda (e) (hasheq 'error (cql-error-message e)))]
                                  [exn:fail? (lambda (e) (hasheq 'error (exn-message e)))])
                    (value->json (rlib-value rl d)))))))
  (run-result "patients" (cons "patient" shown) rows
              (hasheq 'patients (length rows)
                      'defines (for/hasheq ([d (in-list shown)]) (values (string->symbol d) (tally (map (lambda (r) (hash-ref r (string->symbol d))) rows)))))))

;; tally : (listof value) -> {true false null empty nonEmpty other errors}   only the non-zero counts
(define (tally vs)
  (define (count p) (for/sum ([v (in-list vs)]) (if (p v) 1 0)))
  (for/hasheq ([k '(true false null empty nonEmpty other errors)]
               [n (in-list (list (count (lambda (v) (eq? v #t))) (count (lambda (v) (eq? v #f))) (count (lambda (v) (eq? v 'null)))
                                 (count null?) (count pair?)
                                 (count (lambda (v) (not (or (boolean? v) (eq? v 'null) (list? v) (and (hash? v) (hash-has-key? v 'error))))))
                                 (count (lambda (v) (and (hash? v) (hash-has-key? v 'error))))))]
               #:when (positive? n))
    (values k n)))

;; ---- explain ------------------------------------------------------------------------------------
;; walk : ELM pred -> (listof node)   every node satisfying pred, in order, not looking inside them
(define (walk x pred)
  (let loop ([x x])
    (cond [(pred x) (list x)]
          [(pair? x) (append (loop (car x)) (loop (cdr x)))]
          [(struct? x) (append-map loop (cdr (vector->list (struct->vector x))))]
          [(prefab-struct-key x) (append-map loop (cdr (vector->list (struct->vector x))))]
          [else '()])))

(define (short-type t) (let ([m (regexp-match #px"[}]([^}]+)$" (format "~a" t))]) (if m (cadr m) (format "~a" t))))

(define (explain ws path text subject opts) (with-models ws (lambda () (explain* ws path text subject opts))))
(define (explain* ws path text subject opts)
  (define-values (src elm defines rlib-for data-of m) (prepare ws path text opts))
  (define-values (pclass pids) (patients m (hash-set opts 'over (if (regexp-match? #rx"/" subject) subject (string-append "Patient/" subject)))))
  (define pid (car pids))
  (unless (patient-exists? m pid) (error 'cql (format "no ~a/~a in the data source" pclass pid)))
  (define rl (rlib-for pid))
  (define data (let ([d (data-of pid)]) (lambda (type) (d type m))))
  (define table (for/hash ([s (in-list (elm-library-statements elm))] #:when (elm-def? s)) (values (elm-def-name s) s)))
  (define (value name) (with-handlers ([exn:fail? (lambda (e) (hasheq 'error (exn-message e)))]) (value->json (rlib-value rl name))))
  (define (retrieve-info r)
    (define type (short-type (elm-retrieve-data-type r)))
    (define codes (elm-retrieve-codes r))
    (define found (with-handlers ([exn:fail? (lambda (e) (hasheq 'error (exn-message e)))]) (value->json (rlib-eval rl r))))
    (hasheq 'type type
            'filter (cond [(not codes) 'null]
                          [(elm-term-ref? codes) (format "~a in ~s" (or (elm-retrieve-code-property r) "code") (elm-term-ref-name codes))]
                          ;; the codes themselves (`code ~ http://loinc.org|4548-4`): "filtered by codes" said nothing
                          [else (define cs (with-handlers ([exn:fail? (lambda (_) #f)]) (rlib-eval rl codes)))
                                (define listed (filter cql-code? (cond [(list? cs) cs] [(cql-code? cs) (list cs)] [(cql-concept? cs) (cql-concept-codes cs)] [else '()])))
                                (if (pair? listed)
                                    (format "~a ~~ ~a" (or (elm-retrieve-code-property r) "code")
                                            (string-join (for/list ([c (in-list listed)]) (format "~a|~a" (or (cql-code-system c) "") (cql-code-code c))) ", "))
                                    (format "~a filtered by codes" (or (elm-retrieve-code-property r) "code")))])
            'line (or (line-matching src (pregexp (string-append "\\[\\s*" (regexp-quote type) "\\b"))) 'null)
            'patientHas (length (data type))
            'found found))
  (define (step name depth seen)
    (define d (hash-ref table name))
    (define expr (elm-def-expression d))
    (define refs (remove-duplicates (walk expr elm-expression-ref?)))
    ;; a define that retrieves nothing, or uses no other define, says nothing of it (the keys were
    ;; noise in a rerun's answers)
    (define (unless-empty k v h) (if (null? v) h (hash-set h k v)))
    (unless-empty
     'retrieves (map retrieve-info (walk expr elm-retrieve?))
     (unless-empty
     'uses (for/list ([r (in-list refs)])
                    (define n (elm-expression-ref-name r))
                    (cond [(elm-expression-ref-library r)
                           (hasheq 'define (format "~a.~a" (elm-expression-ref-library r) n)
                                   'value (with-handlers ([exn:fail? (lambda (e) (hasheq 'error (exn-message e)))]) (value->json (rlib-eval rl r))))]
                          [(or (member n seen) (not (hash-has-key? table n))) (hasheq 'define n 'value (value n))]
                          [(>= depth 4) (hasheq 'define n 'value (value n) 'line (or (define-line src n) 'null))]
                          [else (step n (add1 depth) (cons n seen))]))
     (hasheq 'define name
             'line (or (define-line src name) 'null)
             'value (value name)))))
  (define entry (or (hash-ref opts 'entry #f) (and (not path) "Query")))
  (cond
    [entry (unless (hash-has-key? table entry)
             (error 'cql (format "no define ~s in the library (defines: ~a)" entry (string-join defines ", "))))
           (hash-set (step entry 0 (list entry)) 'patient pid)]
    [else (hasheq 'patient pid
                  'defines (for/list ([d (in-list defines)]) (hasheq 'define d 'line (or (define-line src d) 'null) 'value (value d))))]))

(define workspace-support (ws-support "cql" artifacts run explain))

(module+ test
  (require rackunit racket/file fhir/registry (only-in fhir/data-source resources->data-source)
           (only-in fhir/diagnostic diagnostic-message diagnostic-loc loc-line))
  (define dir (make-temporary-directory "cql-ws-~a"))
  (define-values (ws _w) (open-workspace dir))
  (define common "library Common version '1'\nusing FHIR version '4.0.1'\ncontext Patient\ndefine \"Adult\": AgeInYears() >= 18\n")
  (define main (string-append "library Gap version '1'\nusing FHIR version '4.0.1'\n"
                              "include FHIRHelpers version '4.0.1' called FHIRHelpers\n"
                              "include Common version '1' called C\n"
                              "valueset \"HbA1c\": 'http://example.org/ValueSet/hba1c'\n"
                              "context Patient\n"
                              "define \"Tests\": [Observation: \"HbA1c\"]\n"
                              "define \"No HbA1c\": not exists \"Tests\"\n"
                              "define \"Gap\": C.\"Adult\" and \"No HbA1c\"\n"))
  (with-output-to-file (build-path dir "Common.cql") (lambda () (display common)))
  (with-output-to-file (build-path dir "Gap.cql") (lambda () (display main)))
  ;; artifacts: two Libraries, no problems (FHIRHelpers is bundled, Common is beside)
  (define-values (rs ds) (artifacts ws (list (cons (build-path dir "Common.cql") common) (cons (build-path dir "Gap.cql") main))))
  (check-equal? ds '())
  (check-equal? (map (lambda (r) (hash-ref r 'name)) rs) '("Common" "Gap"))
  (check-equal? (hash-ref (car rs) 'url) "http://example.org/workspace/Library/Common")
  ;; a translation error is positioned in its file
  (define-values (_r2 ds2) (artifacts ws (list (cons (build-path dir "Bad.cql") "library Bad\ndefine X: Nope\n"))))
  (check-equal? (length ds2) 1)
  ;; a library name declared by two files: the second is a problem, at its `library` line
  (define-values (rs3 ds3) (artifacts ws (list (cons (build-path dir "A.cql") "library Same\n") (cons (build-path dir "B.cql") "\nlibrary Same\n"))))
  (check-equal? (length rs3) 1)
  (check-equal? (length ds3) 1)
  (check-equal? (loc-line (diagnostic-loc (car ds3))) 2)
  ;; the value set, as the workspace would register it from a VCL file
  (register-resources! (list (hasheq 'resourceType "ValueSet" 'url "http://example.org/ValueSet/hba1c"
                                     'compose (hasheq 'include (list (hasheq 'system "http://loinc.org" 'concept (list (hasheq 'code "4548-4")))))))
                       #:source 'test)
  (define (obs id pid code) (hasheq 'resourceType "Observation" 'id id 'status "final" 'subject (hasheq 'reference (string-append "Patient/" pid))
                                    'code (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code code)))))
  (define data (list (hasheq 'resourceType "Patient" 'id "a" 'birthDate "1960-01-01")
                     (hasheq 'resourceType "Patient" 'id "b" 'birthDate "1970-01-01")
                     (hasheq 'resourceType "Patient" 'id "kid" 'birthDate "2020-01-01")
                     (obs "o1" "a" "4548-4") (obs "o2" "b" "1234-5")))
  (parameterize ([current-data-source (resources->data-source data)])
    (define r (run ws (build-path dir "Gap.cql") main (hasheq)))
    (check-equal? (run-result-columns r) '("patient" "Tests" "No HbA1c" "Gap"))
    (define by-pid (for/hash ([row (run-result-rows r)]) (values (hash-ref row 'patient) row)))
    (check-equal? (hash-ref (hash-ref by-pid "a") 'Gap) #f)
    (check-equal? (hash-ref (hash-ref by-pid "b") 'Gap) #t)
    (check-equal? (hash-ref (hash-ref by-pid "kid") 'Gap) #f)
    (check-equal? (hash-ref (hash-ref by-pid "a") 'Tests) '("Observation/o1"))
    (check-equal? (hash-ref (hash-ref (run-result-summary r) 'defines) 'Gap) (hasheq 'true 1 'false 2))
    ;; one define, one patient
    (define r1 (run ws (build-path dir "Gap.cql") main (hasheq 'entry "Gap" 'over "Patient/b")))
    (check-equal? (run-result-rows r1) (list (hasheq 'patient "b" 'Gap #t)))
    (check-exn #rx"no define \"Nope\"" (lambda () (run ws (build-path dir "Gap.cql") main (hasheq 'entry "Nope"))))
    ;; an expression, not a library: it is the define "Query"
    (define q (run ws #f "AgeInYears() > 60" (hasheq)))
    (check-equal? (map (lambda (row) (hash-ref row 'Query)) (run-result-rows q)) '(#t #f #f))
    ;; it reads the workspace's libraries by name; a misspelt define is suggested, positioned in the query
    (check-equal? (map (lambda (row) (hash-ref row 'Query)) (run-result-rows (run ws #f "Gap.\"Gap\"" (hasheq 'over "Patient/b"))))
                  '(#t))
    (check-exn (lambda (e) (and (exn:fail:diagnostic? e)
                                (regexp-match? #rx"^unknown identifier 'Gpa'.*Gap[.]\"Gap\"" (diagnostic-message (exn:fail:diagnostic-diagnostic e)))
                                (equal? (loc-line (diagnostic-loc (exn:fail:diagnostic-diagnostic e))) 1)))
               (lambda () (run ws #f "\"Gpa\"" (hasheq))))
    ;; a qualified name misspelt: the nearest define of that library, written as it is referenced
    (check-exn #rx"unknown identifier 'Gap.Gpa' [(]did you mean `Gap.\"Gap\"`[?][)]"
               (lambda () (run ws #f "Gap.\"Gpa\"" (hasheq))))
    ;; parameters are CQL expressions
    (define plib "library P\nusing FHIR version '4.0.1'\nparameter \"Cutoff\" Integer\ncontext Patient\ndefine \"Twice\": \"Cutoff\" * 2\n")
    (define pq (run ws #f plib (hasheq 'parameters (hasheq 'Cutoff "3 + 4") 'over "Patient/a")))
    (check-equal? (run-result-rows pq) (list (hasheq 'patient "a" 'Twice 14)))
    (check-exn #rx"Cutoff.*not a CQL expression" (lambda () (run ws #f plib (hasheq 'parameters (hasheq 'Cutoff "3 +")))))
    ;; explain: patient b's Gap — Adult from Common, No HbA1c from Tests, whose retrieve read one
    ;; Observation and kept none
    (define e (explain ws (build-path dir "Gap.cql") main "Patient/b" (hasheq 'entry "Gap")))
    (check-equal? (hash-ref e 'value) #t)
    (check-equal? (hash-ref e 'line) 9)
    (define uses (hash-ref e 'uses))
    (check-equal? (map (lambda (u) (hash-ref u 'define)) uses) '("C.Adult" "No HbA1c"))
    (define tests (car (hash-ref (cadr uses) 'uses)))
    (check-equal? (hash-ref tests 'define) "Tests")
    (define rt (car (hash-ref tests 'retrieves)))
    (check-equal? (hash-ref rt 'type) "Observation")
    (check-equal? (hash-ref rt 'patientHas) 1)
    (check-equal? (hash-ref rt 'found) '())
    (check-equal? (hash-ref rt 'line) 7)
    (check-regexp-match #rx"HbA1c" (hash-ref rt 'filter))
    ;; a retrieve by code says which code (it said "code filtered by codes")
    (define clib (string-append "library Codes\nusing FHIR version '4.0.1'\ninclude FHIRHelpers version '4.0.1' called FHIRHelpers\n"
                                "codesystem \"LOINC\": 'http://loinc.org'\ncode \"A1c\": '4548-4' from \"LOINC\"\n"
                                "context Patient\ndefine \"Results\": [Observation: \"A1c\"]\n"))
    (define ce (explain ws #f clib "Patient/b" (hasheq 'entry "Results")))
    (check-equal? (hash-ref (car (hash-ref ce 'retrieves)) 'filter) "code ~ http://loinc.org|4548-4")
    (define all (explain ws (build-path dir "Gap.cql") main "a" (hasheq)))
    (check-equal? (length (hash-ref all 'defines)) 3)
    (check-exn #rx"no Patient/zz" (lambda () (explain ws (build-path dir "Gap.cql") main "zz" (hasheq)))))
  ;; an R5 workspace: the model here is 4.0.1, said at the `using` line
  (define ws5 (workspace dir (struct-copy ws-config (workspace-config ws) [fhir-version "5.0.0"])))
  (define-values (_r5 ds5) (artifacts ws5 (list (cons (build-path dir "Common.cql") common))))
  (check-regexp-match #rx"workspace is FHIR 5.0.0" (diagnostic-message (car ds5)))
  (unregister-source! 'test)
  (delete-directory/files dir))
