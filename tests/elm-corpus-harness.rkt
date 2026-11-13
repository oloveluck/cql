#lang racket/base

;;; elm-corpus-harness.rkt — real CQL libraries against the reference translator's ELM.
;;;
;;; tests/vendor/elm-corpus/<package>/<Library>.cql + <Library>.elm.json come from published FHIR
;;; packages (see its SOURCE.txt): the CQL an IG ships and the ELM the cqframework cql-to-elm
;;; translator produced from it. For each library this harness records
;;;   parse     — parse-cql succeeds
;;;   compile   — compile-library succeeds
;;;   defs      — for every `define` in the reference ELM: whether ours has a definition of that name,
;;;               and whether its expression is STRUCTURALLY EQUAL once translator-only metadata is
;;;               dropped (localId, locator, annotation, resultTypeName, resultTypeSpecifier,
;;;               signature) — the ELM differential
;;; Nothing here evaluates CQL (that needs retrieves + a data source); this is the translation oracle.
;;;
;;; Data: (lib-result package name parse-ok? compile-ok? error defs-total defs-present defs-equal
;;;                   mismatches)   mismatches : (listof (list def-name Difference))
;;; A Difference is (difference reference ours what): the first place the two trees part, as the node
;;; each has there ("Union", "FunctionRef:ToConcept", "Property.code", "As as {…}Concept") and what
;;; parts (`missing operand`, `extra type`, `length 2 1`, `"in"|"~"`); keys are compared in name
;;; order, so the first difference is the same every run. Classified by the pair of nodes, it says
;;; what the reference translator wrote that this one did not ("Union | Retrieve").

(require racket/contract racket/list racket/string racket/file racket/path racket/match json
         (only-in cql/parser/parse parse-cql)
         (only-in cql/parser/ast cql-library-version)
         (only-in cql/compile compile-library current-library-resolver)
         (only-in cql/elm/json elm->jsexpr)
         (only-in cql/error cql-error-string cql-error?)
         (only-in fhir/result/result ok? ok-value err-error))

(provide
 (struct-out lib-result)
 (contract-out
  [check-elm-corpus (-> path-string? (listof lib-result?))]
  ;; corpus-libraries : dir -> (listof (list package name cql-library resolver))   every corpus library
  ;; that parses, with the resolver its includes use (the evaluator differential runs them)
  [corpus-libraries (-> path-string? (listof list?))]
  [normalize-elm (->* (any/c) (any/c) any/c)]
  ;; print-corpus-report : (listof lib-result) label -> void   the totals, then the work list: every
  ;; mismatch classified by its first difference (kind + path, list indices dropped), largest class
  ;; first, with examples (library/definition: reference | ours)
  [print-corpus-report (-> (listof lib-result?) string? void?)]))

(struct lib-result (package name parse-ok? compile-ok? error defs-total defs-present defs-equal mismatches)
  #:transparent)

(define dropped-keys '(localId locator annotation resultTypeName resultTypeSpecifier signature))
;; structural element tags some translator versions write and others do not — the position in the
;; tree already says what the element is
(define structural-types '("InstanceElement" "AliasedQuerySource" "ReturnClause" "CaseItem" "TupleElement"
                           "LetClause" "SortClause" "AggregateClause" "TupleElementDefinition"))

;; normalize-elm : jsexpr [key] -> jsexpr   translator-only metadata removed, numbers as strings.
;; Also version noise: an EMPTY list is the same as an absent one (newer translators write `let: []`,
;; `dateFilter: []`, …), structural element `type` tags are dropped, `strict: false` (the schema
;; default) is the same as no `strict`, and around value sets — ValueSetRef's `preserve`, its `type`
;; where the position says it (InValueSet's `valueset`, Retrieve's `codes`), Retrieve's default
;; `codeComparator` "in" — the two corpus packages' translators disagree, so neither form is held.
;; The same for the empty String literal: hl7.fhir.uv.cql's translator leaves its `value` out, the
;; others (and ours) write `"value": ""`; and for a Ratio's parts, which fhir.cqf.common's translator
;; tags `"type": "Quantity"` and the newer ones (and the schema, which declares them Quantity) do not.
;; The ORDER of a ChoiceTypeSpecifier's choices is not held either: for the choice a union of
;; differently typed lists is cast to, the reference's is the iteration order of a Java HashSet of the
;; types (Procedure before MedicationRequest, MedicationRequest before MedicationAdministration: their
;; names' hash buckets), which says nothing about the choice; ours is the order of the operands.
(define (normalize-elm j [key #f])
  (cond [(hash? j) (for/hash ([(k v) (in-hash j)]
                              #:unless (and (eq? k 'value) (equal? v "") (equal? (hash-ref j 'type #f) "Literal"))
                              #:unless (memq k dropped-keys)
                              #:unless (null? v)
                              #:unless (and (eq? k 'type) (member v structural-types))
                              ;; As.strict defaults to false in the ELM schema
                              #:unless (and (eq? k 'strict) (eq? v #f))
                              #:unless (eq? k 'preserve)
                              ;; fhir.cqf.common's translator leaves out a FALSE closed flag (the
                              ;; schema default is true, so it is meaningful; our emitter writes it)
                              #:unless (and (memq k '(lowClosed highClosed)) (eq? v #f))
                              #:unless (and (eq? k 'type) (equal? v "ValueSetRef") (memq key '(valueset codes)))
                              #:unless (and (eq? k 'type) (equal? v "Quantity") (memq key '(numerator denominator)))
                              #:unless (and (eq? k 'codeComparator) (equal? v "in")))
                     (values k (if (eq? k 'choice)
                                   (sort (normalize-elm v k) string<? #:key canonical-text)
                                   (normalize-elm v k))))]
        [(list? j) (map (lambda (x) (normalize-elm x key)) j)]
        [(number? j) (number->string j)]
        [else j]))

;; canonical-text : jsexpr -> string   one text per value, whatever order a hash holds its keys in
(define (canonical-text j)
  (cond [(hash? j) (format "{~a}" (string-join (for/list ([k (in-list (sort (hash-keys j) symbol<?))])
                                                 (format "~a:~a" k (canonical-text (hash-ref j k))))
                                               ","))]
        [(list? j) (format "[~a]" (string-join (map canonical-text j) ","))]
        [else (format "~s" j)]))

(define (defs-of lib-js)
  (define stmts (let ([s (hash-ref lib-js 'statements #f)]) (if (hash? s) (hash-ref s 'def '()) '())))
  (for/hash ([d (in-list stmts)] #:when (hash? d)) (values (hash-ref d 'name #f) d)))

;; difference : the first place two normalised trees part (see the data definition above)
(struct difference (reference ours what) #:transparent)

;; node-name : jsexpr -> string   an ELM node by its type and what names it
(define (node-name n)
  (cond [(hash? n)
         (string-append (format "~a" (hash-ref n 'type "{}"))
                        (cond [(hash-ref n 'name #f) => (lambda (x) (format ":~a" x))]
                              [(hash-ref n 'path #f) => (lambda (x) (format ".~a" x))]
                              [else ""])
                        (cond [(hash-ref n 'asType #f) => (lambda (t) (format " as ~a" t))]
                              [(hash-ref n 'asTypeSpecifier #f) " as <specifier>"]
                              [else ""]))]
        [(list? n) (format "[~a]" (length n))]
        [else (format "~s" n)]))

;; first-diff : jsexpr jsexpr [enclosing nodes] -> Difference|#f
(define (first-diff a b [pa #f] [pb #f])
  (cond [(and (hash? a) (hash? b))
         (or (for/or ([k (in-list (sort (hash-keys a) symbol<?))])
               (if (hash-has-key? b k)
                   (first-diff (hash-ref a k) (hash-ref b k) a b)
                   (difference (node-name a) (node-name b) (format "missing ~a" k))))
             (for/or ([k (in-list (sort (hash-keys b) symbol<?))])
               (and (not (hash-has-key? a k)) (difference (node-name a) (node-name b) (format "extra ~a" k)))))]
        [(and (list? a) (list? b))
         (if (= (length a) (length b))
             (for/or ([x (in-list a)] [y (in-list b)]) (first-diff x y pa pb))
             (difference (node-name pa) (node-name pb) (format "length ~a ~a" (length a) (length b))))]
        [(equal? a b) #f]
        [(or (hash? a) (hash? b)) (difference (node-name a) (node-name b) "")]
        [else (difference (node-name pa) (node-name pb) (format "~s|~s" a b))]))

(define (error-text e) (if (cql-error? e) (cql-error-string e) (format "~a" e)))

(define (check-library pkg cql-file elm-file)
  (define name (path->string (path-replace-extension (file-name-from-path cql-file) #"")))
  (define ref (hash-ref (call-with-input-file elm-file read-json) 'library))
  (define ref-defs (defs-of ref))
  (define pr (with-handlers ([exn:fail? (lambda (e) (exn-message e))]) (parse-cql (file->string cql-file))))
  (define (fail parse? msg)
    (lib-result pkg name parse? #f msg (hash-count ref-defs) 0 0 '()))
  (cond
    [(string? pr) (fail #f pr)]
    [(not (ok? pr)) (fail #f (error-text (err-error pr)))]
    [else
     ;; includes resolve to the libraries shipped in the same package (FHIRHelpers, Global, SDE …)
     (define cr (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
                  (parameterize ([current-library-resolver (package-resolver (path-only cql-file))])
                    (compile-library (ok-value pr)))))
     (cond
       [(string? cr) (fail #t cr)]
       [(not (ok? cr)) (fail #t (error-text (err-error cr)))]
       [else
        (define ours (defs-of (hash-ref (elm->jsexpr (ok-value cr)) 'library)))
        (define-values (present equal mismatches)
          (for/fold ([p 0] [e 0] [ms '()]) ([(n d) (in-hash ref-defs)])
            (define o (hash-ref ours n #f))
            (cond
              [(not o) (values p e ms)]
              [else
               (define rd (normalize-elm (hash-ref d 'expression (hash))))
               (define od (normalize-elm (hash-ref o 'expression (hash))))
               (if (equal? rd od)
                   (values (add1 p) (add1 e) ms)
                   (values (add1 p) e (cons (list n (first-diff rd od)) ms)))])))
        (lib-result pkg name #t #t #f (hash-count ref-defs) present equal (reverse mismatches))])]))

;; package-resolver : dir -> (sym version -> cql-library|#f)
;; <dir>/<Name>.cql first; else the same library from another corpus package, preferring one whose
;; header version matches (CQF Measures includes FHIRHelpers 4.0.1 but ships it only as ELM — the
;; source lives in fhir.cqf.common)
(define parsed (make-hash))
(define (parse-file f)
  (hash-ref! parsed (path->string f)
             (lambda () (let ([r (parse-cql (file->string f))]) (and (ok? r) (ok-value r))))))
(define ((package-resolver dir) name version)
  (define f (build-path dir (format "~a.cql" name)))
  (cond
    [(file-exists? f) (parse-file f)]
    [else
     (define others
       (for*/list ([pkg (in-list (directory-list (simplify-path (build-path dir 'up)) #:build? #t))]
                   #:when (directory-exists? pkg)
                   [g (in-value (build-path pkg (format "~a.cql" name)))]
                   #:when (file-exists? g)
                   [lib (in-value (parse-file g))] #:when lib)
         lib))
     (or (for/first ([l (in-list others)] #:when (equal? (cql-library-version l) version)) l)
         (and (pair? others) (car others)))]))

(define (check-elm-corpus dir)
  (for*/list ([pkg (in-list (sort (map path->string (directory-list dir)) string<?))]
              #:when (directory-exists? (build-path dir pkg))
              [f (in-list (sort (directory-list (build-path dir pkg) #:build? #t) string<? #:key path->string))]
              #:when (regexp-match? #rx"[.]cql$" (path->string f)))
    (check-library pkg f (path-replace-extension f #".elm.json"))))

(define (corpus-libraries dir)
  (for*/list ([pkg (in-list (sort (map path->string (directory-list dir)) string<?))]
              #:when (directory-exists? (build-path dir pkg))
              [f (in-list (sort (directory-list (build-path dir pkg) #:build? #t) string<? #:key path->string))]
              #:when (regexp-match? #rx"[.]cql$" (path->string f))
              [lib (in-value (parse-file f))] #:when lib)
    (list pkg (path->string (path-replace-extension (file-name-from-path f) #"")) lib (package-resolver (build-path dir pkg)))))

(define (print-corpus-report rs label)
  (define (sum f) (apply + (map f rs)))
  (printf "  ~a: ~a libraries; ~a parse, ~a compile; ~a/~a reference definitions present, ~a ELM-equal\n"
          label (length rs) (count lib-result-parse-ok? rs) (count lib-result-compile-ok? rs)
          (sum lib-result-defs-present) (sum lib-result-defs-total) (sum lib-result-defs-equal))
  (for ([r (in-list rs)] #:when (lib-result-error r))
    (printf "    ~a: ~a\n" (lib-result-name r) (lib-result-error r)))
  (define (short s) (if (> (string-length s) 90) (string-append (substring s 0 90) "…") s))
  ;; class : (cons reference-node our-node) -> (listof (list library definition Difference)), in corpus order
  (define classes
    (sort (for*/fold ([h (hash)] #:result (for/list ([(k v) (in-hash h)]) (cons k (reverse v))))
                     ([r (in-list rs)] [m (in-list (lib-result-mismatches r))])
            (define d (cadr m))
            (hash-update h (cons (difference-reference d) (difference-ours d))
                         (lambda (l) (cons (list (lib-result-name r) (car m) d) l)) '()))
          (lambda (x y) (or (> (length (cdr x)) (length (cdr y)))
                            (and (= (length (cdr x)) (length (cdr y))) (string<? (format "~a" (car x)) (format "~a" (car y))))))))
  (printf "  difference classes (the nodes at the first difference: reference | ours), largest first:\n")
  (for ([c (in-list classes)])
    (printf "    ~a  ~a  |  ~a\n" (length (cdr c)) (short (car (car c))) (short (cdr (car c))))
    (for ([ex (in-list (take (cdr c) (min 3 (length (cdr c)))))])
      (printf "        ~a/~a~a\n" (car ex) (cadr ex)
              (let ([w (difference-what (caddr ex))]) (if (string=? w "") "" (string-append ": " (short w))))))))
