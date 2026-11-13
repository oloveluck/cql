#lang racket/base

;;; requirements.rkt — what a translated library needs, read from its ELM: the artifacts it depends
;;; on and the data it retrieves, as the FHIR Library resource states them (`relatedArtifact` of
;;; type depends-on, `dataRequirement`) — what CRMI's computable-library profile carries and its
;;; dependency tracing reads (Library: relatedArtifact.resource, dataRequirement.profile,
;;; dataRequirement.codeFilter.valueSet).
;;;
;;;   an `include`          depends on that library (its canonical: the caller knows where libraries live)
;;;   a `codesystem`        depends on the code system (`url`, `url|version`)
;;;   a `valueset`          depends on the value set
;;;   a retrieve            a DataRequirement: the type, the profile retrieved (the ELM's templateId),
;;;                         and the code filter — the value set, or the code(s)
;;;
;;; Pure over the ELM as JSON (cql/elm/json), so it reads a translation from anywhere.

(require racket/contract racket/list racket/match racket/string json)

(provide
 (contract-out
  ;; library-requirements : elm-json (name version|#f -> canonical|#f) -> (values related data)
  ;; `library-canonical` gives an included library's canonical (#f: not listed)
  [library-requirements (-> hash? (-> string? (or/c string? #f) (or/c string? #f))
                            (values (listof hash?) (listof hash?)))]))

(define (defs lib key) (let ([section (hash-ref lib key #f)]) (if (hash? section) (hash-ref section 'def '()) '())))
(define (versioned url version) (if (and (string? version) (not (string=? version ""))) (string-append url "|" version) url))

(define (library-requirements elm library-canonical)
  (define lib (hash-ref elm 'library elm))
  (define code-systems (defs lib 'codeSystems))
  (define value-sets (defs lib 'valueSets))
  (define codes (defs lib 'codes))
  (define (named ds name) (findf (lambda (d) (equal? (hash-ref d 'name #f) name)) ds))
  (define (depends-on display resource) (hasheq 'type "depends-on" 'display display 'resource resource))
  (define related
    (append
     (for*/list ([i (in-list (defs lib 'includes))]
                 [c (in-value (library-canonical (hash-ref i 'path "") (hash-ref i 'version #f)))] #:when c)
       (depends-on (format "Library ~a" (hash-ref i 'path "")) c))
     (for/list ([cs (in-list code-systems)] #:when (string? (hash-ref cs 'id #f)))
       (depends-on (format "Code system ~a" (hash-ref cs 'name "")) (versioned (hash-ref cs 'id) (hash-ref cs 'version #f))))
     (for/list ([vs (in-list value-sets)] #:when (string? (hash-ref vs 'id #f)))
       (depends-on (format "Value set ~a" (hash-ref vs 'name "")) (versioned (hash-ref vs 'id) (hash-ref vs 'version #f))))))
  ;; code-filter : Retrieve -> (listof codeFilter)   by value set, or by the code(s) a CodeRef names
  (define (code-filter r)
    (define path (hash-ref r 'codeProperty #f))
    (define (coding ref)
      (define c (named codes (hash-ref ref 'name #f)))
      (define system (and c (named code-systems (hash-ref (hash-ref c 'codeSystem (hasheq)) 'name #f))))
      (and c (for/fold ([h (hasheq 'code (hash-ref c 'id ""))]) ([s (in-value (and system (hash-ref system 'id #f)))] #:when s)
               (hash-set h 'system s))))
    (match (hash-ref r 'codes #f)
      [(hash-table ('type "ValueSetRef") ('name name) _ ...)
       (define vs (named value-sets name))
       (if (and path vs) (list (hasheq 'path path 'valueSet (versioned (hash-ref vs 'id "") (hash-ref vs 'version #f)))) '())]
      [(hash-table ('type "ToList") ('operand (hash-table ('type "CodeRef") ('name name) _ ...)) _ ...)
       (define c (coding (hasheq 'name name)))
       (if (and path c) (list (hasheq 'path path 'code (list c))) '())]
      [(hash-table ('type "CodeRef") ('name name) _ ...)
       (define c (coding (hasheq 'name name)))
       (if (and path c) (list (hasheq 'path path 'code (list c))) '())]
      [_ '()]))
  (define (requirement r)
    (define type (last (regexp-split #rx"[}]" (hash-ref r 'dataType ""))))
    (for/fold ([dr (hasheq 'type type)])
              ([kv (in-list (list (cons 'profile (let ([t (hash-ref r 'templateId #f)]) (if (string? t) (list t) '())))
                                  (cons 'codeFilter (code-filter r))))]
               #:when (pair? (cdr kv)))
      (hash-set dr (car kv) (cdr kv))))
  (values related (remove-duplicates (map requirement (retrieves lib)))))

;; retrieves : elm-json -> (listof Retrieve)   every retrieve, in the order written (statements in
;; order; within a node, its keys by name — a hash has no order of its own)
(define (retrieves x)
  (cond [(hash? x)
         (append (if (equal? (hash-ref x 'type #f) "Retrieve") (list x) '())
                 (append-map (lambda (k) (retrieves (hash-ref x k)))
                             (sort (hash-keys x) symbol<?)))]
        [(list? x) (append-map retrieves x)]
        [else '()]))

(module+ test
  (require rackunit)
  (define elm
    (hasheq 'library
            (hasheq 'includes (hasheq 'def (list (hasheq 'localIdentifier "H" 'path "Helpers" 'version "1.0") (hasheq 'path "Unknown")))
                    'codeSystems (hasheq 'def (list (hasheq 'name "LOINC" 'id "http://loinc.org")))
                    'valueSets (hasheq 'def (list (hasheq 'name "Labs" 'id "http://x/ValueSet/labs" 'version "1.0.0")))
                    'codes (hasheq 'def (list (hasheq 'name "HbA1c" 'id "4548-4" 'codeSystem (hasheq 'name "LOINC"))))
                    'statements
                    (hasheq 'def (list (hasheq 'name "Tested"
                                               'expression (hasheq 'type "Exists"
                                                                   'operand (hasheq 'type "Retrieve" 'dataType "{http://hl7.org/fhir}Observation"
                                                                                    'templateId "http://hl7.org/fhir/StructureDefinition/Observation"
                                                                                    'codeProperty "code"
                                                                                    'codes (hasheq 'type "ToList" 'operand (hasheq 'type "CodeRef" 'name "HbA1c")))))
                                       (hasheq 'name "Any"
                                               'expression (hasheq 'type "Retrieve" 'dataType "{http://hl7.org/fhir}Observation" 'codeProperty "code"
                                                                   'codes (hasheq 'type "ValueSetRef" 'name "Labs")))
                                       (hasheq 'name "Again"
                                               'expression (hasheq 'type "Retrieve" 'dataType "{http://hl7.org/fhir}Observation" 'codeProperty "code"
                                                                   'codes (hasheq 'type "ValueSetRef" 'name "Labs")))
                                       (hasheq 'name "P" 'expression (hasheq 'type "Retrieve" 'dataType "{http://hl7.org/fhir}Patient")))))))
  (define-values (related data)
    (library-requirements elm (lambda (name version) (and (equal? name "Helpers") (format "http://x/Library/~a|~a" name version)))))
  (check-equal? related
                (list (hasheq 'type "depends-on" 'display "Library Helpers" 'resource "http://x/Library/Helpers|1.0")
                      (hasheq 'type "depends-on" 'display "Code system LOINC" 'resource "http://loinc.org")
                      (hasheq 'type "depends-on" 'display "Value set Labs" 'resource "http://x/ValueSet/labs|1.0.0")))
  (check-equal? data
                (list (hasheq 'type "Observation" 'profile '("http://hl7.org/fhir/StructureDefinition/Observation")
                              'codeFilter (list (hasheq 'path "code" 'code (list (hasheq 'system "http://loinc.org" 'code "4548-4")))))
                      (hasheq 'type "Observation" 'codeFilter (list (hasheq 'path "code" 'valueSet "http://x/ValueSet/labs|1.0.0")))
                      (hasheq 'type "Patient"))))
