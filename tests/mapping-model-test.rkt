#lang racket/base

;;; mapping-model-test.rkt — libraries written against a MAPPING model (QI-Core, US Core: a ModelInfo
;;; with a targetUrl, a view over FHIR) compiled to ELM over FHIR and run over FHIR JSON.
;;;   - a retrieve names the FHIR class, the profile is its templateId
;;;   - an element with a `target` mapping reads through it (FHIRHelpers.ToConcept(value), an
;;;     extension filter, …)
;;;   - a code element that maps a ValueSet as well (QI-Core 4.1.1's NotDone profiles) retrieves both
;;;     ways: codes `in` the value set, and records whose mapped value set `~` it (the value set the
;;;     action was not done for) — read at run time from the profile's mapping, found by templateId
;;;   - `convert X to days` is ConvertQuantity(X, 'd'); a System Quantity has its value and unit
;;; The corpora are qicore-elm-corpus-test.rkt (98 libraries vs the reference ELM) and
;;; qicore-measure-test.rkt (4 eCQMs over 190 test cases).

(require rackunit racket/runtime-path racket/file json
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library current-library-resolver)
         (only-in cql/elm/json elm->jsexpr)
         cql/run
         (only-in fhir/result/result ok? ok-value))

(define-runtime-path libdir "vendor/qicore-2024/elm-corpus/ecqm-content-qicore-2024")
(define (res n _v) (let ([f (build-path libdir (format "~a.cql" n))]) (and (file-exists? f) (ok-value (parse-cql (file->string f))))))
(define (lib text) (ok-value (parse-cql text)))
(define (statement elm name)
  (for/first ([s (in-list (hash-ref (hash-ref (hash-ref (elm->jsexpr elm) 'library) 'statements) 'def))]
              #:when (equal? (hash-ref s 'name) name))
    (hash-ref s 'expression)))
(define (run text resources valuesets)
  (load-runtime-library (lib text) #:resolver res #:data (resources->data resources)
                        #:valuesets (valuesets->terminology valuesets)))

(define vs (hasheq 'resourceType "ValueSet" 'url "http://example.org/vs/bp"
                   'expansion (hasheq 'contains (list (hasheq 'system "http://loinc.org" 'code "8480-6")))))
(define (obs id code-element [extra (hasheq)])
  (for/fold ([h (hasheq 'resourceType "Observation" 'id id 'status "final" 'subject (hasheq 'reference "Patient/p")
                        'code code-element)])
            ([(k v) (in-hash extra)])
    (hash-set h k v)))
(define not-done-url "http://hl7.org/fhir/us/qicore/StructureDefinition/qicore-notDoneValueSet")
(define data
  (list (hasheq 'resourceType "Patient" 'id "p")
        (obs "in-vs" (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "8480-6")))
             (hasheq 'valueCodeableConcept (hasheq 'coding (list (hasheq 'system "http://snomed.info/sct" 'code "1")))))
        (obs "not-done" (hasheq 'extension (list (hasheq 'url not-done-url 'valueCanonical "http://example.org/vs/bp")))
             (hasheq 'status "cancelled"))
        (obs "not-done-other" (hasheq 'extension (list (hasheq 'url not-done-url 'valueCanonical "http://example.org/vs/other")))
             (hasheq 'status "cancelled"))
        (hasheq 'resourceType "MedicationRequest" 'id "m" 'status "active" 'intent "order" 'subject (hasheq 'reference "Patient/p")
                'dispenseRequest (hasheq 'expectedSupplyDuration (hasheq 'value 90 'system "http://unitsofmeasure.org" 'code "days")))))

(define qicore
  (string-append "library Q version '1'\nusing QICore version '4.1.1'\ninclude FHIRHelpers version '4.4.000'\n"
                 "valueset \"BP\": 'http://example.org/vs/bp'\ncontext Patient\n"
                 "define NotDone: [\"ObservationNotDone\": \"BP\"] O return O.id\n"
                 "define Done: [Observation: \"BP\"] O return O.value\n"
                 "define Days: [MedicationRequest] M return (convert M.dispenseRequest.expectedSupplyDuration to days).value\n"
                 "define Unit: (convert 5 'mg' to 'g').unit\n"))

(test-case "a QI-Core library compiles to ELM over FHIR"
  (define elm (parameterize ([current-library-resolver res]) (ok-value (compile-library (lib qicore)))))
  (define nd (statement elm "NotDone"))
  ;; the NotDone retrieve: a union of `in` and `~` against the value set
  (define src (hash-ref (car (hash-ref nd 'source)) 'expression))
  (check-equal? (hash-ref src 'type) "Union")
  (check-equal? (for/list ([r (hash-ref src 'operand)]) (list (hash-ref r 'dataType) (hash-ref r 'codeComparator)))
                '(("{http://hl7.org/fhir}Observation" "in") ("{http://hl7.org/fhir}Observation" "~")))
  (check-equal? (hash-ref (car (hash-ref src 'operand)) 'templateId)
                "http://hl7.org/fhir/us/qicore/StructureDefinition/qicore-observationnotdone")
  ;; convert … to days: ConvertQuantity with the UCUM unit
  (define days (hash-ref (hash-ref (statement elm "Days") 'return) 'expression))
  (check-equal? (hash-ref (hash-ref days 'source) 'type) "ConvertQuantity")
  (check-equal? (hash-ref (cadr (hash-ref (hash-ref days 'source) 'operand)) 'value) "d"))

(test-case "a QI-Core library runs over FHIR JSON"
  (define rl (run qicore data (list vs)))
  ;; in the value set by code, or not done for it — not a record not done for another value set
  (check-equal? (rlib-value rl "NotDone") '("in-vs" "not-done"))
  ;; a plain retrieve matches by code only; its value reads through ToValue
  (check-equal? (format "~a" (rlib-value rl "Done")) "(#(struct:cql-concept (#(struct:cql-code 1 http://snomed.info/sct #f #f)) #f))")
  (check-equal? (format "~a" (rlib-value rl "Days")) "(#(struct:fp-decimal 90 0))")
  (check-equal? (rlib-value rl "Unit") "g"))

(test-case "a US Core library: profiles by their class names, elements through their mappings"
  (define text (string-append "library U version '1'\nusing USCore version '3.1.1'\ninclude FHIRHelpers version '4.4.000'\n"
                              "context Patient\n"
                              "define Smoking: [SmokingStatusProfile] S return S.value\n"))
  (define elm (parameterize ([current-library-resolver res]) (ok-value (compile-library (lib text)))))
  (define src (hash-ref (car (hash-ref (statement elm "Smoking") 'source)) 'expression))
  (check-equal? (hash-ref src 'dataType) "{http://hl7.org/fhir}Observation")
  (check-equal? (hash-ref src 'templateId) "http://hl7.org/fhir/us/core/StructureDefinition/us-core-smokingstatus")
  (define rl (run text (list (hasheq 'resourceType "Patient" 'id "p")
                             (obs "s" (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "72166-2")))
                                  (hasheq 'valueCodeableConcept (hasheq 'coding (list (hasheq 'system "http://snomed.info/sct" 'code "449868002"))))))
                  '()))
  (check-equal? (format "~a" (rlib-value rl "Smoking"))
                "(#(struct:cql-concept (#(struct:cql-code 449868002 http://snomed.info/sct #f #f)) #f))"))
