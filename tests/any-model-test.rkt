#lang racket/base

;;; any-model-test.rkt — CQL over a data model nobody designed the engine for.
;;;
;;; A clinic's model, made from two logical models (a Member, a Visit with a coded reason, a choice
;;; of time or period, tags, parts, a reference to its member), data in that model's own JSON, and a
;;; library that uses it the ways measures use FHIR: retrieves with and without a value set, the
;;; Patient context through the model's relationship, an age, a choice element, lists, nested
;;; classes, types. Nothing in the engine knows this model; it is a ModelInfo and a representation.
;;; Also: a ModelInfo document in a model directory is found by `using`, with its representation
;;; declared beside it.

(require rackunit racket/list racket/file
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library)
         (only-in cql/model/modelinfo current-model-providers current-model-directories find-model model-name)
         (only-in cql/model/structure-definitions structure-definitions->model)
         (only-in cql/run load-runtime-library resources->data valuesets->terminology rlib-value)
         (only-in cql/value cql-null? cql-interval? cql-code cql-concept? model-node? model-node-data)
         (only-in fhir/result/result ok? ok-value err-error))

;; ---- the model: what FSH's `Logical:` compiles to ----------------------------------------------
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
                                (el "Visit.member" 'max "1" 'type (list (hasheq 'code "Reference" 'targetProfile '("http://example.org/clinic/StructureDefinition/Member"))))
                                (el "Visit.tag" 'max "*" 'type (list (hasheq 'code "string")))
                                (el "Visit.part" 'max "*" 'type (list (hasheq 'code "BackboneElement")))
                                (el "Visit.part.kind" 'max "1" 'type (list (hasheq 'code "code"))))))
(define clinic (structure-definitions->model (list member visit) #:name "Clinic" #:version "1.0"
                                             #:url "http://example.org/clinic" #:patient "Member" #:birth-date "born"))
(define (provide-clinic n v) (and (equal? n "Clinic") clinic))

;; ---- the data, in the model's own shape -------------------------------------------------------
(define (mem id born) (hasheq 'resourceType "Member" 'id id 'born born))
(define (vis id who code when #:tags [tags '()] #:parts [parts '()])
  (hasheq 'resourceType "Visit" 'id id 'member (hasheq 'reference (string-append "Member/" who))
          'reason (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code code)) 'text "why")
          (if (string? when) 'whenDateTime 'whenPeriod) when
          'tag tags 'part (for/list ([k parts]) (hasheq 'kind k))))
(define members (list (mem "m1" "1960-05-01") (mem "m2" "1990-01-01") (mem "kid" "2020-01-01")))
(define visits (list (vis "v1" "m1" "4548-4" "2025-03-01T09:00:00Z" #:tags '("urgent") #:parts '("lab" "consult"))
                     (vis "v2" "m2" "1234-5" (hasheq 'start "2025-04-01T09:00:00Z" 'end "2025-04-01T10:30:00Z"))
                     (vis "v3" "kid" "4548-4" "2025-02-01")))
(define hba1c (hasheq 'resourceType "ValueSet" 'url "http://example.org/vs/hba1c"
                      'compose (hasheq 'include (list (hasheq 'system "http://loinc.org" 'concept (list (hasheq 'code "4548-4")))))))

(define library-text #<<CQL
library ClinicGap version '1'
using Clinic version '1.0'
valueset "HbA1c": 'http://example.org/vs/hba1c'
context Patient
define "Visits": [Visit]
define "HbA1c Visits": [Visit: "HbA1c"]
define "Adult": AgeInYearsAt(@2025-06-01) >= 18
define "Gap": "Adult" and not exists "HbA1c Visits"
define "Urgent": exists ("Visits" V where 'urgent' in V.tag)
define "Part Kinds": flatten ("Visits" V return (V.part P return P.kind))
define "Timed": "Visits" V where V."when" is DateTime return V."when"
define "Spans": "Visits" V where V."when" is Interval<DateTime> return V."when"
define "Reasons": "Visits" V return V.reason
define "Born": Patient.born
CQL
  )

;; evaluate : member-id -> (hash define -> value)   the library over that member's data
(define lib (let ([r (parse-cql library-text)]) (if (ok? r) (ok-value r) (error 'test "~a" (err-error r)))))
(define (evaluate pid)
  (parameterize ([current-model-providers (list provide-clinic)])
    (define data (resources->data (append (filter (lambda (m) (equal? (hash-ref m 'id) pid)) members) visits)))
    (define rl (load-runtime-library lib #:data data #:valuesets (valuesets->terminology (list hba1c))))
    (for/hash ([d '("Visits" "HbA1c Visits" "Adult" "Gap" "Urgent" "Part Kinds" "Timed" "Spans" "Reasons" "Born")])
      (values d (rlib-value rl d)))))

(parameterize ([current-model-providers (list provide-clinic)])
  (check-true (ok? (compile-library lib)) "the library compiles against the generated model"))

(define m1 (evaluate "m1"))
(define m2 (evaluate "m2"))
(define kid (evaluate "kid"))
(define (ids vs) (map (lambda (v) (hash-ref (model-node-data v) 'id)) vs))
;; the Patient context follows the model's relationship: each member sees their own visits
(check-equal? (ids (hash-ref m1 "Visits")) '("v1"))
(check-equal? (ids (hash-ref m2 "Visits")) '("v2"))
;; a retrieve by value set reads the model's code path (`reason`, a Concept)
(check-equal? (ids (hash-ref m1 "HbA1c Visits")) '("v1"))
(check-equal? (hash-ref m2 "HbA1c Visits") '())
(check-equal? (ids (hash-ref kid "HbA1c Visits")) '("v3"))
;; an age from the model's birth date; logic over it
(check-equal? (map (lambda (r) (hash-ref r "Adult")) (list m1 m2 kid)) '(#t #t #f))
(check-equal? (map (lambda (r) (hash-ref r "Gap")) (list m1 m2 kid)) '(#f #t #f))
;; lists, nested classes, a choice read by its member's type
(check-true (hash-ref m1 "Urgent")) (check-false (hash-ref m2 "Urgent"))
(check-equal? (hash-ref m1 "Part Kinds") '("lab" "consult"))
(check-equal? (length (hash-ref m1 "Timed")) 1)
(check-equal? (hash-ref m1 "Spans") '())
(check-true (cql-interval? (car (hash-ref m2 "Spans"))))
(check-true (cql-concept? (car (hash-ref m1 "Reasons"))))
(check-false (cql-null? (hash-ref m1 "Born")))

;; ---- a ModelInfo document in a model directory, its representation beside it ------------------
(define dir (make-temporary-directory "models-~a"))
(with-output-to-file (build-path dir "clinic-modelinfo-2.0.xml") (lambda () (display #<<XML
<?xml version="1.0" encoding="UTF-8"?>
<modelInfo xmlns="urn:hl7-org:elm-modelinfo:r1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           name="Clinic2" version="2.0" url="http://example.org/clinic2" patientClassName="Clinic2.Person"
           patientBirthDatePropertyName="born">
  <typeInfo xsi:type="ClassInfo" namespace="Clinic2" name="Person" retrievable="true">
    <element name="key" elementType="System.String"/>
    <element name="born" elementType="System.Date"/>
  </typeInfo>
  <typeInfo xsi:type="ClassInfo" namespace="Clinic2" name="Visit" retrievable="true" primaryCodePath="reason">
    <element name="reason" elementType="System.Code"/>
    <element name="person" elementType="System.String"/>
    <contextRelationship context="Patient" relatedKeyElement="person"/>
  </typeInfo>
  <contextInfo name="Patient" keyElement="key" birthDateElement="born"><contextType namespace="Clinic2" name="Person"/></contextInfo>
</modelInfo>
XML
)))
(with-output-to-file (build-path dir "clinic-modelinfo-2.0.representation.rktd")
  (lambda () (write '((type-key . kind) (choice . plain) (reference . key)))))
(parameterize ([current-model-directories (list dir)])
  (check-equal? (model-name (find-model "Clinic2" "2.0")) "Clinic2")
  (define lib2 (ok-value (parse-cql (string-append "library L2\nusing Clinic2 version '2.0'\ncodesystem \"S\": 'http://s'\ncode \"A\": 'a' from \"S\"\n"
                                                   "context Patient\ndefine \"A Visits\": [Visit: \"A\"]\ndefine \"Old\": AgeInYearsAt(@2025-01-01) > 50\n"))))
  (define data (resources->data (list (hasheq 'kind "Person" 'key "p1" 'born "1950-01-01")
                                      (hasheq 'kind "Visit" 'person "p1" 'reason (hasheq 'system "http://s" 'code "a"))
                                      (hasheq 'kind "Visit" 'person "p2" 'reason (hasheq 'system "http://s" 'code "a"))
                                      (hasheq 'kind "Visit" 'person "p1" 'reason (hasheq 'system "http://s" 'code "b")))))
  (define rl (load-runtime-library lib2 #:data data))
  (check-equal? (length (rlib-value rl "A Visits")) 1)        ; p1's visit coded a, not p2's, not the b one
  (check-true (rlib-value rl "Old")))
(delete-directory/files dir)
