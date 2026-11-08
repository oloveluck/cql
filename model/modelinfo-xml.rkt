#lang racket/base

;;; modelinfo-xml.rkt — any ModelInfo document (urn:hl7-org:elm-modelinfo:r1) as a ModelForm.
;;;
;;; A ModelInfo describes a data model to CQL: its classes and their elements, which classes are
;;; retrievable and by which code path, the contexts (Patient, Encounter, …) and how each class
;;; relates to them, the implicit conversions into System types, and the models it builds on. This
;;; module reads any such document — FHIR's, QI-Core's, QDM's, one written for a model of your own —
;;; into the s-expression modelinfo.rkt loads. Descriptions, bindings and search declarations are
;;; dropped: CQL translation and execution do not use them.
;;;
;;; A ModelForm is
;;;   (model name url version
;;;     (required (name version) …)                     requiredModelInfo, System left out
;;;     (patient class-name birth-date-path)
;;;     (types (type name base identifier label primary-code-path retrievable?
;;;                  ((element-name TypeSpec target) …)            target: a mapping expression | #f
;;;                  ((context related-key-element) …)               contextRelationship
;;;                  ((search-name path) …)) …)                     search declarations: a relationship
;;;                                                                  may name a search, not an element
;;;     (conversions (from-type to-type function-name) …)
;;;     (contexts (name key-element birth-date-element type-name) …))
;;; A TypeSpec is one of: "Ns.Name" | (list TypeSpec) | (interval TypeSpec) | (choice TypeSpec …)

(require racket/contract racket/match racket/list racket/port file/gunzip xml)

(provide (contract-out
          [read-modelinfo-xml (-> path-string? list?)]          ; a .xml or .xml.gz file -> ModelForm
          [modelinfo-string->form (-> string? list?)]))         ; the document's text -> ModelForm

(define (read-modelinfo-xml p)
  (define bs (call-with-input-file p
               (lambda (in)
                 (if (regexp-match? #rx"[.]gz$" (if (path? p) (path->string p) p))
                     (call-with-output-bytes (lambda (o) (gunzip-through-ports in o)))
                     (port->bytes in)))))
  (xexpr->form (bytes->xexpr bs)))
(define (modelinfo-string->form s) (xexpr->form (bytes->xexpr (string->bytes/utf-8 s))))
(define (bytes->xexpr bs)
  (parameterize ([collapse-whitespace #t] [xexpr-drop-empty-attributes #t])
    (xml->xexpr (document-element (read-xml (open-input-bytes bs))))))

;; attributes and children, with namespace prefixes on tags ignored (`ns4:typeInfo` = `typeInfo`)
(define (attr x name) (match x [(list* _ (? list? as) _) (let ([a (assq name as)]) (and a (cadr a)))] [_ #f]))
(define (children x) (match x [(list* _ (? list? _as) kids) (filter pair? kids)] [_ '()]))
(define (tag x) (and (pair? x) (let ([s (symbol->string (car x))]) (string->symbol (regexp-replace #rx"^.*:" s "")))))
(define (kids-tagged x t) (filter (lambda (c) (eq? (tag c) t)) (children x)))

(define (qualify ns name) (if (and ns name) (string-append ns "." name) name))
;; spec-of : TypeSpecifier xexpr -> TypeSpec
(define (spec-of x)
  (match (attr x 'xsi:type)
    [(or "NamedTypeSpecifier" "ns4:NamedTypeSpecifier") (qualify (or (attr x 'namespace) (attr x 'modelName)) (attr x 'name))]
    [(or "ListTypeSpecifier" "ns4:ListTypeSpecifier") (list 'list (or (attr x 'elementType) (spec-of (car (children x)))))]
    [(or "IntervalTypeSpecifier" "ns4:IntervalTypeSpecifier") (list 'interval (or (attr x 'pointType) (spec-of (car (children x)))))]
    [(or "ChoiceTypeSpecifier" "ns4:ChoiceTypeSpecifier")
     (cons 'choice (map spec-of (filter (lambda (c) (memq (tag c) '(choice typeSpecifier))) (children x))))]
    [_ (or (attr x 'name) "System.Any")]))
(define (element-type e)
  (or (attr e 'elementType) (attr e 'type)
      (let ([s (findf (lambda (c) (memq (tag c) '(elementTypeSpecifier typeSpecifier))) (children e))])
        (and s (spec-of s)))
      "System.Any"))

(define (type-form t)
  (list 'type (qualify (attr t 'namespace) (attr t 'name)) (attr t 'baseType) (attr t 'identifier) (attr t 'label)
        (attr t 'primaryCodePath) (equal? (attr t 'retrievable) "true")
        (for/list ([e (in-list (kids-tagged t 'element))])
          (list (attr e 'name) (element-type e) (attr e 'target)))
        (for/list ([r (in-list (kids-tagged t 'contextRelationship))])
          (list (attr r 'context) (attr r 'relatedKeyElement)))
        (for/list ([r (in-list (kids-tagged t 'search))] #:when (attr r 'path))
          (list (attr r 'name) (attr r 'path)))))

(define (xexpr->form mi)
  (unless (eq? (tag mi) 'modelInfo) (error 'modelinfo "not a ModelInfo document (its root is ~a)" (tag mi)))
  (define name (attr mi 'name))
  (list 'model name (attr mi 'url) (attr mi 'version)
        (cons 'required (for/list ([r (in-list (kids-tagged mi 'requiredModelInfo))] #:unless (equal? (attr r 'name) "System"))
                          (list (attr r 'name) (attr r 'version))))
        (list 'patient (attr mi 'patientClassName) (attr mi 'patientBirthDatePropertyName))
        (cons 'types (for/list ([t (in-list (kids-tagged mi 'typeInfo))]
                                #:when (member (attr t 'xsi:type) '("ClassInfo" "ns4:ClassInfo" "ProfileInfo" "ns4:ProfileInfo")))
                       (type-form t)))
        (cons 'conversions (for/list ([c (in-list (kids-tagged mi 'conversionInfo))])
                             (list (attr c 'fromType) (attr c 'toType) (attr c 'functionName))))
        (cons 'contexts (for/list ([c (in-list (kids-tagged mi 'contextInfo))])
                          (define ct (findf (lambda (k) (eq? (tag k) 'contextType)) (children c)))
                          (list (attr c 'name) (attr c 'keyElement) (attr c 'birthDateElement)
                                (and ct (qualify (or (attr ct 'namespace) (attr ct 'modelName)) (attr ct 'name))))))))

(module+ test
  (require rackunit)
  ;; a model nobody designed for: two classes, a choice, a relationship to the patient
  (define f (modelinfo-string->form #<<XML
<?xml version="1.0" encoding="UTF-8"?>
<modelInfo xmlns="urn:hl7-org:elm-modelinfo:r1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
           name="Clinic" version="1.0" url="http://example.org/clinic" patientClassName="Clinic.Person"
           patientBirthDatePropertyName="born">
  <requiredModelInfo name="System" version="1.0.0"/>
  <typeInfo xsi:type="ClassInfo" namespace="Clinic" name="Person" retrievable="true" label="Person">
    <element name="id" elementType="System.String"/>
    <element name="born" elementType="System.Date"/>
  </typeInfo>
  <typeInfo xsi:type="ClassInfo" namespace="Clinic" name="Visit" retrievable="true" primaryCodePath="reason">
    <element name="reason" elementType="System.Code"/>
    <element name="when">
      <elementTypeSpecifier xsi:type="ChoiceTypeSpecifier">
        <choice xsi:type="NamedTypeSpecifier" namespace="System" name="DateTime"/>
        <choice xsi:type="IntervalTypeSpecifier"><pointType xsi:type="NamedTypeSpecifier" namespace="System" name="DateTime"/></choice>
      </elementTypeSpecifier>
    </element>
    <element name="tags"><elementTypeSpecifier xsi:type="ListTypeSpecifier" elementType="System.String"/></element>
    <contextRelationship context="Patient" relatedKeyElement="person"/>
  </typeInfo>
  <contextInfo name="Patient" keyElement="id" birthDateElement="born"><contextType namespace="Clinic" name="Person"/></contextInfo>
</modelInfo>
XML
))
  (check-equal? (list-ref f 1) "Clinic")
  (check-equal? (list-ref f 4) '(required))
  (check-equal? (list-ref f 5) '(patient "Clinic.Person" "born"))
  (define visit (assoc "Clinic.Visit" (map cdr (cdr (list-ref f 6)))))
  (check-equal? (list-ref visit 4) "reason")
  (check-equal? (list-ref visit 6) '(("reason" "System.Code" #f) ("when" (choice "System.DateTime" (interval "System.DateTime")) #f)
                                     ("tags" (list "System.String") #f)))
  (check-equal? (list-ref visit 7) '(("Patient" "person")))
  (check-equal? (list-ref visit 8) '())
  (check-equal? (list-ref f 8) '(contexts ("Patient" "id" "born" "Clinic.Person")))
  (check-exn #rx"not a ModelInfo" (lambda () (modelinfo-string->form "<other/>"))))
