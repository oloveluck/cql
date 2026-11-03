#lang racket/base

;;; measure-operation-test.rkt — Measure/$evaluate-measure with no server: the measure, its CQL (as
;;; Library resources), the value sets and one test patient's record form an in-memory data source,
;;; and the operation is applied by its canonical (fhir/operation, cql/measure). The populations must
;;; equal what the eCQM corpus harness expects for the case (measure-test.rkt).

(require rackunit racket/file racket/path racket/runtime-path racket/list json net/base64
         fhir/operation fhir/data-source
         (only-in cql/measure $evaluate-measure))

(define-runtime-path root "vendor/ecqm")

(define (json-files dir)
  (for/list ([f (in-directory dir)] #:when (regexp-match? #rx"[.]json$" (path->string f)))
    (call-with-input-file f read-json)))
;; a .cql file as the Library resource a server would hold
(define (cql-library f)
  (define name (path->string (path-replace-extension (file-name-from-path f) #"")))
  (hasheq 'resourceType "Library" 'id name 'name name
          'content (list (hasheq 'contentType "text/cql"
                                 'data (bytes->string/utf-8 (base64-encode (file->bytes f) #""))))))

(define m "ColorectalCancerScreeningsFHIR")
(define case-dir (build-path root "tests" m "numer-EXM130"))
(define measure (call-with-input-file (build-path root "measure" (string-append m ".json")) read-json))
(define data
  (resources->data-source
   (append (list measure)
           (map cql-library (filter (lambda (f) (regexp-match? #rx"[.]cql$" (path->string f)))
                                    (directory-list (build-path root "cql") #:build? #t)))
           (json-files (build-path root "valueset"))
           (json-files case-dir))))

(define (populations report)
  (for*/hash ([g (in-list (hash-ref report 'group))] [p (in-list (hash-ref g 'population))])
    (values (hash-ref (car (hash-ref (hash-ref p 'code) 'coding)) 'code) (hash-ref p 'count))))

(when (directory-exists? case-dir)
  (parameterize ([current-data-source data])
    (define pid (hash-ref (car ((data-source-all data) "Patient")) 'id))
    ;; by reference, with the Measure by id; the period at the corpus's -07:00
    (define report (apply-operation "http://hl7.org/fhir/OperationDefinition/Measure-evaluate-measure"
                                    #:measure (hash-ref measure 'id)
                                    #:periodStart "2021-01-01T00:00:00-07:00" #:periodEnd "2021-12-31"
                                    #:subject (string-append "Patient/" pid)))
    (check-equal? (hash-ref report 'resourceType) "MeasureReport")
    (check-equal? (hash-ref report 'type) "individual")
    (check-equal? (hash-ref (populations report) "numerator") 1)
    (check-equal? (hash-ref (populations report) "denominator") 1)
    ;; piped: the Measure itself is the subject
    (check-equal? (populations ($evaluate-measure measure #:periodStart "2021-01-01T00:00:00-07:00" #:periodEnd "2021-12-31"))
                  (populations report))
    (check-exn #rx"needs input 'periodStart'" (lambda () ($evaluate-measure measure #:periodEnd "2021-12-31")))))
