#lang racket/base

;;; library.rkt — a CQL source as a FHIR Library resource.
;;;
;;; A `.cql` file becomes a Library the way a measure repository holds one: id and name the CQL
;;; library's name, version from its header, `type` logic-library, the source as base64 text/cql
;;; content, url <base>/Library/<name>. Measure evaluation finds a Measure's library by url, or by
;;; the canonical's last segment = the library name, so a locally held library serves a published
;;; Measure. `#lang cql` modules register theirs (fhir/registry); fhir-server's loader stores them.

(require racket/contract racket/string net/base64)

(provide (contract-out
          ;; the Library of a CQL source, or #f when it declares no `library Name`
          [library-resource (->* (string?) (#:library-base string?) (or/c hash? #f))]
          ;; the same, an error when there is no header
          [cql->library (->* (string?) (#:library-base string?) hash?)]
          [default-library-base string?]))

(define default-library-base "http://localhost/fhir")

;; library-header : String -> (values name version) | (values #f #f)
(define (library-header src)
  (define m (regexp-match #px"(?m:^\\s*library\\s+(\"[^\"]+\"|\\S+)(?:\\s+version\\s+'([^']+)')?)" src))
  (if m (values (string-trim (cadr m) "\"") (caddr m)) (values #f #f)))

(define (library-resource src #:library-base [base default-library-base])
  (define-values (name version) (library-header src))
  (and name
       (let ([lib (hasheq 'resourceType "Library" 'id (regexp-replace* #px"[^A-Za-z0-9.-]" name "-") 'name name
                          'status "active" 'url (string-append base "/Library/" name)
                          'type (hasheq 'coding (list (hasheq 'system "http://terminology.hl7.org/CodeSystem/library-type"
                                                              'code "logic-library")))
                          'content (list (hasheq 'contentType "text/cql"
                                                 'data (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 src) #"")))))])
         (if version (hash-set lib 'version version) lib))))

(define (cql->library src #:library-base [base default-library-base])
  (or (library-resource src #:library-base base)
      (raise-user-error 'cql "a CQL library needs a `library Name` header")))

(module+ test
  (require rackunit)
  (define lib (cql->library "library Screening version '1.0'\ndefine X: 1"))
  (check-equal? (hash-ref lib 'name) "Screening")
  (check-equal? (hash-ref lib 'version) "1.0")
  (check-equal? (hash-ref lib 'url) "http://localhost/fhir/Library/Screening")
  (check-equal? (hash-ref (cql->library "library \"My Lib\"\n" #:library-base "http://x") 'id) "My-Lib")
  (check-false (library-resource "define X: 1"))
  (check-exn #rx"library Name" (lambda () (cql->library "define X: 1"))))
