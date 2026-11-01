#lang s-exp syntax/module-reader

fhir/cql/base

#:read        cql-read
#:read-syntax cql-read-syntax
#:whole-body-readers? #t

;; The reader for #lang fhir/cql. A CQL source file is ONE library: parse the whole body with
;; parse-cql and emit a single (cql-library-form <prefab-ast>) form into the fhir/cql/base module
;; language, which compiles it to ELM. A parse failure raises with the CQL error message.

(require (only-in fhir/cql/parser/parse parse-cql)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in fhir/cql/error cql-error-string)
         (only-in racket/port port->string))

(define (cql-read-syntax src in)
  (define r (parse-cql (port->string in)))
  (if (ok? r)
      (list (datum->syntax #f (list 'cql-library-form (ok-value r))))
      (error 'cql (cql-error-string (err-error r)))))

(define (cql-read in)
  (map syntax->datum (cql-read-syntax #f in)))
