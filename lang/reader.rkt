#lang s-exp syntax/module-reader

cql/base

#:read        cql-read
#:read-syntax cql-read-syntax
#:whole-body-readers? #t

;; The reader for #lang cql. A CQL source file is ONE library: parse the whole body with
;; parse-cql and emit a single (cql-library-form <prefab-ast> <source-text>) form into the cql/base
;; module language, which compiles it to ELM and registers it as a Library (the text is its
;; content). A parse failure raises with the CQL error message.

(require (only-in cql/parser/parse parse-cql)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in cql/error cql-error-string cql-error-diagnostic)
         (only-in fhir/diagnostic raise-diagnostic)
         (only-in racket/port port->string))

(define (source-name src) (cond [(path? src) (path->string src)] [(string? src) src] [else #f]))
;; a parse error raises exn:fail:diagnostic positioned in this file (fhir/diagnostic)
(define (cql-read-syntax src in)
  (define text (port->string in))
  (define r (parse-cql text #:source (source-name src)))
  (if (ok? r)
      (list (datum->syntax #f (list 'cql-library-form (ok-value r) text)))
      (let ([d (cql-error-diagnostic (err-error r))])
        (if d (raise-diagnostic d) (error 'cql (cql-error-string (err-error r)))))))

(define (cql-read in)
  (map syntax->datum (cql-read-syntax #f in)))
