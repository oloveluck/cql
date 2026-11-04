#lang racket/base
;;; registry-test.rkt — a #lang cql module registers its Library with fhir/registry when required.
(require rackunit
         (only-in fhir/registry registered-resource registered-resources)
         (only-in cql/parser/ast cql-library-id)
         (prefix-in demo: "../examples/demo.cql"))
(define name (symbol->string (cql-library-id demo:library)))
(check-equal? (hash-ref demo:resource 'name) name)
(check-equal? (registered-resource "Library" (hash-ref demo:resource 'url)) demo:resource)
(check-true (and (member demo:resource (registered-resources "Library")) #t))
(check-equal? (hash-ref (car (hash-ref demo:resource 'content)) 'contentType) "text/cql")
