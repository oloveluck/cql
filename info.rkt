#lang info

(define collection "cql")
(define version "0.3")
(define pkg-desc "Clinical Quality Language: CQL parser, CQL-to-ELM compiler, ELM interpreter and measure evaluation; the cql command-line tool and #lang cql")
(define pkg-authors '(oloveluck))
(define license 'MIT)
(define deps '("base" "fhir" "fhirpath" "brag-lib" "br-parser-tools-lib"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/cql.scrbl" ())))
(define raco-commands '(("cql" (submod cql/cli main) "compile CQL to ELM; run the official CQL conformance suite" #f)))
(define test-omit-paths '("tests/vendor" "scribblings"))
