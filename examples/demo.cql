#lang cql
// demo.cql — a #lang cql module: compiles to ELM when required, registers itself as the Library
// "Demo" (fhir/registry), and prints its ELM JSON when run.
library Demo version '1.0'

define "Answer": 6 * 7
define "Greeting": 'hello' + ', ' + 'world'
