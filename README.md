# cql — Clinical Quality Language for Racket

CQL (cql.hl7.org) compiled to ELM and run by an ELM interpreter: the expression + definition core with System-typed values, ~150 operators, intervals, queries and the cqframework/cql-tests suite at 1820/1823. Libraries over FHIR R4 translate with the reference translator's ModelInfo: retrieves, includes, terminology definitions, implicit FHIRHelpers conversions — 32/32 published libraries compile, 742/803 definitions ELM-identical to the reference translator. Executing retrieves against data is the next slice.

Part of **fhir — FHIR as a family of Racket `#lang`s** ([oloveluck/fhir](https://github.com/oloveluck/fhir)).
Collection `cql`. `#lang cql` — a `.cql` file is a module; `racket file` prints the compiled JSON.

## Install

```sh
raco pkg install cql          # once published on pkgs.racket-lang.org
# from source, with the family repos side by side:
raco pkg install --link --auto ../fhir/fhir .
```

Depends on `fhir`, `fhirpath` (System value carriers: decimals, quantities, temporals).

## Use

```racket
(require cql/parser/parse cql/compile cql/interp cql/elm/json)   ; parse-cql, compile-library, run-library, elm->json-string
```

```sh
raco cql library.cql     # ELM JSON
raco cql --tck [-v]      # official cqframework/cql-tests (floor 1820)
```

## Develop

```sh
raco test .          # unit tests (the conformance runners are explicit: see the --tck commands above)
```

Architecture notes: `CLAUDE.md`. Conventions, design style and the family-wide conformance table: the
main repo's `CLAUDE.md`. License: MIT (vendored test corpora keep their own terms — see `tests/vendor`).
