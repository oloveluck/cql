# fhir-cql

Clinical Quality Language for Racket: CQL parser, CQL-to-ELM compiler, ELM interpreter and #lang fhir/cql.

Part of **fhir — FHIR as a family of Racket `#lang`s** (main repo: `oloveluck/fhir`). This package
contributes `cql/` to the shared `fhir` collection. `#lang fhir/cql`.

## Depends on

`fhir-core`, `fhir-fhirpath`
(Racket packages; link them first) plus `brag-lib`, `br-parser-tools-lib`.

## Develop

```sh
raco pkg install --link --auto .     # link into the `fhir` collection
raco test cql/
```

The `raco fhir` CLI and the conformance-suite runners are driven from the main `fhir` package:

```
raco fhir cql <file.cql>, raco fhir cql-tck   (ELM JSON; the cqframework/cql-tests suite)
```

Architecture notes: `CLAUDE.md` here; conventions and the ratchet table: the main repo's `CLAUDE.md`.
