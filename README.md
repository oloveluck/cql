# cql — Clinical Quality Language

[CQL](https://cql.hl7.org/) compiled to ELM and run over FHIR R4 data, including quality measures, or
over any data model you describe.
It passes 1820 of the 1823 tests in HL7's CQL suite; [TESTING.md](TESTING.md) has the details.

Part of the [fhir](https://github.com/oloveluck/fhir) family of FHIR languages and tools.

## Command line

[Build the family's tools](https://github.com/oloveluck/fhir#getting-the-tools) once and put
`dist/bin` on your `PATH`; they run without Racket. With Racket installed, `raco cql` is the same.

```sh
cql adults.cql                                   # the library's ELM, as JSON
cql adults.cql --data patient.json               # each define's value for this patient
cql measure.cql --data patient/ --valuesets valuesets/ --libraries cql/ --define "Numerator"
```

```
$ cql adults.cql --data patient.json
Female: true
Age: 51
Adult Female: true
```

`--data` is one patient's record (a resource, a Bundle or a directory). `--libraries` holds the
libraries it includes, such as FHIRHelpers, and `--valuesets` the expanded value sets. Errors point at
the line, with exit status 1.

## Any data model

FHIR 4.0.1 is installed. Any other model is data, not code:
- a **ModelInfo** document, the format the reference CQL translator uses;
- and a few lines saying **how its data looks**: which key names a record's type, how choice elements
  and references are written.

Put both in a directory named by `CQL_MODEL_PATH`, and `using YourModel version '1.0'` finds them.
You can also design the model in FSH as logical models: the fhir-mcp workspace turns them into a
model, and their datatypes become CQL's own types, so no FHIRHelpers is needed. The engine names no
model; a test fails if it ever does.

## In Racket

A `.cql` file that starts with `#lang cql` is a module:

```racket
#lang racket/base
(require json "adults.cql")
(evaluate (list (call-with-input-file "patient.json" read-json)))
;; '#hash(("Adult Female" . #t) ("Age" . 51) ("Female" . #t))
```

It also provides `elm` and `elm-json`. `cql/measure`'s `evaluate-measure` runs a Measure and returns a
MeasureReport, and is the `$evaluate-measure` operation.

## Develop

```sh
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec ../fhirpath .   # repos side by side
raco test .
```

Reference: [oloveluck.github.io/cql](https://oloveluck.github.io/cql/). Architecture: `CLAUDE.md`.
License: MIT.
