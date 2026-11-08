# cql — Clinical Quality Language

[CQL](https://cql.hl7.org/) compiled to ELM, the Expression Logical Model, and run over FHIR R4 data.
The translator uses the reference translator's own FHIR ModelInfo, so retrieves, includes,
terminology and the implicit FHIRHelpers conversions translate as the reference does. Measured:

| Check | Result |
|---|---|
| The official cqframework/cql-tests suite | 1820/1823 |
| 32 published libraries (FHIRHelpers, CQF Measures, the CQL IG) | 32/32 compile; 769/803 definitions identical to the reference translator's ELM |
| Four published eCQMs over their 76 test patients | 71/76 agree with the expected results (the 5 are named corpus problems) |

Part of the [fhir](https://github.com/oloveluck/fhir) family of FHIR languages and tools.

## Command line

`cql` is one of the family's command-line tools, which run without Racket:
[build them once](https://github.com/oloveluck/fhir#getting-the-tools) and put `dist/bin` on your
`PATH`. With Racket and the packages installed, `raco cql` runs the same commands.

```sh
cql adults.cql                                   # the library's ELM, as JSON
cql adults.cql --data patient.json               # each define's value for this patient
cql measure.cql --data patient-dir/ --valuesets valuesets/ --libraries cql/ --define "Numerator"
```

With `adults.cql`:

```cql
library Adults version '1.0'
using FHIR version '4.0.1'
context Patient
define "Female": Patient.gender.value = 'female'
define "Age": AgeInYears()
define "Adult Female": "Female" and "Age" >= 18
```

```
$ cql adults.cql --data patient.json
Female: true
Age: 51
Adult Female: true
```

- **`--data`** is one patient's record: a FHIR JSON resource, a Bundle, or a directory of them.
- **`--libraries`** is a directory of the `.cql` libraries the library includes, FHIRHelpers among them.
- **`--valuesets`** is a directory of ValueSet JSON with expansions. A retrieve against a value set
  that isn't there is an error, never an empty result.
- **`--define`** prints only the named definitions; repeat it for several.

The evaluation timezone can't be set from the command line yet: a DateTime written without an offset
is compared as it is, which can differ from an evaluator that gives it the request's offset.

Errors are positioned, translation errors at the `define` they occur in, and the exit status is 1:

```
$ cql broken.cql
broken.cql:2:1: cql compile error: in define "X": unknown identifier 'Foo'
 2 | define X: Foo + 1
   | ^^^^^^
```

## In Racket

`#lang cql` makes a `.cql` file a module. Requiring it compiles the library to ELM, and registers it
as a FHIR Library, so a Measure finds it by canonical. Nothing runs until you ask:

```racket
#lang racket/base
(require json "adults.cql")       ; adults.cql starts with `#lang cql`

(define patient (call-with-input-file "patient.json" read-json))
(evaluate (list patient))         ; '#hash(("Adult Female" . #t) ("Age" . 51) ("Female" . #t))
```

The module provides `library` (the CQL syntax tree), `elm` and `elm-json`, `(evaluate resources
#:parameters h #:valuesets vss)` and `resource` (the Library). `racket adults.cql` prints the ELM.

The pieces are libraries too:

```racket
(require cql/parser/parse cql/compile cql/interp cql/run cql/measure)
;; parse-cql, compile-library, eval-elm, load-runtime-library and rlib-value, evaluate-measure
```

`evaluate-measure` runs a Measure over a data source (a Bundle, a directory, or fhir-server's store)
and returns a MeasureReport. It is also the `$evaluate-measure` operation: `(apply-operation
"$evaluate-measure" measure #:periodStart "2021-01-01" #:periodEnd "2021-12-31")`.

## Develop

From source, with the family's repositories side by side:

```sh
raco pkg install --link --auto ../fhir/fhir ../fhir/fhir-spec ../fhirpath .
raco test .               # unit tests, the corpora and the evaluator differential
raco cql --tck [-v]       # the official cqframework/cql-tests (floor 1820)
racket tests/bench.rkt    # timings: the suite, an evaluation loop and the eCQMs, under both evaluators
```

Reference documentation: [oloveluck.github.io/cql](https://oloveluck.github.io/cql/). Architecture
notes: `CLAUDE.md`; the family's conventions and conformance table are in the fhir repository's
`CLAUDE.md`. License: MIT (vendored test corpora keep their own terms, see `tests/vendor`).
