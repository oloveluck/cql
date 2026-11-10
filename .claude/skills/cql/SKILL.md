---
name: cql
description: Write, check and run Clinical Quality Language (CQL) libraries without an MCP server — `raco cql` (translate to ELM, run over a patient's FHIR or QDM JSON), `#lang cql` modules, measures (FHIR Measures, cqm-models measure documents), and the data models (FHIR, QI-Core, US Core, QDM, your own). Use when writing or debugging a .cql file, evaluating CQL over patient data, or computing measure populations.
---

# CQL without MCP

Read [`guide.md`](../../../guide.md) first: the language in brief, a checked cheat sheet and worked
example, the data models, and the common problems (that guide is what fhir-mcp serves as
`learn://cql`). This skill is the workflow around it: the commands, the loop, and the Racket API.

`cql` and `raco cql` are the same tool (the standalone build is `dist/bin/cql`).

## The loop
1. **Translate.** `raco cql lib.cql` prints the ELM and exits 0, or lists every translation error,
   each at its line and naming its definition, and exits 1:
   ```
   gap.cql:9:1: cql compile error: in define "Typo": unknown function 'Cuont'
   ```
   `--format json` puts the same diagnostics on stdout as JSON (`line`, `column`, `message`,
   `phase`); `--format outcome` as a FHIR OperationOutcome.
2. **Run over one patient.** `--data` is a resource, a Bundle, a QDM patient document, or a
   directory of them (one patient's record):
   ```
   raco cql gap.cql --data patient/ --valuesets valuesets/ \
       --parameter "Measurement Period=Interval[@2024-01-01T00:00:00Z, @2025-01-01T00:00:00Z)" \
       --define Gap --define Tests
   ```
   Every define is printed (or only the `--define`s), one per line, `name: value`. A definition that
   fails at run time prints its error in place of a value.
3. **Narrow it down.** When a define is wrong, print the defines it uses (`--define` each one) and
   work down to the retrieve. A retrieve that comes back empty is a code or value-set problem (is the
   code in the expansion? is the value set given?); a `where` that drops everything is usually null
   logic (three-valued `and`/`or`) or a choice element (guide.md, "Common problems").

Flags: `--libraries <dir>` holds included libraries as `<Name>.cql` (default: beside the file; then
the bundled FHIRHelpers 4.0.1 and 4.4.000). `--valuesets <dir>` holds value sets: FHIR ValueSets
with an `expansion` (or enumerated `compose` concepts), or cqm-models value set arrays. An
unavailable value set is an error, never false. `--parameter` takes a CQL expression; repeat it.

A value set you do not have can be written in VCL: `raco vcl "(http://loinc.org)4548-4"` prints a
ValueSet (its codes enumerated in `compose`); set its `url` to the one the library declares and save
it into `--valuesets`.

## From Racket
A `.cql` file starting with `#lang cql` is a module. Includes resolve beside the file, then to the
bundled FHIRHelpers:
```racket
#lang racket/base
(require (only-in cql/run read-json-files) "gap.cql")
(evaluate (read-json-files "patient") #:valuesets (read-json-files "valuesets")
          #:define "Gap" #:now "2025-06-30T12:00:00Z")    ; one define, the clock fixed
```
Without `#:define` it answers every define as a hash; with a name the library lacks, an error naming
the nearest. `#:now` (or `raco cql … --now 2026-01-01`, a date or a DateTime) fixes `Now()`/`Today()`/`AgeInYears()`.
It also provides `elm` and `elm-json`. Compiling the module (`raco make`) translates every define: an
unknown identifier or function fails the build at its line. A value prints as CQL writes it with
`render` from `cql/value` (`render : CqlValue -> string`: `true` is the string `"true"`, a FHIR value its
JSON text; compare with `"true"`, or test the value itself, `(eq? v #t)`). For control over everything (parameters, timezone, one
define at a time), use `cql/run`:
```racket
(require cql/run cql/value (only-in cql/parser/parse parse-cql) (only-in fhir/result/result ok-value) racket/file)
(define rl (load-runtime-library (ok-value (parse-cql (file->string "gap.cql")))
                                 #:resolver (directory-resolver "cql/")      ; <Name>.cql there, then bundled
                                 #:data (resources->data (read-json-files "patient"))
                                 #:valuesets (valuesets->terminology (read-json-files "valuesets"))
                                 #:parameters (hash "Measurement Period" (cql-value "Interval[@2024-01-01T00:00:00Z, @2025-01-01T00:00:00Z)"))
                                 #:offset 0))                                 ; the evaluation's timezone, minutes
(render (rlib-value rl "Gap"))                                                ; => "true"
```
`(rlib-value rl name)` evaluates one define (memoised); `(rlib-call rl name args)` (cql/interp)
applies a library function to values; `(cql-value "…")` is any CQL expression's value.

## Measures
- **A FHIR Measure:** `cql/measure`'s `evaluate-measure` runs it over `current-data-source` (the
  Measure, its Library resources with text/cql content, the value sets, the patients) and returns
  a MeasureReport; it is also the `$evaluate-measure` operation (`fhir/operation`). With a server:
  `raco fhir-server evaluate-measure <db> <Measure> --period-start D --period-end D`.
- **A cqm-models (QDM) measure document** (Bonnie, MADiE, cqm-execution):
  ```racket
  (require cql/cqm)
  (define-values (start end) (cqm-measure-period measure))                    ; the document's period
  (define rl (load-runtime-library main-library #:resolver (directory-resolver dir)
               #:data (resources->data (cqm-patient-records patient) #:patient-context? #f)
               #:valuesets (valuesets->terminology (read-json-files dir)) #:offset 0
               #:parameters (hash "Measurement Period" (cql-value (format "Interval[@~aZ, @~aZ]" start end)))))
  (cqm-measure-populations measure rl)   ; => (list (hash 'IPP 1 'DENOM 1 'DENEX 0 'NUMER 1)), per population set then stratum
  ```

## Working on the engine itself
`CLAUDE.md` is the architecture. `raco test .` runs everything; `raco cql --tck` is HL7's suite
(1820/1823). The corpora and their floors are in `TESTING.md`; a change must not lower any, and
`tests/guide-test.rkt` keeps guide.md's examples true.
