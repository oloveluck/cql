# Clinical Quality Language (CQL)

CQL states facts about a patient: "had an HbA1c test this year", "is in the denominator". A library
is a list of named definitions. It compiles to ELM, the standard machine form, and the ELM runs over
one patient's data at a time. This engine passes 1820 of the 1823 tests in HL7's CQL suite.

This guide is for writing CQL here: the language in brief, the ways to run it, the data models it
reads, and what to do when something goes wrong. The specification is the reference for the language
itself.

## Official references
- The CQL specification: https://cql.hl7.org/
- Author's guide: https://cql.hl7.org/02-authorsguide.html
- Developer's guide (the full language): https://cql.hl7.org/03-developersguide.html
- Operator and function reference: https://cql.hl7.org/09-b-cqlreference.html
- ELM, the logical specification: https://cql.hl7.org/04-logicalspecification.html
- Using CQL with FHIR (IG): https://hl7.org/fhir/uv/cql/

## Cheat sheet
```cql
library Screening version '1.0'
using FHIR version '4.0.1'
include FHIRHelpers version '4.0.1' called FHIRHelpers
valueset "HbA1c": 'http://example.org/fhir/ValueSet/hba1c'
parameter "Measurement Period" Interval<DateTime>
context Patient

define "Tests": [Observation: "HbA1c"]                        // a retrieve: type and value set
define "Tests This Year":                                     // a query; `as dateTime`: see
  "Tests" O where (O.effective as dateTime) during "Measurement Period"   // Common problems
define "Has Test": exists "Tests This Year"
define "Adult": AgeInYears() >= 18
define "Gap": "Adult" and not "Has Test"
define "Last Value":
  (Last("Tests This Year" O sort by (effective as dateTime))).value as Quantity
```
Strings use single quotes, and names with spaces use double quotes. `and`, `or` and `not` are
three-valued, so null is not false. `exists` tests a list. `First`, `Last`, `Count` and `Sum`
aggregate. Timing phrases include `during`, `before`, `3 days or less before` and `overlaps`.
Declarations come in this order: `using`, `include`, `codesystem`, `valueset`, `code`, `concept`,
`parameter`, then `context` and the definitions.

## Worked example
A patient aged 18 or over with no HbA1c test has a gap:
```cql
library HbA1cGap version '1'
using FHIR version '4.0.1'
include FHIRHelpers version '4.0.1' called FHIRHelpers
codesystem "LOINC": 'http://loinc.org'
code "HbA1c code": '4548-4' from "LOINC"
context Patient
define "Tests": [Observation: "HbA1c code"]
define "No HbA1c": not exists "Tests"
define "Gap": AgeInYears() >= 18 and "No HbA1c"
```
Over a Patient born in 1970 with no Observations, `Gap` is `true`.

## Recipe: the latest value per patient
"Is the most recent HbA1c above 9%?" is not "was any HbA1c above 9%?": a FHIR search like
`Observation?code=…|4548-4&value-quantity=gt9` answers the second. Sort by the date, take the last:
```cql
library LatestA1c version '1'
using FHIR version '4.0.1'
include FHIRHelpers version '4.0.1' called FHIRHelpers
codesystem "LOINC": 'http://loinc.org'
code "HbA1c code": '4548-4' from "LOINC"
context Patient
define "Results": [Observation: "HbA1c code"] O where O.status in { 'final', 'amended', 'corrected' }
define "Latest": Last("Results" O sort by (effective as dateTime))
define "Latest Value": "Latest".value as Quantity
define "Latest Number": "Latest Value".value
define "Latest Unit": "Latest Value".unit
define "Latest Date": "Latest".effective as dateTime
define "Poorly Controlled": "Latest Value" > 9 '%'
define "No HbA1c": "Latest" is null
define "Name": Combine(First(Patient.name).given, ' ') + ' ' + First(Patient.name).family
```
With results of 9.6% (January) and 8.9% (June), `Latest Value` is the June Quantity (8.9 %),
`Latest Number` is 8.9 and `Latest Unit` `'%'` (plain columns for a table, where a Quantity would be a
JSON object), and `Poorly Controlled` is `false`; with no result it is `null`, so ask `"Latest" is null`
for "never tested" (or `Coalesce("Poorly Controlled", false)` when a table should say false for them).
`Name` is the patient's given names and family name, for a table a person reads.

For another lab, change the code and the comparison: LDL cholesterol is `code "LDL code": '13457-7'
from "LOINC"` with `"Latest Value" >= 160 'mg/dL'`, eGFR `'98979-8'` with `< 60 'mL/min/{1.73_m2}'`;
the comparison's unit must be the data's (or convertible: `'mmol/L'` against `mg/dL` data is not).
A table a person reads says yes/no rather than 1/0 and blank — in the workspace's SQL over the run:
`SELECT "Name", "Latest Number", "Latest Date", CASE WHEN "No HbA1c" THEN 'never tested' WHEN
"Poorly Controlled" THEN 'yes' ELSE 'no' END AS above_9 FROM LatestA1c` (a run is named by its file, `LatestA1c.cql`).

Variations, each a define or two on the same `"Results"`: the latest in a window, the last two, and
their mean (a trend needs more than one value):
```cql
library A1cVariations version '1'
using FHIR version '4.0.1'
include FHIRHelpers version '4.0.1' called FHIRHelpers
codesystem "LOINC": 'http://loinc.org'
code "HbA1c code": '4548-4' from "LOINC"
parameter "Measurement Period" Interval<DateTime> default Interval[@2025-03-01T00:00:00Z, @2026-01-01T00:00:00Z)
context Patient
define "Results": [Observation: "HbA1c code"] O where O.status in { 'final', 'amended', 'corrected' }
define "In Period": "Results" O where (O.effective as dateTime) during "Measurement Period"
define "Latest In Period": Last("In Period" O sort by (effective as dateTime))
define "Last Two": Take(("Results" O sort by (effective as dateTime) desc), 2)
define "Mean Of Last Two": Avg("Last Two" O return (O.value as Quantity).value)
define "Rising": ((First("Last Two").value as Quantity).value) > ((Last("Last Two").value as Quantity).value)
```
With 9.6% (January), 8.9% (June) and 9.2% (September) and the period from March: `Latest In Period`
is September's, `Last Two` is September's then June's, `Mean Of Last Two` is 9.05, and `Rising` is
`true` (the newer is first). A window is a parameter, so `parameters: {"Measurement Period": …}` moves
it without editing the library. The query inside `Take` is in parentheses: `sort by … desc, 2` would
read `2` as a second sort item.

## Running it
**Command line.** `cql` (or `raco cql`):
```
cql gap.cql                                     # the library's ELM, as JSON; errors at their line
cql gap.cql --data patient/                     # every define's value for this patient
cql gap.cql --data patient.json --define Gap --libraries cql/ --valuesets valuesets/ \
    --parameter "Measurement Period=Interval[@2024-01-01T00:00:00Z, @2025-01-01T00:00:00Z)"
```
`--data` is one patient's record: a resource, a Bundle, a QDM patient document, or a directory of
them. `--libraries` holds the libraries it includes (FHIRHelpers is bundled), `--valuesets` the
expanded value sets (FHIR ValueSets, or cqm-models value set arrays). `--now 2025-06-30T12:00:00Z`
fixes what `Now()`, `Today()` and `AgeInYears()` read, so a run gives the same answer tomorrow. A
translation error names its definition and line, and exits 1.

**As a module.** A file that starts with `#lang cql` is a Racket module providing `elm`, `elm-json`
and `(evaluate resources #:parameters h #:valuesets vss #:now t #:define name)`: the define-name to
value table for one patient's resources, or with `#:define` that one define's value (a name the
library does not define is an error naming the nearest). `#:now` (a DateTime string) fixes the
clock, as `--now` does (`#:now #f`, the default, reads the clock); `cql/value`'s `current-evaluation-time` parameter does it for any call.
Nothing runs when it is required. The file is the library after one line — `#lang cql`, then
`library Screening version '1.0'`, `using FHIR version '4.0.1'`, … — and another module says
`(require "screening.cql")` and calls `(evaluate resources)`.
Without the `#lang cql` line, requiring the file fails with `default-load-handler: expected a
`module' declaration, but found something else`: add the line. `raco cql` reads the file with or
without it.

**In a workspace** (fhir-mcp). A `.cql` file is a Library. `run` evaluates every define (or one,
`entry`) for every patient of the workspace's data, one row per patient. `explain` follows one
patient's answer through the defines it uses down to the retrieves, with how many resources of each
type the patient has and which ones the filter kept. `query("cql", "AgeInYears() > 65")` evaluates
one expression for every patient without a file; it includes the workspace's libraries by name, so
`query("cql", 'Diabetes."Latest Value"')` reads a define of `Diabetes.cql`. Includes resolve from the workspace's `.cql` files,
then the data source's Libraries, then the bundled FHIRHelpers. Value sets resolve from the workspace
(a `.vcl` file, an FSH ValueSet), then the data, then the packages. Parameters are CQL expressions,
set in `workspace.yaml` or on the run.

## Data models
`using` names the model. Installed:
- **FHIR 4.0.1.** Include FHIRHelpers: it supplies the conversions between FHIR elements and CQL's
  own types (`O.status = 'final'` compares a FHIR code with a String through it).
- **QI-Core** 4.1.1, 5.0.0, 6.0.0 and **US Core** 3.1.1, 6.1.0, which are views over FHIR. A library
  names their profiles, and its ELM reads FHIR, as the reference translator writes it: each profile
  element reads through its mapping (`C.verificationStatus` becomes
  `FHIRHelpers.ToConcept(C.verificationStatus)`; an extension or a component becomes the query that
  finds it). So the data is plain FHIR JSON. QI-Core libraries include FHIRHelpers 4.4.000, which is
  bundled too:
```cql
library HypertensionCheck version '1'
using QICore version '4.1.1'
include FHIRHelpers version '4.4.000' called FHIRHelpers
codesystem "ConditionVerificationStatusCodes": 'http://terminology.hl7.org/CodeSystem/condition-ver-status'
valueset "Essential Hypertension": 'http://cts.nlm.nih.gov/fhir/ValueSet/2.16.840.1.113883.3.464.1003.104.12.1011'
code "confirmed": 'confirmed' from "ConditionVerificationStatusCodes"
context Patient
define "Hypertension": [Condition: "Essential Hypertension"] C
  where C.verificationStatus is null or C.verificationStatus ~ "confirmed"
```
- **QDM** 5.0.2 to 5.6, over the JSON that Bonnie, MADiE and cqm-execution exchange (cqm-models): a
  patient document with its `dataElements`, and value sets as cqm-models value set arrays. A
  retrieve names a QDM datatype by its label; `Positive…`/`Negative…` (`"Intervention, Order"` /
  `"Intervention, Not Ordered"`) select the data elements without or with a `negationRationale`.
  `cql/cqm` computes a cqm-models measure document's populations as cqm-execution does.
```cql
library HospiceCheck version '1'
using QDM version '5.6'
valueset "Hospice care": 'urn:oid:2.16.840.1.113762.1.4.1108.15'
parameter "Measurement Period" Interval<DateTime>
context Patient
define "Hospice Encounters":
  ["Encounter, Performed": "Hospice care"] E where E.relevantPeriod overlaps "Measurement Period"
define "In Hospice": exists "Hospice Encounters"
```
- **Your own model.** In a workspace, design the classes in FSH as logical models and name the model
  in `workspace.yaml`:
```
models:
  Clinic: http://example.org/clinic patient=Member birthDate=born version=1.0
```
  `Clinic` is made from every logical model whose canonical starts with `http://example.org/clinic`.
  `patient=` names the class that is the Patient context (any Reference to it relates a record to
  that patient), `birthDate=` the element `AgeInYears()` reads, `key=` the element that keys a
  patient (default `id`). Their datatypes become CQL's own (a CodeableConcept is a Concept, a Period
  an `Interval<DateTime>`), so no FHIRHelpers is needed: `using Clinic version '1.0'`, then
  `[Visit: "HbA1c"]`. Outside a workspace, a model is a ModelInfo document in a directory named by
  `CQL_MODEL_PATH`, with its representation (how its JSON looks) in a
  `<same name>.representation.rktd` beside it.

## Common problems
- `no data model is installed`: the `using` names a model that is neither installed nor in
  `workspace.yaml`'s `models`, nor in `CQL_MODEL_PATH`.
- `value set … is not available` at run time: no ValueSet with that url is in the value sets given
  (the workspace, the data, the packages). In a workspace, write a `.vcl` file for it and use its
  canonical. An unavailable value set is an error, never false.
- `Could not resolve … FHIRHelpers`, or a comparison of a FHIR element with a CQL value that fails
  to translate: add `include FHIRHelpers version '4.0.1' called FHIRHelpers` (4.4.000 for QI-Core).
- A define is null when you expected false: an operand was null (CQL's logic is three-valued).
  Explain it for that patient, or guard with `is null` / `Coalesce`.
- A timing test on a choice element is never true: in a FHIR 4.0.1 library,
  `O.effective during "Measurement Period"` reads only a Period `effective` (the reference translator
  casts the choice to Period), so an Observation with `effectiveDateTime` gives null. Say which
  member you mean: `(O.effective as dateTime) during "Measurement Period"`. QI-Core libraries use
  QICoreCommon's `O.effective.toInterval()`, which reads every member.
- `unknown identifier` for a query alias inside `sort by`: `sort by` sees the result row, not the
  aliases. Sort by the row's own elements (`sort by (effective as dateTime)`: a choice, so say which member).
- `no codesystem named "LONIC" (did you mean `LOINC`?)` at a `code` line: a code, concept or value
  set names a terminology definition the library does not have (a typo, or one in an include:
  qualify it, `FHIRHelpers."…"`).
- A whole library fails to load because one definition does not translate: it does not. The other
  definitions run, and that one raises its translation error when evaluated.
