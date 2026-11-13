# Testing cql

Every suite is vendored with its source and license (`tests/vendor/*/SOURCE.txt`) and gated by a floor
that only rises. `raco test .` runs them all; the commands below run one with its report.

## Results

| Suite | What it checks | Result |
|---|---|---|
| HL7 CQL suite (cqframework/cql-tests) | each expression compiled to ELM, run, and compared with the expected value | 1820/1823 |
| Published libraries (FHIRHelpers, CQF Measures, the CQL IG) | each of 32 libraries parses and compiles; each definition's ELM compared with the reference translator's | 32/32 compile; 786/803 definitions ELM-identical |
| QI-Core libraries (cqframework/ecqm-content-qicore-2024, `tests/qicore-elm-corpus-test.rkt`) | the 98 libraries of the 2024 eCQMs, written against QI-Core 4.1.1: each parses and compiles to ELM over FHIR; each definition's ELM compared with the reference translator's (3.25.0) | 98/98 compile; 1932/2144 definitions ELM-identical |
| QI-Core eCQMs (`tests/qicore-measure-test.rkt`) | Breast and Cervical Cancer Screening, Controlling High Blood Pressure, Diabetes Glycemic Status > 9%, over their 190 MADiE test cases: populations against each case's MeasureReport | 188/190 |
| QDM libraries (cqm-execution's fixtures, `tests/qdm-elm-corpus-test.rkt`) | 42 libraries (QDM 5.0.2 to 5.6) and the ELM their measure documents carry | 41/42 compile; 559/723 definitions ELM-identical (older translators) |
| QDM measures (`tests/qdm-measure-test.rkt`) | 19 measures over 33 cqm-models patients: cqm-execution's own assertions (proportion, several population sets with strata, continuous-variable and ratio observations), then the expected values stored in the patients | all 31 assertions; 16/26 stored |
| ELM shapes (`tests/elm-shape-test.rkt`) | construct by construct, the ELM the corpora made this translator write (a union chain's grouping, `ValueSet contains x`, a point beside an interval, a multi-source query's return, an aggregate, typed lists, branches of different types, Skip/Take/Tail as Slice …), each with the value it evaluates to | passes |
| Mapping models (`tests/mapping-model-test.rkt`) | QI-Core and US Core over FHIR JSON: profile retrieves, mapped elements, QI-Core's NotDone value set, `convert … to days` | passes |
| eCQMs (cqframework/ecqm-content-r4-2021) | four measures over their 76 test patients: populations against the expected results | 71/76 |
| Evaluator differential | the compiled evaluator against the tree walker, result by result, over the three corpora above | 0 of 6,206 results differ |
| Code paths | a retrieve's statically resolved code path against the generic navigation, over 65 (type, property) pairs, the eCQM data, the R4 IG examples in fhir-spec and mutants of each resource's code element | 0 of 19,472 differ |
| Any data model (`tests/any-model-test.rkt`) | a model made from logical models, and one from a ModelInfo document in a model directory, each with data in its own JSON shape: retrieves by value set and code, the Patient context through the model's relationships, ages, choices, lists, nested classes | passes |
| The guide (`tests/guide-test.rkt`) | every CQL example in guide.md translates; the worked example and the common problems it describes behave as written | passes |
| The manual (`tests/docs-test.rkt`) | scribblings/cql.scrbl renders with no broken reference to its own sections or bindings | passes |
| Model neutrality (`tests/model-neutral-test.rkt`) | the engine's modules name no data model (no FHIR, its elements or FHIRHelpers) in their code | passes |

When FHIR became one model among others (2026-11-08), every outcome of the previous engine was
recorded and compared with the new one, value by value: 3,608 suite expressions and outputs, 565 corpus
definitions, 2,051 eCQM definitions and 76 eCQM population results. None differ.

## The misses, named

**QI-Core eCQMs, 2:** two Diabetes > 9% cases (`NUMPass HbA1cGreaterThan9`, `NUMPass GMIGreaterThan9`)
describe a 9.1% result, but their exported Observation's value is a SNOMED concept; the expected
reports follow the description, not the data.

**QDM stored values, 10**, each named in `tests/qdm-measure-test.rkt`: QDM 5.3 measures that read
`principalDiagnosis` over QDM 5.5 data, which no longer has it (cqm-execution disables its own tests
of these cases); stored observations that predate cqm-execution's current results, which this engine
gives; a stored stratum the patient's data contradict; and one library definition the reference
translator could not translate either.

**HL7 suite, 3:**
- `Floor(2147483648)` and `Floor(-2147483649)` expect out-of-range Integer literals to evaluate,
  while `ValueLiteralsAndSelectors` requires the same literals to be errors. The corpus contradicts
  itself; this engine follows the literal rule.
- `days between DateTime(2014, 1, 15) and DateTime(2014, 2)` expects the uncertainty interval
  `[17, 44]`, against the `[16, 44]` the same corpus's arithmetic gives.

**eCQMs, 5**, each a problem in the corpus's expectations rather than the engine (`tests/measure-test.rkt`
names them):
- two colorectal timezone cases, `…-FAIL-…-TZoffset-07`, that are screened at every offset east of
  −09:00, so their "FAIL" labels hold only for a period anchored at −10:00 or further west;
- `numer-EXM130-FitDNA-FAIL-category-laboratory`, identical to the passing `FitDNA-3yr` case apart
  from its ids;
- `denomexcl-CMS122-Patient`, whose MeasureReport gives initial population 1 and denominator 0, while
  the measure defines its denominator as its initial population;
- `no-ip-CMS122-Patient`, whose MeasureReport leaves out a patient that meets every initial-population
  criterion.

**Published libraries:** the 17 definitions whose ELM differs are printed by
`tests/elm-corpus-test.rkt` (and the QI-Core and QDM ones by their tests), classified by the nodes
at the first difference — what the reference wrote and what this translator wrote there
(`Union | Retrieve`, `FunctionRef:ToConcept | Property.code`). The largest classes left:
- `Union | Retrieve` (QI-Core, 56): the reference translator follows a code element through a
  reference — `[MedicationRequest: "VS"]` is also the requests whose `medication` references a
  Medication coded in the value set. Not written here yet (the manual's Limits say so).
- a property of a value typed as a choice of resources (`ToValue | Property.performed`): the reference
  resolves the property on each member.
- EXM108's lists of mixed resource types passed through `Coalesce` and `in`.
- `Example/includesOrStartsDuring`: the two packages' translators pick different choice members.

What the comparison leaves out, because the corpus packages' translators disagree with each other
or because it carries no meaning: translator metadata (localId, locator, annotations, result types,
signatures), empty lists, structural `type` tags, `strict: false`, the forms around value sets, a
false `lowClosed`/`highClosed`, an empty String literal's `value`, a Ratio's parts' `type`, and the
order of a ChoiceTypeSpecifier's choices (in the reference, a hash set's iteration order).
`tests/elm-shape-test.rkt` pins, construct by construct, the shapes the corpora made this
translator write.

## Running them

```sh
raco test .                          # everything, with the floors
raco cql --tck [-v]                  # the HL7 suite; -v lists every failure
raco test tests/elm-corpus-test.rkt  # the published libraries, with the differences classified
raco test tests/measure-test.rkt     # the eCQMs
raco test tests/evaluator-test.rkt   # the evaluator differential
raco test tests/code-path-test.rkt   # the code-path differential
racket tests/coverage.rkt            # the HL7 suite by suite and by failure kind
racket tests/bench.rkt               # timings under both evaluators
```

## Performance

`tests/bench.rkt` times the HL7 suite end to end, an evaluation loop, and the eCQM corpus, each under
both evaluators (`CQL_ELM_EVALUATOR=tree` selects the tree walker). On the 76 eCQM patients, with
their data read up front, one patient takes about 0.32 ms.

Translating is parsing, mostly: the grammar's parser costs about 60 µs a token, and translating the
98 QI-Core libraries is 13 s of parsing and 0.4 s of everything else. So a definition is parsed
once: the same definition met again — in another library, under another file name, in a document
being edited — is not parsed again (FHIRHelpers: 0.55 s the first time, 8 ms after an edit to one
definition), and a definition that is one long list is parsed element by element (a 93,000-token
code table: 27 s, now 5).
