# Testing cql

Every suite is vendored with its source and license (`tests/vendor/*/SOURCE.txt`) and gated by a floor
that only rises. `raco test .` runs them all; the commands below run one with its report.

## Results

| Suite | What it checks | Result |
|---|---|---|
| HL7 CQL suite (cqframework/cql-tests) | each expression compiled to ELM, run, and compared with the expected value | 1820/1823 |
| Published libraries (FHIRHelpers, CQF Measures, the CQL IG) | each of 32 libraries parses and compiles; each definition's ELM compared with the reference translator's | 32/32 compile; 769/803 definitions ELM-identical |
| eCQMs (cqframework/ecqm-content-r4-2021) | four measures over their 76 test patients: populations against the expected results | 71/76 |
| Evaluator differential | the compiled evaluator against the tree walker, result by result, over the three corpora above | 0 of 6,206 results differ |
| Code paths | a retrieve's statically resolved code path against the generic navigation, over 65 (type, property) pairs, the eCQM data, the R4 IG examples in fhir-spec and mutants of each resource's code element | 0 of 19,472 differ |

## The misses, named

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

**Published libraries:** the 34 definitions whose ELM differs are printed by
`tests/elm-corpus-test.rkt`, classified by the kind and path of the first difference.

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
