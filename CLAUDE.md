# CLAUDE.md — cql

Conventions, HTDP design style, and the conformance ratchet table live in the main `fhir` repo's
CLAUDE.md (`~/Projects/fhir`); this file holds only the notes for this package. This repo root IS the
`cql` collection (`(define collection "cql")`): modules are `cql/compile` etc., and imports of the
foundation use collection paths (`fhir/model/node`, never a relative path into another package).
The CLI is `cli.rkt` (`raco cql`); the conformance runner(s) live in `tests/` behind `--tck`.

## CQL / ELM lang (`#lang cql`, `cql/`)
Clinical Quality Language (https://cql.hl7.org/) as the next sibling lang. Unlike FHIRPath (evaluated
directly), **CQL is compiled to ELM** (Expression Logical Model, the machine-readable form of a
library) and the ELM is then RUN — the interpreter never evaluates CQL directly. This is the
**foundation CQL slice**: the expression + definition core (the FHIRPath-superset expression grammar +
`library`/`using`/`include`/`parameter`/`define`/function-`define`), System-typed literals (Integer,
Long, Decimal, Quantity, Ratio, String, Boolean, Date/DateTime/Time, List, Interval, Tuple),
operators, `if`/`case`. Layers:
- `cql/parser/` — a **fork-and-extend** of `fhirpath/parser/` (brag grammars can't be imported): a
  `br-parser-tools` `lexer.rkt` (CQL keywords, `'…'` strings, `"…"` delimited ids, `42L` longs), a
  `#lang brag` `grammar.rkt` (a superset precedence cascade + library/definition/interval/list/tuple/
  if/case productions), prefab `ast.rkt` + `lower` (the fhirpath pattern), and a memoized `parse.rkt`
  (`parse-cql` → `Result<cql-library>`, `parse-cql-expr` for bare-expression fragments).
- `cql/elm/` — prefab ELM node structs (`ast.rkt`; binary/n-ary/unary operators share the ELM
  OperatorExpression shape via generic `elm-nary`/`elm-unary` carrying the ELM type string) + `json.rkt`
  emitting **canonical ELM JSON** via `racket/json` (NOT `serialize/to-json.rkt`, which is FHIR-specific);
  `drop-false` shared util; interval `lowClosed`/`highClosed` are meaningful #f (never dropped).
- `cql/compile.rkt` — `cql-ast → elm-ast` with a **light System-type inference** (bottom-up
  `(values node type)`) that resolves overloaded surface operators (`+`→Add on numbers / Concatenate on
  strings; `&`→`Concatenate(Coalesce(l,''),Coalesce(r,''))`; `!=`→`Not(Equal)`; membership→In/Contains;
  numeric promotion Integer<Long<Decimal<Quantity) and stamps `resultTypeName`. Errors are `cql-error`
  (`cql/error.rkt`) in `err`.
- `cql/interp.rkt` + `cql/value.rkt` — the ELM interpreter (`eval-elm`, the ELM analog of `fp-eval`),
  **reusing** `fhirpath/{decimal,quantity,temporal-ops,equality,value}` and adding CQL's structured
  carriers (`cql-interval`/`cql-tuple`/`cql-ratio`/`cql-code`/`cql-concept`) + the `cql-null` singleton.
  Key semantic bridge: CQL is single-value-with-`null` (three-valued And/Or/Xor/Implies; null-propagating
  arithmetic/comparison), not FHIRPath's always-collection model.
- `#lang cql` (`cql/lang/reader.rkt` + `cql/base.rkt`): a `.cql` file is one library; the reader
  parses it and emits `(cql-library-form <ast>)`; the custom `#%module-begin` compiles to ELM and
  provides `library` (CQL AST), `elm` (ELM AST), `elm-json`, and `results` (define-name → interpreted
  value); `main` prints the ELM JSON. `examples/demo.cql` runs it.
- **Operator registry** (`cql/interp.rkt`): the interpreter core is a small structural recursion over
  ELM nodes; operators are DATA in a hash registry (`def-op!`/`apply-op`, O(1) dispatch, `lazy?` flag for
  short-circuit `and`/`or`/`implies`) — the CQL analog of `fhirpath/functions.rkt`. ~150 operators span
  arithmetic, comparison, logical, string, list, aggregate, DateTime (constructors, `<prec> from`,
  duration `+`/`-`, `between`/`difference in`, precision timing `before/after <prec> of`, `same … as`),
  interval (start/end/width/point-from, contains/includes/meets/overlaps/starts/ends, union/intersect/
  except/collapse/expand, precision-qualified timing), and a **query engine** (`from … where/return/sort/
  aggregate` over lists → cartesian tuples). Values reuse the FHIRPath System carriers (`moment`,
  `fp-decimal`, `fp-quantity`, `fp-long`) + CQL's `cql-interval`/`cql-tuple`/`cql-null`. Intervals are
  normalized to closed bounds at construction (open→successor/predecessor); Integer results overflow to
  null (32-bit). `cql/tests/bench.rkt` guards performance.
- Conformance: `cql/tests/harness.rkt` runs the cqframework/cql-tests XML format
  (`<test><expression [invalid=…]/><output/>`) via **compile→run-ELM→compare-expected** (the
  FHIRPath-TCK philosophy: it evaluates the `<output>` literal as CQL and compares values, treating an
  `invalid` expression as expected-to-fail and a `<library>` test as skipped). The **official HL7 suites
  are vendored verbatim** in `cql/tests/vendor/cql-tests/` (from `cqframework/cql-tests`, commit
  `36a47eb…`, see its SOURCE.txt); `raco cql --tck` (→ `cql/tests/run.rkt`, floor-ratcheted) runs
  them: **1819/1823 (99.8%)** — 11 of 16 suites at 100% (incl. Interval 410/411, DateTime 314/317,
  Arithmetic 234/236, List/Comparison/String/Types near-full). Notable semantics implemented:
  **uncertainty as Interval[min,max]** (operands filled to their full MILLISECOND low/high boundaries,
  extremes over the four corners; comparisons do band comparison; uncertainty intervals participate in
  `+`/`-`/`*`); **ratio literals** (`1'cm':2'cm'`, `=` componentwise / `~` cross-multiplied);
  **instance selectors** (`Code{…}`/`Concept{…}`/`Quantity{…}` build their carriers; other classes a
  typed `cql-obj` so `is Vocabulary` sees the ValueSet/CodeSystem hierarchy); **null-typed lists**
  (`null as List<T>` = `cql-null-list`: Length 0, scalar membership false, list-list inclusion null);
  **CQL ms-precision uncertainty** (a second-precision moment vs a ms-precision one agreeing through
  seconds compares as 'empty — deliberately different from FHIRPath's missing-frac=.000, applied only
  in cql/interp's cql-moment-compare, never in the shared fhirpath engine); calendar-duration `~`
  (pairwise 1 year = 12 months AND = 365 days); literal domains enforced at compile (Integer 32-bit,
  Decimal 28/8 digits); `X between A and B` desugars at the comparison level with the unit-word left
  operand recognised as a duration (context-sensitive, like the reference grammar). The **4 remaining
  misses are irreducible corpus edges**: `Floor(±2^31)` expects out-of-range literals to evaluate while
  ValueLiteralsAndSelectors requires the SAME literals to error (corpus self-contradiction, 2); one
  standalone uncertainty bound (`[17,44]`) contradicts the same corpus's arithmetic convention
  (`[16,44]`-derived, 1); and the `starts 1 day or less on or after day of` timing-phrase family
  (a whole grammar for 1 test). `cql/tests/coverage.rkt` reports the live per-suite/failure-bucket
  breakdown each iteration; `cql/tests/vendor/cql-smoke.xml` is the harness's own self-test (not part of
  the conformance number). `raco cql <file.cql>` prints ELM JSON.
- **Real-library corpus** (`tests/elm-corpus-test.rkt`, `tests/elm-corpus-harness.rkt`): 32 CQL
  libraries from published packages (fhir.cqf.common 4.0.1, hl7.fhir.us.cqfmeasures 5.0.0,
  hl7.fhir.uv.cql 2.0.0 — `tests/vendor/elm-corpus/SOURCE.txt`), each with the reference cql-to-elm
  translator's ELM; per library: parse, compile, and per-`define` ELM structural equality (translator
  metadata dropped). **Baseline: 2/32 parse, 0/803 definitions ELM-equal** — the first failures are
  `code "x": …` definitions (19 libraries), retrieves (7), FHIR model types / `start of` / `sort by` (4):
  the deferred slice below, now measured. Ratcheted.
- **Deferred CQL slices**: retrieves (`[Condition: "vs"]`) + a FHIR data-source/retrieve layer (absent —
  `navigate.rkt` assumes in-memory roots), queries (`with`/`where`/`return`/`sort`/`aggregate`),
  terminology-backed membership via the `tx-service`, FHIR model-info from `full-field-specs`, ELM XML
  output, temporal literals as canonical ELM Date/DateTime/Time constructor nodes (slice-1 emits them as
  a Literal carrying the lexeme — round-trips through our interpreter), and full agreement with the reference
  translator's ELM (the corpus above measures it).
