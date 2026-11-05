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
  The algebra is `cql/interp/`, one pure, tested module per concern — `base.rkt` (the error, the
  numeric tower, 32-bit overflow to null, interval bounds), `temporal.rkt` (constructors with domain
  checks, components, duration/difference and their uncertainty intervals, boundaries, truncation,
  date arithmetic), `arith.rkt`, `compare.rkt` (`cql-compare`/`cql-equal`/`equivalent`, the null-aware
  list helpers) and `interval.rkt` (relations, membership, set operations, collapse/expand, the timing
  dispatch) — and `interp.rkt` keeps the environment, the evaluator, the operator catalogue, queries,
  strings, aggregates, navigation and conversions. Six operators used to be registered twice (the
  later `def-op!` wins): the dead first copies are gone.
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
  them: **1820/1823 (99.8%)** — 11 of 16 suites at 100% (incl. Interval 410/411, DateTime 314/317,
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
  misses are irreducible corpus edges** (were 4; the interval-phrase grammar closed the 4th): `Floor(±2^31)` expects out-of-range literals to evaluate while
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
  metadata dropped). **32/32 parse; 32/32 compile; 764/803 definitions ELM-equal** — ratcheted. The
  grammar follows cql.g4 for terminology definitions (`code`/`concept`/`codesystems`/`display`, access
  modifiers), retrieves (`[Ctx -> Type: path in "VS"]`), `with`/`without … such that`, `sort by`,
  fluent functions, `Tuple {…}`/`Choice<…>` types, `is true`/`is false`, and the full interval-operator
  phrase grammar (`starts`/`ends`/`occurs` bounds, `3 days or less on or before day of`, `within … of`;
  leading/trailing bounds desugar to Start/End at lowering, offsets/within are `cql-timing-offset`/
  `cql-within`). Keywords are admitted as names after `.`, as tuple keys and as function names
  (`period.start`, `System.Code`, `define fluent function version`). A query may be the RIGHT operand
  of a set operator only (on the left it would end a greedy where/aggregate body early — a TCK test
  caught that). Libraries parse DEFINITION BY DEFINITION (`parse.rkt` splits the token stream before
  each definition keyword in column 0): brag's CFG parser is superlinear, FHIRHelpers took seconds whole.
- **Typed translation over a data model** (`compile.rkt`, `model/`): `using FHIR version '4.0.1'` loads
  the reference translator's own FHIR ModelInfo (`model/fhir-modelinfo-4.0.1.xml.gz`, cqframework,
  Apache-2.0; `model/gen-modelinfo.rkt` derives the compact `fhir-4.0.1.rktd` that
  `model/modelinfo.rkt` reads: element types, base types, primaryCodePath, profile urls, contexts, and
  the FHIRHelpers conversion table). `compile` returns `(values elm CType)`; a library compiles in an
  `lctx` (every declared name, the model, includes) with definitions compiled ON DEMAND and memoized
  (a reference needs the definition's type). Includes resolve through `current-library-resolver`
  (name version -> library AST; the corpus harness resolves within the package). The translator's
  behaviours implemented: Retrieve (dataType `{url}Type`, templateId, codeProperty = primaryCodePath,
  `in` for a ValueSet, `~` + ToList for a Code); the implicit `context Patient` definition
  (SingletonFrom([Patient])); Property `scope` on query aliases, AliasRef/QueryLetRef/IdentifierRef;
  model->System implicit conversion (FHIRHelpers.ToString/ToConcept/ToInterval/… — only when the library
  includes FHIRHelpers, inherited through base types) at every System-typed operator; System promotions
  (Integer->Decimal, Date->DateTime, Code->Concept); overload resolution by conversion cost with
  argument coercion (incl. a choice argument to a member-typed parameter = implicit As); fluent
  functions; FHIRPath-style methods (`.where(…)` = Query over `$this`, `.first()`, `.single()`,
  lowercase System names); InValueSet/AnyInValueSet; quantity-offset timing phrases (`3 years or less on
  or before` = And(In(A, Interval[B - q, B]), Not(IsNull(B)))); AgeIn*/CalculateAge*At over the model's
  birth-date path; ELM operators with named attributes (First.source, Split.stringToSplit, …). Unqualified
  type names mean the model's type when it has one (`Quantity` in a FHIR library is FHIR.Quantity).
  Also matched to the reference: a choice operand meeting a System-typed one is cast to the member that
  converts (`ToQuantity(O.value as FHIR.Quantity)`, an implicit As written with `asType`); an untyped
  `null` branch of if/case is cast to the other branches' type; a member value passed to a choice-typed
  parameter is cast to the whole choice; instance element values are coerced to the class's element
  types; `union` chains group as the reference parse does (`((A u B) u (C u D)) u E`).
- **Promotions over intervals** (`promotion`/`promote`/`unify-timing` in compile.rkt): an
  Interval<Date> meeting an Interval<DateTime> — a case arm beside one (FHIRCommon's ToInterval), a
  timing operand, the window of `A 30 days or less before Today()` against a DateTime A — is rebuilt
  as the reference writes it: `Interval[ToDateTime(X.low), ToDateTime(X.high)]` with
  `lowClosedExpression`/`highClosedExpression` = `X.lowClosed`/`X.highClosed` (X repeated). The
  `elm-interval` closed fields may be expressions: the emitter writes the `…Expression` keys, the
  interpreter evaluates them, and `.lowClosed`/`.highClosed` navigate an interval value. if/case
  branches meet at the widest promotable type. A member value against an interval whose points are
  a choice (`Interval[C.onset, C.abatement] includes E.period`) is cast to the whole choice
  (`member-of-interval-choice`), not converted to System.
- Remaining ELM differences (39 definitions, ratcheted; `tests/elm-corpus-test.rkt` prints them
  CLASSIFIED by the kind and path of the first difference, largest class first, with examples — the
  work list): EXM108's day-of-procedure phrases and typed unions (a union of differently-typed queries
  the reference casts to a choice list); a CodeRef through an include alias inside `such that`;
  `condition.onset during encounter.period` where the two packages' translators pick DIFFERENT choice
  members (CQF Measures: Period; the CQL IG: dateTime); ratio/quantity instance details and
  Message operands in FHIRHelpers; a handful of single cases the output names.
- The corpus harness normalises only translator-VERSION noise, on both sides: empty lists = absent
  (newer translators write `let: []`, `dateFilter: []`), structural element `type` tags
  (InstanceElement, AliasedQuerySource, …), `strict: false` (the schema default), and around value
  sets — ValueSetRef's `preserve`, its `type` under InValueSet's `valueset`/Retrieve's `codes`,
  Retrieve's default `codeComparator` "in", and a FALSE `lowClosed`/`highClosed` (fhir.cqf.common's
  translator leaves it out; ours writes it) — the two packages disagree, so neither form is held.
  Includes resolve within the package, else from another corpus package (CQF Measures includes
  FHIRHelpers 4.0.1 but ships only its ELM; the source is in fhir.cqf.common).
- **Execution over FHIR data** (`run.rkt`, `fhir-value.rkt`, interp's runtime libraries): FHIR data
  are `fhir-node`s (model type + JSON, `value.rkt`) navigated through the ModelInfo (`fhir-property`:
  element types, choice elements by their `valueX` key, list elements, `.value` of a primitive is the
  System value). `load-runtime-library` compiles a library and its includes into `rlib`s (memoised
  definitions, overloads by name, parameters, terminology defs, include aliases); `call-library-function`
  picks the overload by the FunctionRef's `signature` (now emitted), else by the argument values' types.
  A `runtime` carries the data (`resources->data`: type -> resources; patient-context filtering on by
  default), value set expansions (`valuesets->terminology`) and the evaluation timezone offset
  (`current-evaluation-offset`: a DateTime without an offset takes the request's — spec; off for the
  TCK). Retrieve filters by codeProperty against a ValueSet (membership by system+code) or codes (~);
  an unavailable value set is an error, never false. The main library's parameter defaults apply to its
  includes (as a measure evaluator passes them). `measure-populations` = patient-based proportion
  scoring. Also: Code/Concept Equal (all elements, the engine's structural equality) and Equivalent
  (code+system; Concepts by a shared code) — Code equality was simply broken before (the TCK never
  compared Codes); dotted property paths (`birthDate.value`); ToList, Split, Slice, IndexOf, Matches,
  ReplaceMatches, Message (an Error stops evaluation), CalculateAge[At]; `collapse … per`; Coalesce
  converts a FHIR argument among System ones; a choice argument to a parameter one member converts to.
  `raco cql <file.cql> --data <dir|json> [--valuesets dir] [--libraries dir] [--define name]…` runs it.
- **eCQM execution corpus** (`tests/measure-test.rkt`, `tests/measure-harness.rkt`, `tests/vendor/ecqm`):
  4 measures (Breast/Cervical/Colorectal Cancer Screening, Diabetes HbA1c Poor Control) with their own
  12 libraries and 57 value set expansions, 76 test patients from cqframework/ecqm-content-r4-2021
  (CC0). Expected populations: the case's MeasureReport, else the repository's naming convention
  (numer/denom/denomexcl/neg-ip, `-FAIL-`). **71/76 agree** — ratcheted; the 5 disagreements are named in
  the test with the evidence (timezone-problem cases the library itself flags, a case identical to a
  passing one, two MeasureReports inconsistent with the data or with proportion scoring).
- **Deferred CQL slices**: retrieves (`[Condition: "vs"]`) + a FHIR data-source/retrieve layer (absent —
  `navigate.rkt` assumes in-memory roots), queries (`with`/`where`/`return`/`sort`/`aggregate`),
  terminology-backed membership via the `tx-service`, FHIR model-info from `full-field-specs`, ELM XML
  output, temporal literals as canonical ELM Date/DateTime/Time constructor nodes (slice-1 emits them as
  a Literal carrying the lexeme — round-trips through our interpreter), and full agreement with the reference
  translator's ELM (the corpus above measures it).
- **Measures as an operation** (`measure.rkt`): `evaluate-measure` runs a Measure over
  `current-data-source` (fhir/data-source: a Bundle in memory, a directory, or fhir-server's store) —
  its Library and includes as Library resources, value sets from the source then
  `current-sd-packages`, each patient's data by reference — and `$evaluate-measure` is the operation
  value (fhir/operation, the R4 OperationDefinition, which returns a MeasureReport):
  `(apply-operation "$evaluate-measure" measure #:periodStart "2021-01-01" #:periodEnd "2021-12-31")`. Moved here from
  fhir-server's store/measure.rkt, which now binds it to the store. `tests/measure-operation-test.rkt`
  applies it by canonical to an eCQM case over an in-memory data source.
- **`#lang cql` as a module other code uses** (base.rkt): it provides `library`, `elm`, `elm-json`,
  `(evaluate resources #:parameters h #:valuesets vss)` — the library over a patient's FHIR data
  (cql/run `evaluate-library`: define-name -> value) — and `(results)` (no data). Nothing runs at
  require time: a library with retrieves used to fail as soon as it was required. fhir-examples'
  screening/ program requires a .cql file and calls `evaluate` per patient.

- **Editor and agent support** (`support.rkt`, through `fhir/lang/support`): diagnostics (parse, then
  every definition translated), the libraries beside a document as its project, the declarations as an
  outline (read from the text), hover, go to definition (an include alias leads to the library's file
  when it is beside; a qualified `C.X` through its alias), completion of declared names and keywords,
  and the tools `cql_elm` and `cql_evaluate`. The LSP and MCP hubs load it by collection name.
