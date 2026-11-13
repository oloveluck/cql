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
- **Two evaluators, one semantics** (interp.rkt): `eval-elm` is the tree walker and the reference;
  `compile-elm` is a closure compiler, the same structural recursion with each clause returning a
  `(Env -> CqlValue)` closure, cached per node in an ephemeron table. Libraries (`make-rlib`,
  `run-library`) and `run-elm` use the evaluator `current-elm-evaluator` names — `'compiled` by
  default, `CQL_ELM_EVALUATOR=tree` for a process. Two rules keep them identical: compiling never
  raises (an unknown operator or a bad literal raises when its closure RUNS, so an untaken branch
  raises nothing), and only literals and quantities are computed ahead (nothing that reads the clock,
  the evaluation offset, data or parameters). The query engine, library-function calls and the
  operators' algebra are shared (they take the evaluator as an argument; and/or/implies share
  `and-values`/`or-values`/`implies-values`). `tests/evaluator-test.rkt` compares every result, not pass
  counts: 3590 HL7 suite expressions and outputs, 565 ELM-corpus definitions over no data, 2051 eCQM
  definitions over the 76 patients — 0 differ.
- **What depends only on the library or the runtime is computed once** (behaviour-preserving; the
  outcome of every eCQM, ELM-corpus and suite result was checked identical to 29b6bf8's):
  per library (`library-tables`): the function table by name, the first overloads, the definitions,
  the static code system and value set terms (make-rlib used to rebuild them for every patient with a
  quadratic `append`); per runtime (typed caches in the `runtime` struct): a type's resources, a
  code-filtered retrieve (by type and property, then the codes value's identity — a ValueSetRef's value
  is the library's shared term object; a missed identity only recomputes), a value set's `vocabulary`
  (members, and the code-only index built lazily); per rlib: the overload a signature picks;
  per evaluation: includes (`caching-resolver` in run.rkt — `evaluate-measure` and the eCQM harness use
  it; a resolver that re-read and re-parsed every include for every patient was 90% of a measure's
  time); the property path split, `simple-name`, the terminology lookup by url.
- **A retrieve's code property is resolved once against the model** (`code-path.rkt`): the data type and
  the code property fix the path's shape through the ModelInfo, so `resolve-code-path` builds a CodePath
  — a Reader per step (`field` key many?, `choice` of member keys, `coding`, `bare`) — and
  `code-path-codes` reads a resource's codes from its JSON with no model lookups or per-value type tests.
  The generic navigation (`navigate-path` + `node-codes` over `node-property`, exported as `generic-codes`)
  is the reference: no CodePath where the model does not fix the shape (an element it lacks, a System
  type, a `value` step — e.g. DeviceUseStatement.device.code through a Reference), and 'unshaped for a
  resource whose JSON the generic navigation would type differently (a value with its own
  `resourceType`, an object where a Coding's string element is declared, another resource type); both
  read generically. `choice-keys` (representation.rkt) is the one definition of a choice member's JSON keys.
  `tests/code-path-test.rkt`: 65 (type, property) pairs (every retrievable class's primaryCodePath and
  every code-filtered retrieve in the eCQM and ELM-corpus libraries), 64 resolved, over the eCQM data and
  the R4 IG examples in fhir-spec plus mutants of each resource's code element (null, scalars, lists,
  resourceType-bearing values, malformed codings): 19472 compared, 0 differ.
  Measured on the 76 eCQM patients with the data read up front (engine only): the engine before this work (29b6bf8)
  took 3.3 ms/patient with includes re-read per patient and 0.9 with them cached; 0.47 with the caches above,
  0.32 with CodePaths. The closure compiler alone is ~2x on the isolated eval loop,
  ~5% on the eCQMs (their time is retrieval, FHIR navigation and terminology, not dispatch), and
  5-8% slower on the HL7 suite end to end, where each expression is compiled to be run once.
  `tests/bench.rkt` times all three under both evaluators.
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
  operand recognised as a duration (context-sensitive, like the reference grammar). The **3 remaining
  misses are irreducible corpus edges** (the interval-phrase grammar closed a 4th): `Floor(±2^31)`
  expects out-of-range literals to evaluate while ValueLiteralsAndSelectors requires the SAME literals
  to error (corpus self-contradiction, 2); one standalone uncertainty bound (`[17,44]`) contradicts
  the same corpus's arithmetic convention (`[16,44]`-derived, 1). `cql/tests/coverage.rkt` reports the
  live per-suite/failure-bucket breakdown each iteration; `cql/tests/vendor/cql-smoke.xml` is the
  harness's own self-test (not part of the conformance number). `raco cql <file.cql>` prints ELM JSON.
- **Real-library corpus** (`tests/elm-corpus-test.rkt`, `tests/elm-corpus-harness.rkt`): 32 CQL
  libraries from published packages (fhir.cqf.common 4.0.1, hl7.fhir.us.cqfmeasures 5.0.0,
  hl7.fhir.uv.cql 2.0.0 — `tests/vendor/elm-corpus/SOURCE.txt`), each with the reference cql-to-elm
  translator's ELM; per library: parse, compile, and per-`define` ELM structural equality (translator
  metadata dropped). **32/32 parse; 32/32 compile; 786/803 definitions ELM-equal** — ratcheted. The
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
- Remaining ELM differences (17 definitions, ratcheted; `tests/elm-corpus-test.rkt` prints them
  CLASSIFIED by the nodes at the first difference — what the reference wrote and what we wrote there,
  `Union | Retrieve`, `FunctionRef:ToConcept | Property.requestStatusReason` — largest class first,
  with examples: the work list): EXM108's properties of a value typed as a choice of resources, and
  its lists of mixed resource types through `Coalesce` and `in`; `condition.onset during
  encounter.period` where the two packages' translators pick DIFFERENT choice members (CQF Measures:
  Period; the CQL IG: dateTime); `[MedicationAdministration: "VS"]` in the CQL IG's Example, which the
  newer translator unites with the records referencing a coded Medication (2026-11-13 pass, below).
- The corpus harness normalises only translator-VERSION noise, on both sides: empty lists = absent
  (newer translators write `let: []`, `dateFilter: []`), structural element `type` tags
  (InstanceElement, AliasedQuerySource, …), `strict: false` (the schema default), and around value
  sets — ValueSetRef's `preserve`, its `type` under InValueSet's `valueset`/Retrieve's `codes`,
  Retrieve's default `codeComparator` "in", and a FALSE `lowClosed`/`highClosed` (fhir.cqf.common's
  translator leaves it out; ours writes it) — the two packages disagree, so neither form is held;
  likewise an empty String literal's `value`, a Ratio's parts' `type`, and the ORDER of a
  ChoiceTypeSpecifier's choices (the reference's is a Java HashSet's iteration order).
  Includes resolve within the package, else from another corpus package (CQF Measures includes
  FHIRHelpers 4.0.1 but ships only its ELM; the source is in fhir.cqf.common).
- **Execution over a model's data** (`run.rkt`, `representation.rkt`, interp's runtime libraries): data
  are `model-node`s (model + class + JSON, `value.rkt`) navigated through the ModelInfo and the model's
  representation (`node-property`: element types, choice elements by their keys, list elements, a
  primitive's scalar element is the System value) — see "Any data model" below. `load-runtime-library` compiles a library and its includes into `rlib`s (memoised
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
- **Not implemented** (the manual's Limits say the user-facing ones): ELM as XML; a retrieve's date
  range and profile conformance (only type and code filter); `collapse … per` applies no `per`; a code
  element followed through a reference (`[MedicationRequest: "VS"]` for a `medicationReference` — the
  2026-11-13 pass, below); FHIR Measures beyond patient-based proportion scoring; FHIR versions other
  than 4.0.1. (This list once named retrieves, queries, terminology membership and canonical temporal
  literals as deferred: all four are implemented — a `@`-literal has been a Date/DateTime/Time
  constructor node since the HL7 suite work.)
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
  when it is beside; a qualified `C.X` through its alias), completion of declared names and keywords.
  The language server loads it by collection name.

- **Workspace support** (`workspace.rkt`, `fhir/lang/workspace`; served by fhir-mcp): artifacts = a
  Library per `.cql` file (text/cql content) with every translation error, a FHIR-version mismatch at
  the `using` line (the model here is 4.0.1), and a library name declared by two files; `run` = every
  define (or `entry`) for every patient of the data source (`over` Patient/id), values as JSON (a
  resource as "Type/id"), per-define tallies; ad-hoc text that is not a library is wrapped as
  `define "Query"` (FHIRHelpers included); parameters are CQL expressions; `explain` = one patient's
  define, recursively through the defines it references (an included one's value via `rlib-eval`),
  with each retrieve's type, filter, line, how many resources of the type the patient has and which it
  kept. Includes resolve: the workspace's .cql files, then registered/data-source Libraries (searched by
  name on a server), then `libraries/` (run.rkt's `bundled-library`: `<Name>-<version>.cql`, else
  `<Name>.cql` — FHIRHelpers 4.0.1 and 4.4.000, SOURCE.txt). It also provides `guide` (below). Value sets: the
  workspace's (registered; expanded by $expand when not enumerated), then the data source (by url, lazy;
  a server that cannot search ValueSet holds none), then the packages (`measure.rkt`'s
  `packages-valuesets`, now exported). `interp.rkt` exports `rlib-eval` (one ELM node in a library's
  context).

- **Any data model** (2026-11-08). The engine names no model: `tests/model-neutral-test.rkt` fails if
  compile/interp/run/representation/code-path/value or the model modules name FHIR, its elements or
  FHIRHelpers in code. A data model is two pieces of DATA:
  - its **ModelInfo** (`model/modelinfo.rkt`): classes, elements, bases, primaryCodePath, retrievability,
    conversions, contexts, and now the parts the old loader dropped — `requiredModelInfo` (a model built
    on another: classes, conversions and contexts are looked up through it), `contextRelationship` (the
    Patient compartment: FHIR's Observation relates by `subject` and `performer`; a relationship may
    name a search, whose path is used without its `where(...)` filters), element `target` mappings
    (compiled, see "Other models" below; a mapped element read directly from data is still an error,
    since reading nothing would be wrong). `model/modelinfo-xml.rkt` reads any ModelInfo document;
    `gen-modelinfo.rkt` writes it as a ModelForm `.rktd`. A registry finds models: the shipped files
    (`shipped-model-files`: FHIR 4.0.1, model and representation as a runtime-path pair),
    `register-model!`, `current-model-providers` (made on request — the workspace's), and
    `current-model-directories`/`CQL_MODEL_PATH` (ModelInfo `.xml`/`.xml.gz` or `.rktd`). Models are
    equal only to themselves (identity hashing: keying caches by a transparent model made the eCQMs
    46x slower until it was fixed). The FHIR 4.0.0 alias was dropped: nothing used it.
  - its **representation** (`representation.rkt`, a settings list, e.g. `model/fhir-4.0.1.representation.rktd`):
    the type key (FHIR: resourceType), the choice encoding (suffixed, with per-type `choice-suffixes`
    for a logical model's `whenPeriod`/`whenString`/`whenCode`, or plain), the scalar element (FHIR's
    primitive `value`), the reference style (typed "Type/id", or key), and the `shapes` of System values
    written as objects (interval low/high or start/end, a Concept's codes/display or coding/text, a
    Quantity's unit keys). A model without one inherits a required model's, else the default.
  Codes come from the model's conversions: a class converting to System.Code holds a code
  (`code-class?`), one converting to System.Concept lists them in its element that is a list of such a
  class (`concept-codings-element`) — FHIR's Coding and CodeableConcept, derived, not named. A retrieve
  resolves its model from the library's `using` (url and version, `rlib-models`); the retrieve source
  is asked `(class-name model)` (one-argument sources still work); `resources->data` scopes a patient's
  records by the model's Patient compartment (any relationship naming the patient; a record naming no
  patient is everyone's), `patient-data` does so for one known key. `eval-is` now decides
  `Interval<T>`, `List<T>` and choice types (it answered false for every non-named type).
  `model/structure-definitions.rkt` makes a model from logical StructureDefinitions (FSH `Logical:`):
  a class per model, nested classes for BackboneElements, FHIR datatypes as System types (no FHIRHelpers
  needed), `Reference` as `<Model>.Reference`, the Patient context and its relationships from the
  References to the named patient class (matched by canonical or name — FSH writes FHIR's canonical for
  a name it also knows). The workspace makes such models from workspace.yaml's `models`
  (`Clinic: <canonical> patient=Member birthDate=born version=1.0`) and runs over that model's patients.
  Held to: every outcome of the old engine — 3608 suite expressions and outputs, 565 corpus
  definitions, 2051 eCQM definitions, 76 eCQM populations — identical (a canonicalised snapshot,
  diffed); `code-path-test` unchanged (19472 compared, 0 differ); `tests/any-model-test.rkt` (a clinic
  model from logical models and one from a ModelInfo XML in a model directory, each with its own data
  shape); fhir-mcp's workspace test (a model designed in FSH, queried by `using`, explained).

- **Other models: QI-Core, US Core, QDM** (2026-11-08). Shipped (`model/models.rktd`, an index of name,
  version, url, ModelForm file and representation — a model loads only when asked for, by name or by
  url): FHIR 4.0.1; QI-Core 4.1.1/5.0.0/6.0.0 and US Core 3.1.1/6.1.0 (the reference translator's
  ModelInfos, Apache-2.0, `model/SOURCE.txt`); QDM 5.0.2–5.6.
  - **Mapping models** (a ModelInfo with a targetUrl: QI-Core, US Core). A library is typed by the
    profiles and its ELM reads the target model, as the reference translator writes it (compile.rkt,
    "mapping models"): `using` emits the target's url; a retrieve names the target class
    (`{http://hl7.org/fhir}Observation`) with the profile as templateId; a type in the ELM names the
    target's class; an element with a `target` mapping (`mapping.rkt` parses the little language:
    alternatives `Type:term;…`, `Lib.Fn(term)`, `%value`/`%parent` paths, `[cond,…]` filters) is the
    mapping's ELM — a filter is a Query over `$this` with `Equal(FHIRHelpers.ToString($this.url), …)`
    (First at each list step of a condition path), a single element over a list is SingletonFrom, a
    function of a list element maps each item; the same at a sort row (`sort by issued.earliest()`)
    and over a list's items. A code element that maps a System.ValueSet alternative as well
    (QI-Core 4.1.1's NotDone profiles) retrieves as `Union(Retrieve in VS, Retrieve ~ VS)`; at run
    time the `~` half reads the profile's ValueSet alternative — the profile found by templateId among
    the installed mapping models (`find-profile-class`), its path walked over the FHIR record — and
    matches the value set's url (interp.rkt `mapped-value-set-reader`). Typing details the corpus
    drove: a class based on a System type (`QICore.UnitsOfTime` on System.String) is its subtype; a
    System value promotes into a choice parameter (`ToDateTime(As(x, Date))`); a choice argument whose
    members either are parameter members or fit not at all is a Case over the members that are
    (System types only — FHIR members keep the plain cast); a choice compared with a type reads the
    member that IS it; an aggregate's result has its `starting` value's type.
    Language features the corpus needed: `$this` in `sort by`; `X in day of Y` (membership with a
    precision = included-in); a query of sources alone as a closed term (the left operand of `union`);
    `difference/duration in … between` as terms (not primaries: `… and E.f()` read as `(…).f()`); a
    block comment ending at its first `*/` (`/***/` swallowed definitions); `convert X to days` =
    ConvertQuantity(X, 'd') (the interpreter converts through UCUM, calendar words as their units); a
    System Quantity's `value`/`unit` and a Ratio's parts as properties; DateFrom/TimeFrom without a
    precision; sort directions kept as written (`ascending`); DateTime's timezone offset promoted to
    Decimal; model instances equal when of one class with equal data (CQL's structural equality; the
    FHIR snapshot is unchanged).
    `tests/qicore-elm-corpus-test.rkt`: 98 libraries of ecqm-content-qicore-2024 (CC0), 98 compile,
    1854/2144 definitions ELM-equal (was 409 before the mappings; 1932 since the 2026-11-13 pass). `tests/qicore-measure-test.rkt`: 4
    eCQMs over 190 MADiE cases, 188 agree (2 named: values exported as concepts where the case
    describes a percentage). `tests/mapping-model-test.rkt`: NotDone both ways, ConvertQuantity, US Core.
  - **QDM** (cqm-models JSON). `model/qdm.representation.rktd` declares it, with four representation
    settings added for it (representation.rkt, all data): `type-prefix` (`_type` = "QDM::Diagnosis";
    a name that is a System type — QDM::Code — is that System value), `element-keys` (an element's
    keys in order: `code` under `dataElementCodes` else `code`, `id` else mongoose's `_id`; a
    single-valued element written as a list is its first item, while a retrieve's code filter sees
    every item — `node-property-all`, cqm-models' getCode), `code-system-prefix` (bare OIDs read as
    `urn:oid:`), and `profiles` (a record of class B is the model's NegativeB/PositiveB when
    `negationRationale` is present/absent — cqm-models' getByProfile). Records answer retrieves of
    their superclasses. Robustness the data needed: DateTimes with a `+0000` offset; a record of a
    class the model version lacks, where a System type is declared, read as that type (QDM 5.5's
    DiagnosisComponent where 5.3 declares a Code; a Code written as an object holding its code object).
    run.rkt: `valuesets->terminology` takes cqm-models value sets (`{oid, concepts}`, also as one JSON
    array); `resources->data` takes a cqm-models patient document; `read-json-files` takes an array
    file's objects. `cqm.rkt`: `cqm-patient-records`, `cqm-measure-period`, `cqm-measure-populations`
    (proportion, ratio, CV, cohort; patient- or episode-based; strata; CV observations over
    MSRPOPL−MSRPOPLEX in episode order; ratio observations one per observation per IPP episode, null
    outside its population). `rlib-call` (interp.rkt) applies a library function to values.
    `load-runtime-library` translates with `compile-library/partial`: a definition that fails to
    translate raises its error when evaluated, and the rest of the library runs (the reference
    translator's recovery leaves Null there; an error is more honest). `raco cql … --parameter
    "name=<CQL expression>"`.
    `tests/qdm-elm-corpus-test.rkt`: cqm-execution's 42 fixture libraries (Apache-2.0), 41 compile,
    555/723 ELM-equal (the reference ELM is from older translators; 559 since the 2026-11-13 pass). `tests/qdm-measure-test.rkt`: 33
    patients of 19 measures; all 31 of cqm-execution's own enabled assertions hold (transcribed with
    their line numbers); 16/26 agree with the stored expectedValues, the 10 named (QDM 5.3 measures
    reading principalDiagnosis over 5.5 data — cqm-execution disables those tests; stale observations;
    a stored stratum the data contradict; a definition the reference translator could not translate).
  Held to: the FHIR snapshot of every engine outcome (6300) unchanged; the FHIR ELM corpus 770 (+1, the
  DateFrom precision); the HL7 suite 1820/1823.

- **Documentation for users and agents** (2026-11-08; the pattern for the family's languages, see the
  main repo's CLAUDE.md). `guide.md` is the language for agents — cheat sheet, worked example,
  running it, the data models, common problems — provided by `cql/workspace` as `guide` (fhir-mcp's
  `learn://cql`); `tests/guide-test.rkt` translates every ```cql block (with the bundled libraries)
  and checks the claims (the worked example's answer; the choice-timing trap: `O.effective during P`
  is null for an `effectiveDateTime`, as the reference translator casts the choice to Period — the
  guide's first cheat sheet had that very bug, which is why its claims are tested).
  `.claude/skills/cql/SKILL.md` is the workflow without MCP (every command in it was run).
  `scribblings/cql.scrbl` is the manual; `tests/docs-test.rkt` renders it and fails on a broken
  reference to its own sections or `cql/…` bindings (it once stopped reading at a `|` inside
  `@racket`, unnoticed). Writing them fixed: the CLI and a `#lang cql` module now find includes
  beside the file, then the bundled libraries (`directory-resolver`; a module that included
  FHIRHelpers could not run before); `cql-value` (a CQL expression's value, for parameters) backs
  `--parameter`.


- **Claude as the user** (2026-11-09; the main repo's CLAUDE.md). `compile-library/diagnostics`
  returns warnings beside the errors (`warn!`, positioned at the definition): the choice-timing trap
  (`O.effective during P` reads only the Period member — null for an effectiveDateTime) says which
  member to cast to; guide-test pins the warning at its line. Unknown identifiers and functions
  suggest the nearest names (`ctype->string` prints types as CQL writes them).

- **Translated when it compiles** (2026-11-09): the `#lang cql` reader translates every definition
  (includes beside the file, then bundled): an error fails `raco make` at its define, warnings print.
  A module's `evaluate`/`results` raise an `exn:fail` (positioned when the error knows where) instead
  of the raw `cql-error` struct. `render` prints a model's data (a FHIR Quantity, a resource) as JSON.
  guide.md has the latest-value-per-patient recipe, tested.

- **New users, round 3** (2026-11-09): `current-evaluation-time` (cql/value; a DateTime string or a
  moment) is what `Now()`/`Today()` read when set — `evaluate #:now`, `raco cql --now`; `evaluate
  #:define name` answers one define, a missing name with the nearest; a terminology reference to a
  codesystem/code the library does not define is a translation error at its definition (`known-term`;
  the TCK, ELM, eCQM and QDM corpora are unchanged by it). `tests/module-test.rkt`.

- **New users, round 4** (2026-11-09): ad-hoc workspace queries include the workspace's libraries by
  name and report their translation error about the query text (`query-includes`, `query-error`);
  `Combine` joins model primitives by their scalar value (`scalar-of`; the reference translator would
  convert the list elements in the ELM — ours does not yet); the latest-value recipe gained
  `Latest Number`/`Latest Unit`/`Name`, tested.

- **New users, round 6** (2026-11-09): an unknown qualified name suggests the nearest of that library's
  names as written (`` `Diabetes."Latest Value"` ``); a call to a function an included library lacks is
  `unknown function Lib.F (did you mean …)`, not "no overload … matches"; the recipe has "No HbA1c".

- **New users, round 7** (2026-11-10): the workspace's `explain` leaves out a define's `retrieves`
  and `uses` when it has none.

- **New users, round 9** (2026-11-10): `Take`/`Skip` with one argument (a query that read `, 2` as a
  second sort item) is an error that says to parenthesize the query, not a `cadr` crash; guide.md has
  recipe variations (a window, the last two, their mean), tested; explain's `of-type` is `patientHas`.

- **New users, round 10** (2026-11-10): the workspace's `explain` names a retrieve's code filter by its
  codes (`code ~ http://loinc.org|4548-4`, the codes evaluated with `rlib-eval`); guide.md shows a
  `#lang cql` file and the error Racket gives without the line.

- **New users, round 12** (2026-11-10): guide.md's latest-value recipe says how to adapt it to another lab
  (code, comparison, units) and gives the yes/no table a person reads (SQL over the run).

- **New users, round 15** (2026-11-10): a translation error about a name (`unknown function
  'AgeInYear'`, `unknown identifier 'Foo'`) is positioned at the name inside its definition, not at
  `define` (`at-the-name` in error.rkt, from the library text: `raco cql`, the `#lang cql` reader,
  support's diagnostics, the workspace's artifacts); `raco cql --now` takes a date.
  `tests/name-position-test.rkt`.

- **New users, round 16** (2026-11-10): the latest-value recipe has `"Above 9"` (`never tested`/`yes`/`no`,
  the table a person reads, from the library — no SQL CASE); guide-test checks it both ways. `#:now`
  takes a date in the guide's words too.

- **New users, round 17** (2026-11-10): a workspace run lists patients in id order, and a define answering a
  few texts (at most six, besides nulls) is tallied by text (`tally`), not as `other`.

- **New users, round 18** (2026-11-10): a day its month lacks is no date — `@1940-02-30` is a translation
  error at its define (`calendar-day`, compile.rkt), `Date(1940, 2, 30)` a run-time error
  (`check-date-parts`), `ToDate('1940-02-30')` null (`on-calendar`); guide.md has `Ever Above 9`.
  `tests/calendar-test.rkt`. TCK 1820/1823 unchanged.

- **New users, round 19** (2026-11-10): a workspace run summarises a define answering numbers by
  `numbers`/`min`/`max`/`mean` (`tally`); `cql-null` prints as `#<cql:null>` (value.rkt).

- **New users, round 21** (2026-11-11): a model value meeting a System operator when the library does not
  include the library that converts it (`Patient.gender = 'female'` without FHIRHelpers) is a
  translation error naming the include (`to-system*`, compile.rkt); it compared the raw FHIR value and
  answered false for a female patient. Inside the converting library itself nothing is said; the version
  in the message is the model's (model-neutral). `AgeInYears()` needs no include. `tests/helpers-test.rkt`.

- **New users, round 22** (2026-11-11): a workspace run summarises a define answering Quantities by
  `quantities`/`min`/`max`/`mean` and `unit` (`tally`); the latest-value recipe has `Ever Above 9` beside
  `Above 9`, tested both ways.

- **New users, round 23** (2026-11-11): `current-evaluation-time` given something that is not a date or a
  DateTime is a one-line user error; the recipe's `Latest Result` (value and unit as text) replaces
  `Latest Unit`; the workspace's `explain` explains each define once (`explained: "above"`).

- **New users, round 25** (2026-11-11): `current-evaluation-time` given something that is not a date says
  how to write one (`write it as 2026-01-01, or 2026-01-01T12:00:00Z`).


- **New users, round 30** (2026-11-11): a Quantity literal's unit is UCUM or a calendar word, else a
  translation error at its define with what to write (`ucum-unit` in compile.rkt, through fhirpath/ucum's
  `ucum-valid?`/`ucum-suggestion`): `'mL/min/1.73m2'` translated clean and every comparison with it was
  null. The latest-value recipe's `Above 9` says `unknown` for a null comparison (it said `no`).
  TCK 1820/1823 unchanged. guide-test checks both.

- **New users, round 31** (2026-11-11): a workspace run's summary has `notes` naming a define null for
  every patient, with the quantity units its text compares with (`all-null-notes`,
  `define-quantity-units`); guide.md lists the recipe's HbA1c-named defines to rename for another lab.

- **A library states what it needs** (2026-11-13): `requirements.rkt` (`library-requirements`, pure
  over the ELM as JSON) reads a translated library's includes, codesystems and valuesets as
  `depends-on` relatedArtifacts and its retrieves as DataRequirements (type, templateId as profile,
  the code filter by value set or by the code a CodeRef names; statements in order, a node's keys
  by name, so the answer is deterministic). The workspace's Library resource carries them when the
  library translates (`library-resource … elm`); an included library's canonical is the
  workspace's for a workspace file, a stored Library's url, else `<the model's url>/Library/<name>`
  (model-neutral: `http://hl7.org/fhir/Library/FHIRHelpers`). fhir-mcp's dependency tracing and
  `$data-requirements` read them from the resource.

- **New users, round 32** (2026-11-12): a non-UCUM unit's translation error is at the quoted unit
  (`at-the-name` reads `'<u>' is not a UCUM unit`); `mEq/L` suggests `meq/L` (fhirpath/ucum's spellings).
  `tests/name-position-test.rkt`.

- **A subject the data does not hold** (2026-11-13): `evaluate-measure #:subject "Patient/nobody"` is
  a `not-found` operation error; it answered an individual MeasureReport of zeros, which read as
  "evaluated, not in the initial population". `tests/measure-operation-test.rkt`.

- **Quality, performance and spec alignment pass** (2026-11-13). Held to a dump of everything the
  repository produces over its corpora (the ELM, errors and warnings of the 172 corpus libraries; the
  ELM and value of the HL7 suite's 3,608 expressions and outputs; every define of the eCQMs over the
  76 patients; the QI-Core and QDM measure cases): every value, error, warning and population is the
  one it was; the ELM changed only where it is said below. Tests 713 -> 850.
  - **The ELM, aligned with the reference translator** (each shape pinned in
    `tests/elm-shape-test.rkt`; FHIR corpus 770 -> 786/803, QI-Core 1854 -> 1932/2144, QDM 555 ->
    559/723). A union chain of seven or more operands was regrouped a second time on the way down
    (`regrouped`, a weak table marking only the top node): a chain is now made a UnionTree once
    (`union-tree`, `compile-union`, `union-nodes`). `"VS" contains x` compiled to Contains(ValueSetRef, x)
    and FAILED when run ("In: not a list or interval"): it is InValueSet, the value set also as
    `valuesetExpression` (`compile-membership`, `elm-in-valueset`'s `as-expression?`). Expand and
    Collapse are binary (a Null `per`, `with-default-operands`); a point beside an interval under an
    operator that relates intervals is `if P is null then null else Interval[P, P]`
    (`point->interval`); `/` converts Integer and Long operands (ToDecimal); a multi-source query
    without a return returns, distinct, the Tuple of its aliases; an aggregate's accumulator is a
    QueryLetRef; list elements meet at one type as if/case branches do, and `List<T> { … }` holds Ts
    (`cql-typed-list`: the type was dropped); branches of different types meet at their choice, each
    cast to it (`branch-choice`; not among tuples); a choice among Coalesce's arguments is read through
    the member that converts; Combine's list of model strings is converted item by item; Skip, Take
    and Tail are Slices, function or method (`slice`: ELM has no operators of those names — the
    interpreter's are gone, a null count takes nothing either way; a NEGATIVE count is now Slice's
    index from the end, as the HL7 suite defines Slice, where `Take(X, -1)` was empty and
    `Skip(X, -1)` everything — the specification's prose says empty for both).
  - **The ELM JSON, aligned with the schema** (elm/json.rkt): a Quantity's `value` is a JSON number
    (`decimal-json`; the lexeme only where no double holds it — the corpus comparison's number
    normalisation had hidden the string); a Ratio's parts carry no `type`; an operator without
    operands has no `operand` element (`Today`); a Query's `aggregate` clause is written (it was
    dropped: an aggregating query read, in JSON, as one returning its rows).
  - **A unit is a string or a calendar word** (cql.g4's `unit`): `5 mg` was read as `5 'mg'`; it is a
    parse error at the word (`quantity-unit`, `exn:fail:bare-unit`, `bare-unit-token`). guide.md's
    common problems has it, guide-test checks it.
  - **The corpus report says what differs** (`tests/elm-corpus-harness.rkt`): a mismatch is classified
    by the nodes at its first difference (`difference`, `node-name`: `Union | Retrieve`,
    `FunctionRef:ToValue | Property.performed`), keys compared in name order; it was the JSON path.
    Not held, with the reason in the harness: an empty String literal's `value`, a Ratio's parts'
    `type`, the order of a ChoiceTypeSpecifier's choices (the reference's is a Java HashSet's
    iteration order: the bucket of each class name's hash — it fits the six pairs checked).
  - **Parsing** (parser/parse.rkt; it is 95% of translating: 60 µs a token, quadratic in a list's
    elements). A definition is parsed once (`chunk-cache`, keyed by its tokens' types and values:
    `chunk-key`, `parsed-chunk`; each use gets fresh definition structs over the shared bodies, so
    each keeps its own position): FHIRHelpers 555 ms, then 8 ms after an edit to one definition or
    under another source name; the QI-Core corpus 16.4 -> 12.5 s. A definition that is one long list
    is parsed element by element (`parse-long-list`, lists of 50 elements and more; any element that
    fails is left to the whole parse, so errors are unchanged): OpioidData's 1,000 tuples 27 -> 5 s,
    the QDM corpus 29.3 -> 6.9 s. The three caches are bounded (`cached`: emptied at their limit) —
    a language server parsed every keystroke's text into a table that only grew.
  - **Translating** (compile.rkt; QI-Core corpus 620 -> 390 ms): `ctype->elm-name` built three regexps
    per type name (a fifth of the time); `parse-mapping` is memoised (mapping.rkt); `make-lctx`'s
    overloads were appended quadratically.
  - **A stale translation, fixed** (run.rkt `translation`, `include-closure`, `same-libraries?`): the
    compiled ELM was cached by the library's AST alone, so in a long-lived process an included
    library edited between two runs left the includer running ELM typed by the old one (a call kept
    naming F(Integer) after the argument became a Decimal). It is reused while the include closure
    is the same objects. workspace.rkt's `built-models` kept a model per edit of a logical model.
  - **Quality**: one reading of a model's conversion (`conversion`, `type-conversion`, `converted`:
    it was written three times), `member-spec`, `assemble/collecting` (the accumulating compiles'
    shared half), one list of calendar units, `system-ops` without its repeated keys, no `set!` of a
    parameter; workspace.rkt's Tally stated and split (`tally`, `number-tally`, `basic-tally`);
    error.rkt's `at-the-name` as `mentioned`, `definition-body`, `line+column`, with the tests it
    lacked; requirements.rkt's code filter written once; interp.rkt's `in-library`/`rlib-env`,
    `run-library` without its dead table; 50 unused imports; stale comments (tests/run.rkt, the
    "reduced Query shape" note, the "Deferred CQL slices" list above).
  - **Decided against / left** (TESTING.md and the manual's Limits say the user-facing ones):
    `[MedicationRequest: "VS"]` followed through a `medicationReference` — the reference translator
    unites the retrieve with `[T] MR with [Medication] M such that M.id = Last(Split(MR.medication.reference, '/'))
    and M.code in "VS"`. Written, model-neutral (a representation setting, `code-references`), and
    measured: QI-Core +28 definitions, FHIR +1 — and one MADiE case (Controlling High Blood Pressure,
    `DENEXPass DementiaMedAsReferenceCQLIT412`) then answers the exclusion its NAME expects and its
    stored report does not (the report was recorded by an engine that did not follow the reference).
    Lowering 188/190 is the owner's call, so it is not in. What it takes: FHIR's representation
    declaring `(code-references ("medication" . "Medication"))`; in `compile-retrieve`, for such an
    element whose choice has a member with a `reference`, the Union with that query (a mapping model
    reads `id.value` and `medication.reference.value`, FHIR converts with FHIRHelpers.ToString; a
    code filter is `Equivalent(ToConcept(M.code), ToConcept(code))`); and the corpus harness comparing
    fhir.cqf.common and CQF Measures 5.0.0, whose translator predates it, without the query.
    A UsingDef's `version` for a mapping model (the reference
    writes the ModelInfo's targetVersion, i.e. none; the runtime reads ours to find the model);
    `includes.path` and `identifier.system` with a namespace (the translator's options, not the
    library's text); a property of a choice of resources resolved per member; `collapse … per`;
    Round/Ln/Ceiling over Integers (Decimal-only in the reference: no corpus evidence); a warning
    when the data's FHIR version is not the library's (FHIR JSON does not state its version; a
    workspace already refuses the mismatch).
