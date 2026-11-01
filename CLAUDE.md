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
  metadata dropped). **32/32 parse; 32/32 compile; 742/803 definitions ELM-equal** — ratcheted. The
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
- Remaining ELM differences (61 definitions, ratcheted): translator-version noise the normaliser does
  not cover (a newer translator writes InValueSet's valueset without `type` + `preserve`, older ones omit
  `codeComparator` on explicit code paths — the two packages disagree, so neither form matches both);
  a choice operand against a timing phrase whose other side is an interval (the reference converts via
  the Period member); `Interval[…].low` re-typing in FHIRCommon's age/range intervals; ratio/quantity
  instance details in FHIRHelpers; EXM108's day-of-procedure phrases (16 defs). `cql/tests/
  elm-corpus-harness.rkt` + the scratch differs in the session are the tools: group mismatches by
  first-difference path, then fix the largest class.
- The corpus harness normalises only translator-VERSION noise, on both sides: empty lists = absent
  (newer translators write `let: []`, `dateFilter: []`), structural element `type` tags
  (InstanceElement, AliasedQuerySource, …), and `strict: false` (the schema default). Includes resolve
  within the package, else from another corpus package (CQF Measures includes FHIRHelpers 4.0.1 but
  ships only its ELM; the source is in fhir.cqf.common).
- **Deferred CQL slices**: retrieves (`[Condition: "vs"]`) + a FHIR data-source/retrieve layer (absent —
  `navigate.rkt` assumes in-memory roots), queries (`with`/`where`/`return`/`sort`/`aggregate`),
  terminology-backed membership via the `tx-service`, FHIR model-info from `full-field-specs`, ELM XML
  output, temporal literals as canonical ELM Date/DateTime/Time constructor nodes (slice-1 emits them as
  a Literal carrying the lexeme — round-trips through our interpreter), and full agreement with the reference
  translator's ELM (the corpus above measures it).
