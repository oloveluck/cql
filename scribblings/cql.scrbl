#lang scribble/manual
@(require (for-label racket/base racket/contract
                     cql/parser/parse cql/compile cql/elm/json cql/interp cql/run cql/measure
                     cql/value cql/error cql/model/modelinfo cql/cqm cql/workspace))

@title{cql: Clinical Quality Language}

@hyperlink["https://cql.hl7.org/"]{CQL}, compiled and run. You write a library in a @tt{.cql} file; it is
compiled to ELM (the standard machine form) and the ELM is run over one patient's JSON data:
retrieves, value set membership, queries, the implicit FHIRHelpers conversions, and measure
populations. The data model is whatever the library's @tt{using} names: FHIR R4, QI-Core and US Core
(views over FHIR), QDM (the cqm-models JSON of Bonnie, MADiE and cqm-execution), or a model you
describe (@secref["models"]). A @tt{.cql} file is a Racket module whose definitions a program
evaluates over a patient's data.

Held to published suites: cqframework/cql-tests 1820 of 1823; 32 published FHIR libraries compile,
770 of 803 definitions identical to the reference translator's ELM; the 98 libraries of the 2024
QI-Core eCQMs compile, 1854 of 2144 identical; four FHIR eCQMs agree with 71 of 76 test patients,
four QI-Core eCQMs with 188 of 190 test cases; QDM measures pass all 31 of cqm-execution's own
assertions. Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir} family.

@table-of-contents[]

@section{Quick start}

@bold{1. Install.} The packages are not on the Racket package server yet; clone the repositories
side by side and install from source:

@verbatim{
raco pkg install --link --auto fhir/fhir fhirpath cql
}

@bold{2. Write a library.}

@verbatim|{#lang cql
library BloodPressureScreening version '1.0.0'
using FHIR version '4.0.1'
parameter "Panel Codes" List<System.String> default { '85354-9', '55284-4' }
context Patient

define "Readings":
  [Observation] O where exists (O.code.coding C where C.code.value in "Panel Codes")

define "Systolic Values":
  "Readings" O return singleton from (O.component C
                                        where exists (C.code.coding K where K.code.value = '8480-6')
                                        return (C.value as FHIR.Quantity).value.value)

define "Highest Systolic": Max("Systolic Values")
define "Hypertensive": "Highest Systolic" >= 140}|

@bold{3. Run it over a patient's resources} (a directory of FHIR JSON files or Bundles, one Patient
among them):

@verbatim{
$ raco cql screening.cql --data patient/
Readings: { Observation(...), Observation(...) }
Systolic Values: { 152, 138 }
Highest Systolic: 152
Hypertensive: true
}

A mistake is reported on its line, naming the definition: @tt{screening.cql:12:1: cql compile error:
in define "Systolic Values": unknown function 'Mx'}. Without @litchar{--data}, @tt{raco cql screening.cql} prints
the ELM JSON.

@section{Writing a library}

A file is one library: a header, then definitions. @tt{using FHIR version '4.0.1'} loads the FHIR
model (other models: @secref["models"]), so @tt{[Observation]} retrieves resources and @tt{O.code.coding} navigates them;
@tt{context Patient} scopes retrieves to one patient; @tt{parameter} declares an input (with a
@tt{default}); @tt{include} brings in another library under an alias; @tt{valueset "Name": 'url'}
makes @tt{[Observation: "Name"]} a retrieve filtered by membership. Names with spaces are quoted
with @tt{"…"}, strings with @tt{'…'}. Without FHIRHelpers, FHIR primitives are navigated to their
@tt{.value}; a library that includes FHIRHelpers gets the implicit conversions the reference
translator makes. The language reference is @hyperlink["https://cql.hl7.org/"]{cql.hl7.org}.

A trap worth knowing: a timing phrase on a choice element reads only the member the translator
picks. In a FHIR 4.0.1 library, @tt{O.effective during "Measurement Period"} casts @tt{effective} to
a Period, so an Observation with @tt{effectiveDateTime} gives null; write
@tt{(O.effective as dateTime) during "Measurement Period"}. The repository's @tt{guide.md} (a
cheat sheet, a worked example and the common problems, each checked by its tests) has more.

@section{Using it}

@subsection{Command line}

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @tt{raco cql lib.cql} "the ELM JSON")
      (list @tt{raco cql lib.cql --data patient/} "run it; every definition as name: value")
      (list @tt{raco cql lib.cql --data bundle.json --define Hypertensive} "just the named definitions")
      (list @tt{raco cql lib.cql --data p/ --valuesets vs/} "ValueSet resources (with expansions) for the library's value sets")
      (list @tt{raco cql measure.cql --data p/ --libraries libs/} @elem{includes found as @tt{libs/<Name>.cql}; else beside the file; else the bundled FHIRHelpers 4.0.1 and 4.4.000})
      (list @tt{raco cql lib.cql --data p/ --parameter "Measurement Period=Interval[…]"} "a parameter as a CQL expression; repeat it")
      (list @tt{raco cql lib.cql --data patient.json} "a QDM patient document (cqm-models) is one patient's record too")
      (list @tt{raco cql --tck [-v]} "the cqframework/cql-tests suite, floor-ratcheted"))]

@litchar{--format text|json|outcome} chooses how errors are reported (all of them, not just the
first). Exit codes: 0, 1 on a CQL error or a suite regression, 2 on usage.

@subsection{As a module}

@racket[(require "screening.cql")] compiles the library when the program compiles; a parse or
translation error is a compile error positioned in the file. Its includes resolve to
@tt{<Name>.cql} beside the file, else to a bundled library (FHIRHelpers). The module provides @racket[evaluate],
the library over one patient's resources (the Patient among them), answering every definition's
value by name; @racket[(results)] for a library with no retrieves; and the library's @racket[elm].
Requiring the module also registers it as a Library resource, so measure evaluation finds it by name.

@racketblock[
(require (only-in "screening.cql" [evaluate screen]))
(screen (cons patient observations) #:parameters (hash "Panel Codes" '("85354-9" "55284-4")))
(code:comment "=> (hash \"Readings\" ... \"Highest Systolic\" 152 \"Hypertensive\" #t ...)")
]

The data is JSON in the library's model (FHIR resources, a QDM patient document).
@racket[#:valuesets] takes ValueSet resources with expansions (or cqm-models value set arrays).

@subsection{Values}

Integers, strings and booleans are Racket's; @tt{null} is @racket[cql-null]; lists are lists; a
Decimal is fhirpath's @racket[fp-decimal], a Quantity an @racket[fp-quantity], a date or time a
@racket[moment]; intervals, tuples, codes and concepts have their own structs
(@secref["cql-values"]). @racket[render] prints any of them the way the command line does.

@subsection{From Racket}

@racketblock[
(require cql/parser/parse cql/compile cql/elm/json cql/run fhir/result/result)
(define lib (ok-value (parse-cql text #:source "screening.cql")))
(elm->json-string (ok-value (compile-library lib)))                   (code:comment "the ELM")
(evaluate-library lib resources #:parameters (hash "Panel Codes" codes))
(code:comment "with includes found in a directory (then the bundled ones), parameters as CQL")
(define rl (load-runtime-library lib #:resolver (directory-resolver "cql/")
                                     #:data (resources->data resources)
                                     #:valuesets (valuesets->terminology value-sets)
                                     #:parameters (hash "Measurement Period"
                                                        (cql-value "Interval[@2024-01-01T00:00:00Z, @2025-01-01T00:00:00Z)"))))
(rlib-value rl "Hypertensive")
]

@subsection{Measures}

@tt{Measure/$evaluate-measure} runs a Measure's library over a data source holding the patients,
the Library resources and the value sets, and answers a MeasureReport:

@racketblock[
(require fhir/operation fhir/data-source cql/measure)
(parameterize ([current-data-source (directory->data-source "ecqm/")])
  (apply-operation "$evaluate-measure" measure
                   #:periodStart "2021-01-01" #:periodEnd "2021-12-31" #:subject "Patient/x"))
]

Over a server's store, fhir-server serves the same operation
(@tt{raco fhir-server evaluate-measure}).

A measure in the cqm-models format (a QDM measure document, as Bonnie, MADiE's QDM side and
cqm-execution hold it) is computed by @racket[cqm-measure-populations]: proportion, ratio, continuous
variable and cohort scoring, patient- or episode-based, with strata and observations, the way
cqm-execution computes them:

@racketblock[
(require cql/run cql/cqm)
(define-values (start end) (cqm-measure-period measure))
(define rl (load-runtime-library main-library #:resolver (directory-resolver dir)
             #:data (resources->data (cqm-patient-records patient) #:patient-context? #f)
             #:valuesets (valuesets->terminology (read-json-files dir)) #:offset 0
             #:parameters (hash "Measurement Period" (cql-value (format "Interval[@~aZ, @~aZ]" start end)))))
(cqm-measure-populations measure rl)
(code:comment "=> (list (hash 'IPP 1 'DENOM 1 'DENEX 0 'NUMER 1)): per population set, then stratum")
]

@section[#:tag "models"]{Data models}

A library's @tt{using} names its model. Installed:

@itemlist[
@item{@bold{FHIR 4.0.1}, the ModelInfo the reference translator ships. Include FHIRHelpers for the
implicit conversions between FHIR elements and CQL's types.}
@item{@bold{QI-Core} 4.1.1, 5.0.0, 6.0.0 and @bold{US Core} 3.1.1, 6.1.0: views over FHIR. A library
is typed by their profiles and its ELM reads FHIR, as the reference translator writes it: a retrieve
names the FHIR class with the profile as its template; each element with a mapping in the ModelInfo
reads through it (@tt{FHIRHelpers.ToConcept(C.verificationStatus)}, an extension or a component as
the query that finds it). The data is plain FHIR JSON. QI-Core 4.1.1's NotDone profiles match a
value set both ways: by code, and by the value set the action was not done for.}
@item{@bold{QDM} 5.0.2 to 5.6, over cqm-models JSON: a patient document's @tt{dataElements}, each
naming its class under @tt{_type}; a @tt{Positive…}/@tt{Negative…} retrieve selects the data
elements without or with a @tt{negationRationale}.}]

Any other model is two pieces of data: its ModelInfo (the reference translator's format) and its
@deftech{representation}, how its JSON looks. A representation is a settings file
(@tt{<model>.representation.rktd} beside the ModelInfo, in a directory named by @tt{CQL_MODEL_PATH}),
an association list of:

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @bold{setting} @bold{meaning} @bold{FHIR / QDM})
      (list @tt{type-key} "the key a record names its class under" @elem{@tt{resourceType} / @tt{_type}})
      (list @tt{type-prefix} "what precedes the class name there" @elem{— / @tt{"QDM::"}})
      (list @tt{choice} @elem{a choice element under name + type (@tt{suffixed}) or its own name (@tt{plain})} @elem{@tt{suffixed} / @tt{plain}})
      (list @tt{choice-suffixes} "per member type, the suffixes it is written with" "—")
      (list @tt{scalar-element} "the element a bare JSON scalar stands for" @elem{@tt{"value"} / —})
      (list @tt{reference} @elem{@tt{typed} (@tt{"Type/id"}) or @tt{key}} @elem{@tt{typed} / @tt{key}})
      (list @tt{shapes} "the keys of intervals, concepts and quantities written as objects" "defaults")
      (list @tt{element-keys} "the keys an element may be written under, first present wins" @elem{— / @tt{code} under @tt{dataElementCodes}})
      (list @tt{code-system-prefix} "what a bare code system is read with" @elem{— / @tt{"urn:oid:"}})
      (list @tt{profiles} "rules making a record of class B its profile <prefix>B" @elem{— / @tt{Negative}/@tt{Positive} by @tt{negationRationale}}))]

A model can also be made from logical StructureDefinitions (FSH @tt{Logical:}):
@tt{cql/model/structure-definitions}, which a workspace's @tt{models} setting uses. The engine names
no model: a test fails if its code ever does.

@section{In an editor, for an agent}

@tt{cql/support} is what the family's language server (fhir-lsp) loads for CQL: a library's
diagnostics (parse, then every definition translated), the libraries beside it as its project, the
declarations as an outline, hover, go to definition (an include alias leads to the library's file
beside it), and completion of declared names and keywords. It is a @tt{fhir/lang/support} value.
@tt{cql/workspace} is what fhir-mcp runs for an agent: a library run over every patient of the data,
and one patient's answer explained define by define down to the retrieves. It also provides the
repository's @tt{guide.md} as @racket[guide], which fhir-mcp serves as @tt{learn://cql}. Without MCP,
the repository's Claude skill (@tt{.claude/skills/cql}) gives an agent the same workflow with the
command line and the Racket API.

@section{Limits}

@itemlist[
@item{@bold{Data models.} No FHIR R5 ModelInfo. A retrieve filters by type and code;
@tt{dateProperty} and @tt{dateRange} are not applied, and a retrieve's template (profile) does not
filter records. The Patient context applies when the data holds exactly one patient, related through
the model's context relationships (FHIR: @tt{subject}, @tt{patient}, @tt{performer}…). A QDM measure
written for one QDM version reads data of another as it is: a QDM 5.3 measure's
@tt{principalDiagnosis} is absent from QDM 5.5 data.}
@item{@bold{Terminology.} Membership comes from the ValueSet resources supplied: their
@tt{expansion}, else enumerated concepts. Filters and nested value sets are not evaluated and the
terminology service is not called; a value set not supplied is an error.}
@item{@bold{FHIR Measures.} Proportion scoring, patient-based, from the first library and the first
group's population codes; @tt{Measure.scoring} is not read. No stratifiers, supplemental data or
measure observations. @tt{$evaluate-measure} reads @tt{measure}, @tt{periodStart},
@tt{periodEnd} and @tt{subject}. (cqm-models measure documents get every scoring, strata and
observations: @racket[cqm-measure-populations].)}
@item{@bold{Translation errors.} A definition that does not translate does not stop its library from
running: it raises its translation error when evaluated (the reference translator's error recovery
writes a Null there).}
@item{@bold{ELM} is emitted as JSON only. Of 803 definitions in the published FHIR corpus, 33 differ
from the reference translator in named categories, which @tt{tests/elm-corpus-test.rkt} prints
classified (choice operands in timing phrases, typed unions, ratio details in FHIRHelpers, one
measure's day-of-procedure phrases).}]

@section{Reference}

@subsection[#:style 'unnumbered]{@hash-lang[] @tt{cql}}
@defmodulelang[cql]

Provides @racket[library] (the CQL AST), @racket[elm] (the ELM AST), @racket[elm-json],
@racket[(evaluate resources #:parameters h #:valuesets vss)] and @racket[(results)]. Nothing runs when
the module is required.

@subsection[#:style 'unnumbered]{@tt{cql/parser/parse}}
@defmodule[cql/parser/parse]

@defproc[(parse-cql [str string?] [#:source source (or/c string? path? #f) #f]) (or/c ok? err?)]{
A library as @racket[(ok cql-library)], or @racket[(err cql-error)] with the diagnostic positioned
in @racket[source]. Libraries parse definition by definition and are memoized.}
@defproc[(parse-cql-expr [str string?]) (or/c ok? err?)]{One bare expression.}

@subsection[#:style 'unnumbered]{@tt{cql/compile}}
@defmodule[cql/compile]

@defproc[(compile-library [lib cql-library?]) (or/c ok? err?)]{@racket[(ok elm-library)], or the first translation error.}
@defproc[(compile-library/errors [lib cql-library?]) (values (or/c elm-library? #f) (listof cql-error?))]{
Every translation error, each positioned at its definition; the library only when there are none.}
@defproc[(compile-library/partial [lib cql-library?]) (values (or/c elm-library? #f) (listof cql-error?))]{
Every translation error, and the library with each failing definition replaced by one that raises
its error when evaluated, so the rest of it runs. What @racket[load-runtime-library] uses.}
@defproc[(compile-expr [ast any/c]) (or/c ok? err?)]{A bare expression to an ELM node.}
@defparam[current-library-resolver resolve (-> symbol? (or/c string? #f) (or/c cql-library? #f))]{
How @tt{include} finds a library's AST by name and version. @racket[#f] means unknown: references
into it compile untyped.}

@subsection[#:style 'unnumbered]{@tt{cql/elm/json}}
@defmodule[cql/elm/json]

@deftogether[(
@defproc[(elm->jsexpr [x any/c]) jsexpr?]
@defproc[(elm->json-string [x any/c]) string?]
@defproc[(library->jsexpr [lib elm-library?]) jsexpr?])]{
Canonical ELM JSON of a library (wrapped in @tt{{"library": …}}) or a bare node.}

@subsection[#:style 'unnumbered]{@tt{cql/error}}
@defmodule[cql/error]

@defstruct*[cql-error ([phase (or/c 'parse 'translate 'interpret)] [message string?] [diagnostic any/c])]{
The payload of an @racket[err]; @racket[diagnostic] is a @tt{fhir/diagnostic} or @racket[#f] (the
constructor's optional third argument).}
@defproc[(cql-error-string [e cql-error?]) string?]

@subsection[#:style 'unnumbered #:tag "cql-values"]{@tt{cql/value}}
@defmodule[cql/value]

@deftogether[(
@defthing[cql-null any/c]
@defproc[(cql-null? [v any/c]) boolean?]
@defthing[cql-null-list any/c]
@defproc[(cql-null-list? [v any/c]) boolean?])]{
The null singleton, and a null cast to a list type (@tt{null as List<T>}): still null, but
@tt{Length} sees an empty list.}
@defstruct*[cql-interval ([low any/c] [high any/c] [low-closed? boolean?] [high-closed? boolean?])]{Normalized to closed bounds at construction.}
@defstruct*[cql-tuple ([fields hash?])]{Symbol to value.}
@defstruct*[cql-ratio ([numerator fp-quantity?] [denominator fp-quantity?])]
@defstruct*[cql-code ([code any/c] [system any/c] [version any/c] [display any/c])]
@defstruct*[cql-concept ([codes list?] [display any/c])]
@defstruct*[cql-obj ([class string?] [fields hash?])]{An instance of another class, carrying its class for @tt{is}.}
@defstruct*[model-node ([model any/c] [type string?] [data any/c])]{A data model's value at a point in a record: its model, its class (@racket["FHIR.Observation"]) and the data there. Equal when of the same model, class and data.}
@defproc[(cql-value? [v any/c]) boolean?]
@defproc[(render [v any/c]) string?]{@racket[(render (list 1 2))] is @racket["{1, 2}"].}
@defparam[current-evaluation-offset minutes (or/c exact-integer? #f)]{
The evaluation request's timezone offset. A DateTime with a time and no offset takes it;
@racket[#f] leaves such values offset-less (what the expression suite expects).}

@subsection[#:style 'unnumbered]{@tt{cql/interp}}
@defmodule[cql/interp]

@defproc[(run-library [lib elm-library?]) hash?]{Every definition evaluated with no data: name to value.}
@defproc[(eval-elm [node elm-expr?] [env ienv?]) cql-value?]{One ELM node in an environment, by the
tree walker: a structural recursion over the node. It is the reference semantics.}
@defproc[(eval-elm/compiled [node elm-expr?] [env ienv?]) cql-value?]{The same, through the node's
compiled closure (@racket[compile-elm]). It gives the same value or raises the same error as
@racket[eval-elm] everywhere; @tt{tests/evaluator-test.rkt} holds the two to it over the HL7 suite and
both library corpora.}
@defproc[(compile-elm [node elm-expr?]) procedure?]{The node as a closure from an environment to a
value, compiled once per node and cached. Compiling never raises: an error is raised when the
closure runs, as the tree walker would raise it, so an untaken branch raises nothing.}
@defproc[(run-elm [node elm-expr?] [env ienv?]) cql-value?]{One node by the evaluator
@racket[current-elm-evaluator] names.}
@defparam[current-elm-evaluator evaluator (or/c 'compiled 'tree)]{Which evaluator libraries
(@racket[make-rlib], @racket[run-library]) and @racket[run-elm] use. The default is
@racket['compiled]; the environment variable @tt{CQL_ELM_EVALUATOR=tree} selects the tree walker for
a process.}
The semantics the operators rest on are the @tt{cql/interp/} modules — @tt{temporal} (durations,
boundaries, date arithmetic), @tt{arith}, @tt{compare} (ordering, equality, equivalence) and
@tt{interval} (relations, membership, set operations, expand/collapse) — each pure and tested alone.
@defproc[(make-ienv [#:defs defs hash? (hash)] [#:operands operands hash? (hash)]) ienv?]
@defproc[(runtime [data procedure?] [valueset procedure?] [offset (or/c exact-integer? #f)]) runtime?]{
What a library runs against: @racket[data] is @tt{type-name -> (listof resource)}, scoped to the
context patient; @racket[valueset] is @tt{url -> (hash (cons system code) -> #t) | #f}, where
@racket[#f] means unavailable, an error when needed.}
@defproc[(make-rlib [lib elm-library?] [includes hash?] [rt (or/c runtime? #f)] [parameters hash?]) rlib?]{
A library ready to run: definitions memoised, includes by alias, parameter values overriding the
library's defaults.}
@defproc[(rlib-value [rl rlib?] [name string?]) cql-value?]{A definition's value.}
@defproc[(rlib-eval [rl rlib?] [node elm-expr?]) cql-value?]{One ELM node in the library's context (its definitions, functions, includes and data).}
@defproc[(rlib-call [rl rlib?] [name string?] [args (listof cql-value?)]) cql-value?]{The library's function @racket[name] applied to values (a measure observation for one episode).}
@deftogether[(@defproc[(runtime? [v any/c]) boolean?] @defproc[(rlib? [v any/c]) boolean?] @defproc[(ienv? [v any/c]) boolean?])]
@defproc[(test-equal? [a any/c] [b any/c]) boolean?]{Value equality as the conformance harness compares results.}

@subsection[#:style 'unnumbered]{@tt{cql/run}}
@defmodule[cql/run]

@defproc[(evaluate-library [lib cql-library?] [resources (listof hash?)]
                           [#:parameters parameters hash? (hash)]
                           [#:valuesets valuesets (listof (or/c hash? list?)) '()]
                           [#:offset offset (or/c exact-integer? #f) #f]
                           [#:resolver resolver procedure? bundled-library]) hash?]{
The library over these resources, in the context of the Patient among them: every definition's value
by name. What @hash-lang[] @tt{cql}'s @racket[evaluate] is.}
@defproc[(load-runtime-library [lib cql-library?]
                               [#:resolver resolver procedure? (lambda (name version) #f)]
                               [#:data data procedure? (lambda (type) '())]
                               [#:valuesets valuesets procedure? (lambda (url) #f)]
                               [#:parameters parameters hash? (hash)]
                               [#:offset offset (or/c exact-integer? #f) #f]) rlib?]{
Compiles the library and, once each, the libraries it includes (found by @racket[resolver], a
@tt{name version -> cql-library | #f}), binding the include aliases. The main library's parameters
apply to its includes. A definition that does not translate raises its error when evaluated
(@racket[compile-library/partial]).}
@defproc[(directory-resolver [dir (or/c path-string? #f)]) procedure?]{A resolver: @tt{<Name>.cql} in
@racket[dir], else @racket[bundled-library]; cached. How the command line and a @hash-lang[] @tt{cql}
module find their includes.}
@defproc[(bundled-library [name (or/c symbol? string?)] [version (or/c string? #f)]) (or/c cql-library? #f)]{
A library shipped in the package's @tt{libraries/}: @tt{<name>-<version>.cql} when that version is
there, else @tt{<name>.cql} (FHIRHelpers 4.4.000 and 4.0.1).}
@defproc[(cql-value [text string?] [#:offset offset (or/c exact-integer? #f) #f]) any/c]{
A CQL expression's value with no data or library, such as a parameter's:
@racket[(cql-value "Interval[@2024-01-01T00:00:00Z, @2025-01-01T00:00:00Z)")]. Raises
@racket[exn:fail] with the parse or translation error.}
@defproc[(resources->data [resources (listof hash?)] [#:patient-context? patient-context? boolean? #t]) procedure?]{
Records as a retrieve source, by class: Bundles unwrapped, a cqm-models patient document as its
patient and data elements. With @racket[patient-context?], scoped to the one patient among them.}
@defproc[(valuesets->terminology [valuesets (listof (or/c hash? list?))]) procedure?]{ValueSets with
expansions (else enumerated compose concepts), or cqm-models value sets (@tt{{oid, concepts}}, also
as one array), as the membership function @racket[runtime] takes.}
@defproc[(caching-resolver [resolver procedure?]) procedure?]{The resolver, each (name, version)
resolved once. Use it to evaluate many patients against libraries that do not change: a resolver
that reads and parses a file or a data source otherwise does so for every include of every patient.}
@defproc[(measure-populations [measure hash?] [rl rlib?]) (hash/c string? exact-nonnegative-integer?)]{
One patient's proportion-measure populations (initial-population, denominator, exclusions, numerator,
exception), each 0 or 1.}
@defproc[(read-json-files [dir path-string?]) (listof hash?)]{Every @tt{.json} under a directory,
MeasureReports excluded; a file holding a JSON array gives its objects.}

@subsection[#:style 'unnumbered]{@tt{cql/cqm}}
@defmodule[cql/cqm]

Measures in the cqm-models format: a QDM patient document and a measure document (main library,
scoring, calculation method, population sets with their populations, observations and
stratifications).

@defproc[(cqm-patient-records [doc hash?]) (listof hash?)]{The patient (as a @tt{QDM::Patient}
record) and its data elements; a document wrapping its patient as @tt{qdmPatient} is unwrapped.}
@defproc[(cqm-measure-period [measure hash?]) (values string? string?)]{The measure period's bounds as
DateTime text: @racket["2012-01-01T00:00:00.000"] and @racket["2012-12-31T23:59:59.999"].}
@defproc[(cqm-measure-populations [measure hash?] [rl rlib?]) (listof hash?)]{
One patient's populations for each population set, then each of its strata (cqm-execution's
population index): population code (@racket['IPP], @racket['DENOM], @racket['DENEX],
@racket['NUMER], @racket['NUMEX], @racket['DENEXCEP], @racket['MSRPOPL], @racket['MSRPOPLEX],
@racket['STRAT]) to a count, and @racket['OBSERV] to the observations. Patient-based populations
count the patient; episode-based ones the distinct episodes. Proportion: DENOM within IPP, DENEX
within DENOM, NUMER within DENOM less DENEX, DENEXCEP within what remains; ratio: NUMER within IPP;
continuous variable: one observation per measure-population episode not excluded, in episode order;
ratio observations: one value per observation per IPP episode, null outside its population.}

@subsection[#:style 'unnumbered]{@tt{cql/workspace}}
@defmodule[cql/workspace]

@defthing[workspace-support any/c]{The @tt{fhir/lang/workspace} support fhir-mcp loads for CQL.}
@defthing[guide string?]{The repository's @tt{guide.md}: the language for agents (fhir-mcp's
@tt{learn://cql}).}

@subsection[#:style 'unnumbered]{@tt{cql/measure}}
@defmodule[cql/measure]

@defthing[$evaluate-measure procedure?]{
The R4 @tt{Measure/$evaluate-measure} as a @tt{fhir/operation} value, registered when the module
loads. It runs over @racket[current-data-source]: the Measure's Library and includes as Library
resources, value sets from the source then @racket[current-sd-packages], each patient's data by
reference. An individual MeasureReport with @tt{#:subject}; without, a summary over every Patient
with the proportion score.}
@defproc[(evaluate-measure [measure hash?] [period-start string?] [period-end string?]
                           [#:subject subject (or/c string? #f) #f]
                           [#:valuesets valuesets procedure? (lambda (url) #f)]
                           [#:offset offset (or/c exact-integer? #f) #f]) hash?]{The same as a procedure.}
@defproc[(measurement-period [start string?] [end string?] [offset (or/c exact-integer? #f)]) any/c]{The period as an Interval of DateTimes: whole days, end inclusive.}
@defproc[(period-offset [s string?]) (or/c exact-integer? #f)]{@racket["2021-01-01T00:00:00-07:00"] gives @racket[-420].}
@defparam[current-measure-offset minutes exact-integer?]{The evaluation timezone when the period start has no offset. Default 0.}

@subsection[#:style 'unnumbered]{@tt{cql/model/modelinfo}}
@defmodule[cql/model/modelinfo]

The data models a library's @tt{using} names. FHIR 4.0.1 (the ModelInfo the reference translator
ships) is installed; any other model is a ModelInfo document (in @racket[current-model-directories] or
@tt{CQL_MODEL_PATH}), registered with @racket[register-model!], or made on request by a provider in
@racket[current-model-providers] (from logical models: @tt{cql/model/structure-definitions}). How a
model's data looks is its representation (@tt{cql/representation}), declared as data beside it. The
engine names no model. A CType is a System type by name
(@racket["Integer"]), a model type (@racket["FHIR.Encounter"]), @racket["Any"], or a vector:
@racket[(vector 'list T)], @racket[(vector 'interval T)], @racket[(vector 'choice Ts)],
@racket[(vector 'tuple fields)].

@defstruct*[model ([name string?] [url string?] [version string?] [required list?] [patient-class any/c] [birth-date-path any/c]
                   [classes hash?] [conversions hash?] [contexts hash?] [representation-settings any/c] [target-url (or/c string? #f)])]{
A model, equal only to itself. @racket[target-url] is the model a mapping model reads (QI-Core and US
Core: FHIR's), else @racket[#f].}
@defstruct*[class-info ([name string?] [base any/c] [identifier any/c] [label any/c] [primary-code-path any/c] [retrievable? boolean?]
                        [elements hash?] [relationships list?] [targets hash?] [searches hash?] [target any/c])]{
@racket[targets]: element to its mapping in a mapping model; @racket[target]: the class it maps to.}
@defproc[(find-model [name string?] [version (or/c string? #f)]) (or/c model? #f)]{Registered (version @racket[#f]: the newest), then a provider, then the model directories.}
@defproc[(find-model-by-url [url string?]) (or/c model? #f)]
@defproc[(register-model! [m model?] [#:versions versions (listof string?) '()]) void?]
@defparam[current-model-providers providers (listof procedure?)]{Each a procedure of a name and a version answering a model or @racket[#f].}
@defproc[(installed-models) (listof model?)]{Every model the registry, the shipped index and the model directories hold (loading each).}
@defproc[(find-profile-class [identifier string?]) (or/c (cons/c model? string?) #f)]{The class of an installed mapping model whose identifier (a profile url) this is.}
@defproc[(model-element-target [m model?] [type string?] [element string?]) (or/c string? #f)]{The element's mapping, in a mapping model.}
@defparam[current-model-directories dirs (listof path-string?)]{ModelInfo @tt{.xml}, @tt{.xml.gz} or ModelForm @tt{.rktd} files, each with an optional @tt{<same name>.representation.rktd}.}
@defproc[(model-relationships [m model?] [type string?] [context string?]) (listof string?)]{The element paths by which an instance of @racket[type] relates to the context (its compartment).}
@deftogether[(
@defproc[(model-class [m model?] [name string?]) (or/c class-info? #f)]
@defproc[(model-element-type [m model?] [type string?] [element string?]) (or/c any/c #f)]
@defproc[(model-subtype? [m model?] [a string?] [b string?]) boolean?]
@defproc[(model-conversion [m model?] [type string?]) (or/c (cons/c any/c string?) #f)]
@defproc[(model-context-type [m model?] [context string?]) (or/c string? #f)]
@defproc[(model-resolve-type [m model?] [name string?]) (or/c string? #f)])]{
A class; an element's type, searching the base chain; the subtype relation; a type's implicit
conversion (target CType and FHIRHelpers function); a context's type; a bare name resolved
(@racket["Encounter"] to @racket["FHIR.Encounter"]).}
