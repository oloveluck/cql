#lang scribble/manual
@(require (for-label racket/base racket/contract
                     cql/parser/parse cql/compile cql/elm/json cql/interp cql/run cql/measure
                     cql/value cql/error cql/model/modelinfo))

@title{cql: Clinical Quality Language for Racket}

@hyperlink["https://cql.hl7.org/"]{CQL} compiled to ELM, and ELM run. The compiler covers the
expression and definition language with the System types, intervals and queries, and the typed
translation of libraries over FHIR R4 (retrieves, includes, terminology definitions, the implicit
FHIRHelpers conversions) with the reference translator's own ModelInfo. The interpreter runs the ELM
over FHIR data: retrieves, value set membership, measure populations.

Measured against the published suites: cqframework/cql-tests 1820/1823; 32 published libraries
compile with 742 of 803 definitions ELM-identical to the reference translator; four eCQMs agree with
their expected populations on 71 of 76 test patients.

Part of the @hyperlink["https://github.com/oloveluck/fhir"]{fhir} family; depends on @tt{fhir} and
@tt{fhirpath} (the System value carriers).

@table-of-contents[]

@section{Install}

@verbatim{
raco pkg install --link --auto fhir/fhir fhirpath cql
    # from source, with the repos alongside
}

@section{Writing a library}

A file is one library: a header, then definitions. @tt{using FHIR version '4.0.1'} loads the FHIR
model, so @tt{[Observation]} retrieves resources and @tt{O.code.coding} navigates them;
@tt{context Patient} scopes retrieves to one patient; @tt{parameter} declares an input;
@tt{include} brings in another library under an alias. Names with spaces are quoted with
@tt{"…"}, strings with @tt{'…'}.

@verbatim|{#lang cql
library BloodPressureScreening version '1.0.0'
using FHIR version '4.0.1'
parameter "Panel Codes" List<System.String>
context Patient

define "Readings":
  [Observation] O where exists (O.code.coding C where C.code.value in "Panel Codes")

define "Systolic Values":
  "Readings" O return singleton from (O.component C
                                        where exists (C.code.coding K where K.code.value = '8480-6')
                                        return (C.value as FHIR.Quantity).value.value)

define "Highest Systolic": Max("Systolic Values")
define "Hypertensive": "Highest Systolic" >= 140}|

Without FHIRHelpers, FHIR primitives are navigated to their @tt{.value}; a library that includes
FHIRHelpers gets the implicit conversions the reference translator makes. Terminology definitions
(@tt{valueset "BP Panels": 'http://…'}) make @tt{[Observation: "BP Panels"]} a retrieve filtered by
membership. The language reference is @hyperlink["https://cql.hl7.org/"]{cql.hl7.org}.

@section{Using it}

@bold{As a module.} @racket[(require "screening.cql")] compiles the library when the program
compiles; a parse or translation error is a compile error, positioned in the file (a translation
error names its @tt{define}). The module provides @racket[evaluate]: the library over one patient's
resources (the Patient among them), every definition's value by name.

@racketblock[
(require (only-in "screening.cql" [evaluate screen]))
(screen (cons patient observations) #:parameters (hash "Panel Codes" '("85354-9" "55284-4")))
(code:comment "=> (hash \"Readings\" ... \"Highest Systolic\" 152 \"Hypertensive\" #t ...)")
]

Resources are FHIR JSON. @tt{#:valuesets} takes ValueSet resources with expansions, for a
library's terminology definitions. @racket[(results)] evaluates a library that has no retrieves with
no data. @tt{racket screening.cql} prints the ELM JSON. A library with @tt{include}s runs through
@racket[load-runtime-library] with a resolver, or the command line, which finds them by file name.

@bold{Values.} Integers, strings and booleans are Racket's; @tt{null} is @racket[cql-null]; lists are
lists; a Decimal is fhirpath's @racket[fp-decimal], a Quantity an @racket[fp-quantity], a date or
time a @racket[moment]; intervals, tuples, codes and concepts have their structs
(@secref["cql-values"]). @racket[render] prints any of them.

@bold{From Racket.}

@racketblock[
(require cql/parser/parse cql/compile cql/elm/json cql/run fhir/result/result)
(define lib (ok-value (parse-cql text #:source "screening.cql")))
(elm->json-string (ok-value (compile-library lib)))                   (code:comment "the ELM")
(evaluate-library lib resources #:parameters (hash "Panel Codes" codes))
(code:comment "includes, by name and version")
(define rl (load-runtime-library lib #:resolver find-library
                                     #:data (resources->data resources)
                                     #:valuesets (valuesets->terminology value-sets)))
(rlib-value rl "Hypertensive")
]

@bold{Measures.} @tt{Measure/$evaluate-measure} runs a Measure's library over a data source:
@racket[(apply-operation "$evaluate-measure" measure #:periodStart "2021-01-01" #:periodEnd "2021-12-31" #:subject "Patient/x")]
with @racket[current-data-source] holding the patients, the Library resources and the value sets.
Over a server's store, fhir-server serves it (@tt{raco fhir-server evaluate-measure}).

@bold{Command line.}

@verbatim{
raco cql screening.cql                                             the ELM JSON
raco cql screening.cql --data patient/ --valuesets vs/ --define Hypertensive
                                                                   run it over FHIR data (patient context)
raco cql measure.cql --data bundle.json --libraries libs/
    includes found as libs/<Name>.cql
raco cql --tck [-v]
    the cqframework/cql-tests suite
}

@litchar{--data} is a directory of resources or Bundles, or one file. Each definition, or the ones named
with @litchar{--define}, prints as @tt{name: value}. @litchar{--format text|json|outcome} chooses how errors
are reported, all of them. Exit codes: 0, 1 on a CQL error or a suite regression, 2 on usage.

@section{Limits}

@itemlist[
@item{@bold{Data model.} FHIR 4.0.1 (4.0.0 and an unversioned @tt{using FHIR} share it). No R5
ModelInfo, no QDM. A retrieve filters by type and code; @tt{dateProperty} and @tt{dateRange} are not
applied, and @tt{templateId} is carried but not used to filter. The Patient context applies when the
data holds exactly one Patient, matched by @tt{subject}, @tt{patient} and @tt{beneficiary}
references.}
@item{@bold{Terminology.} Membership comes from the ValueSet resources supplied: their
@tt{expansion}, else enumerated concepts. Filters, nested value sets and excludes are not evaluated,
and the terminology service is not called; a value set with neither is empty, and one not supplied
at all is an error. @tt{in CodeSystem} looks the code system up in the same table.}
@item{@bold{Measures.} Proportion scoring, patient-based, from the first library and the first
group's population codes; @tt{Measure.scoring} is not read, so ratio, continuous-variable and cohort
measures are scored as proportions. No stratifiers, supplemental data or measure observations; an
episode-of-care population counts as in when its list is non-empty. @tt{$evaluate-measure} reads
@tt{measure}, @tt{periodStart}, @tt{periodEnd} and @tt{subject} only.}
@item{@bold{The module.} @racket[evaluate] passes no library resolver, so a library with an
@tt{include} must run through @racket[load-runtime-library] or the command line. A translation error
does not fail the module: @racket[elm] is @racket[#f] and only running the file reports it.}
@item{@bold{ELM} is emitted as JSON only. Of 803 definitions in the published corpus 61 differ from
the reference translator, in named categories (translator-version noise around @tt{InValueSet},
choice operands in timing phrases, @tt{Interval[…].low} re-typing, ratio details in FHIRHelpers, one
measure's day-of-procedure phrases). Three expression-suite tests remain, each a corpus
contradiction or a grammar for one test.}]

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
Every translation error, each positioned at its definition; a failing definition is skipped and the
rest still compile.}
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
@defstruct*[fhir-node ([type string?] [json any/c])]{FHIR data at a point in a resource: its model type (@racket["FHIR.Observation"]) and the JSON there.}
@defproc[(cql-value? [v any/c]) boolean?]
@defproc[(render [v any/c]) string?]{@racket[(render (list 1 2))] is @racket["{1, 2}"].}
@defparam[current-evaluation-offset minutes (or/c exact-integer? #f)]{
The evaluation request's timezone offset. A DateTime with a time and no offset takes it;
@racket[#f] leaves such values offset-less (what the expression suite expects).}

@subsection[#:style 'unnumbered]{@tt{cql/interp}}
@defmodule[cql/interp]

@defproc[(run-library [lib elm-library?]) hash?]{Every definition evaluated with no data: name to value.}
@defproc[(eval-elm [node elm-expr?] [env ienv?]) cql-value?]{One ELM node in an environment.}
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
@deftogether[(@defproc[(runtime? [v any/c]) boolean?] @defproc[(rlib? [v any/c]) boolean?] @defproc[(ienv? [v any/c]) boolean?])]
@defproc[(test-equal? [a any/c] [b any/c]) boolean?]{Value equality as the conformance harness compares results.}

@subsection[#:style 'unnumbered]{@tt{cql/run}}
@defmodule[cql/run]

@defproc[(evaluate-library [lib cql-library?] [resources (listof hash?)]
                           [#:parameters parameters hash? (hash)]
                           [#:valuesets valuesets (listof hash?) '()]
                           [#:offset offset (or/c exact-integer? #f) #f]) hash?]{
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
apply to its includes.}
@defproc[(resources->data [resources (listof hash?)] [#:patient-context? patient-context? boolean? #t]) procedure?]{Resources (Bundles unwrapped) as a retrieve source, by type.}
@defproc[(valuesets->terminology [valuesets (listof hash?)]) procedure?]{ValueSets with expansions (else enumerated compose concepts) as the membership function @racket[runtime] takes.}
@defproc[(measure-populations [measure hash?] [rl rlib?]) (hash/c string? exact-nonnegative-integer?)]{
One patient's proportion-measure populations (initial-population, denominator, exclusions, numerator,
exception), each 0 or 1.}
@defproc[(read-json-files [dir path-string?]) (listof hash?)]{Every @tt{.json} under a directory, MeasureReports excluded.}

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

The FHIR 4.0.1 ModelInfo the reference translator ships. A CType is a System type by name
(@racket["Integer"]), a model type (@racket["FHIR.Encounter"]), @racket["Any"], or a vector:
@racket[(vector 'list T)], @racket[(vector 'interval T)], @racket[(vector 'choice Ts)],
@racket[(vector 'tuple fields)].

@defstruct*[model ([name string?] [url string?] [version string?] [patient-class string?] [birth-date-path string?]
                   [classes hash?] [conversions hash?] [contexts hash?])]
@defstruct*[class-info ([name string?] [base any/c] [identifier any/c] [primary-code-path any/c] [retrievable? boolean?] [elements hash?])]
@defproc[(find-model [name string?] [version (or/c string? #f)]) (or/c model? #f)]
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
