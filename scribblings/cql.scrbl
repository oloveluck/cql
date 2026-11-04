#lang scribble/manual
@(require (for-label racket/base racket/contract
                     cql/parser/parse cql/compile cql/elm/json cql/interp cql/run cql/measure
                     cql/value cql/error cql/model/modelinfo))

@title{cql: Clinical Quality Language for Racket}

@hyperlink["https://cql.hl7.org/"]{CQL} compiled to ELM, and ELM run. The compiler covers the
expression and definition language with the System types, intervals, queries, and the typed
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
raco pkg install --link --auto fhir/fhir fhirpath cql      # from source, with the repos alongside
}

@section{@hash-lang[] @tt{cql}}
@defmodulelang[cql]

A @tt{.cql} file is one library. The module provides:

@itemlist[
@item{@racket[library], the CQL AST; @racket[elm], the ELM AST; @racket[elm-json], the canonical ELM
JSON text.}
@item{@racket[(evaluate resources #:parameters h #:valuesets vss)]: the library over a patient's FHIR
resources (a list of jsexprs, the Patient among them) as a hash from definition name to value.
@racket[h] maps parameter names to values; @racket[vss] is a list of ValueSet jsexprs with expansions.}
@item{@racket[(results)]: every definition evaluated with no data, for libraries without retrieves.}]

Nothing runs when the module is required. Running the file prints the ELM JSON. A parse error is a
compile error positioned in the file; a translation error names its @tt{define}.

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

@section{Values}
@defmodule[cql/value]

CQL is single-valued with @racket[cql-null] (three-valued logic), not FHIRPath's collections.
Integers are exact integers; Long, Decimal, Quantity and Date/DateTime/Time reuse fhirpath's
@racket[fp-long], @racket[fp-decimal], @racket[fp-quantity] and @racket[moment]; strings and booleans
are Racket's; a List is a list.

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
@defstruct*[cql-obj ([class string?] [fields hash?])]{An instance of another class (@tt{System.ValueSet{…}}), carrying its class for @tt{is}.}
@defstruct*[fhir-node ([type string?] [json any/c])]{FHIR data at a point in a resource: its model type (@racket["FHIR.Observation"]) and the JSON there.}
@defproc[(cql-value? [v any/c]) boolean?]
@defproc[(render [v any/c]) string?]{A stable rendering for diagnostics: @racket[(render (list 1 2))] is @racket["{1, 2}"].}
@defparam[current-evaluation-offset minutes (or/c exact-integer? #f)]{
The evaluation request's timezone offset. A DateTime with a time and no offset takes it;
@racket[#f] leaves such values offset-less (what the expression suite expects).}

@section{Parsing and compiling}
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
rest still compile, and a reference to it does not report again.}
@defproc[(compile-expr [ast any/c]) (or/c ok? err?)]{A bare expression to an ELM node.}
@defparam[current-library-resolver resolve (-> symbol? (or/c string? #f) (or/c cql-library? #f))]{
How @tt{include} finds a library's AST by name and version. @racket[#f] means unknown: references into
it compile untyped.}

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
The payload of an @racket[err]. @racket[diagnostic] is a @tt{fhir/diagnostic} with the position when
there is one, else @racket[#f]; the constructor takes it as an optional third argument.}
@defproc[(cql-error-string [e cql-error?]) string?]{@tt{file:line:col: cql parse error: …} or @tt{CQL translate error: …}.}

@section{Running ELM}
@defmodule[cql/interp]

@defproc[(run-library [lib elm-library?]) hash?]{Every definition evaluated with no data: name to value.}
@defproc[(eval-elm [node elm-expr?] [env ienv?]) cql-value?]{One ELM node in an environment.}
@defproc[(make-ienv [#:defs defs hash? (hash)] [#:operands operands hash? (hash)]) ienv?]{An environment: definitions (name to a promise of a value) and operands.}
@defproc[(runtime [data procedure?] [valueset procedure?] [offset (or/c exact-integer? #f)]) runtime?]{
What a library runs against: @racket[data] is @tt{type-name -> (listof resource)}, already scoped to
the context patient; @racket[valueset] is @tt{url -> (hash (cons system code) -> #t) | #f}, where
@racket[#f] means the value set is unavailable, an error when needed; @racket[offset] as
@racket[current-evaluation-offset].}
@defproc[(make-rlib [lib elm-library?] [includes hash?] [rt (or/c runtime? #f)] [parameters hash?]) rlib?]{
A library ready to run: its definitions memoised, its includes (alias to @racket[rlib]), and parameter
values that override the library's defaults.}
@defproc[(rlib-value [rl rlib?] [name string?]) cql-value?]{A definition's value.}
@deftogether[(@defproc[(runtime? [v any/c]) boolean?] @defproc[(rlib? [v any/c]) boolean?] @defproc[(ienv? [v any/c]) boolean?])]
@defproc[(test-equal? [a any/c] [b any/c]) boolean?]{Value equality as the conformance harness compares results.}

@subsection[#:style 'unnumbered]{@tt{cql/run}}
@defmodule[cql/run]

Libraries over FHIR data: includes, a data source, terminology, measure populations.

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
Compiles the library and, once each, the libraries it includes (found by @racket[resolver]), binding
the include aliases.}
@defproc[(resources->data [resources (listof hash?)] [#:patient-context? patient-context? boolean? #t]) procedure?]{
Resources (Bundles unwrapped) as a retrieve source, by type.}
@defproc[(valuesets->terminology [valuesets (listof hash?)]) procedure?]{
ValueSets with expansions (else enumerated compose concepts) as the membership function
@racket[runtime] takes.}
@defproc[(measure-populations [measure hash?] [rl rlib?]) (hash/c string? exact-nonnegative-integer?)]{
One patient's proportion-measure populations (initial-population, denominator, exclusions, numerator,
exception), each 0 or 1, from the Measure's population criteria.}
@defproc[(read-json-files [dir path-string?]) (listof hash?)]{Every @tt{.json} under a directory, MeasureReports excluded.}

@section{Measures as an operation}
@defmodule[cql/measure]

@defthing[$evaluate-measure procedure?]{
The R4 @tt{Measure/$evaluate-measure} as a @tt{fhir/operation} value, registered when the module
loads. It runs over @racket[current-data-source]: the Measure's Library and includes as Library
resources, value sets from the source then @racket[current-sd-packages], each patient's data by
reference. @racket[(apply-operation "$evaluate-measure" measure #:periodStart "2021-01-01" #:periodEnd "2021-12-31" #:subject "Patient/x")]
returns an individual MeasureReport; without a subject, a summary report over every Patient with the
proportion score.}
@defproc[(evaluate-measure [measure hash?] [period-start string?] [period-end string?]
                           [#:subject subject (or/c string? #f) #f]
                           [#:valuesets valuesets procedure? (lambda (url) #f)]
                           [#:offset offset (or/c exact-integer? #f) #f]) hash?]{The same as a procedure.}
@defproc[(measurement-period [start string?] [end string?] [offset (or/c exact-integer? #f)]) any/c]{The period as an Interval of DateTimes: whole days, end inclusive.}
@defproc[(period-offset [s string?]) (or/c exact-integer? #f)]{@racket["2021-01-01T00:00:00-07:00"] gives @racket[-420].}
@defparam[current-measure-offset minutes exact-integer?]{The evaluation timezone when the period start has no offset. Default 0.}

@section{The data model}
@defmodule[cql/model/modelinfo]

The FHIR 4.0.1 ModelInfo the reference translator ships, read from a compact form. A CType is a
System type by name (@racket["Integer"]), a model type (@racket["FHIR.Encounter"]), @racket["Any"],
or a vector: @racket[(vector 'list T)], @racket[(vector 'interval T)], @racket[(vector 'choice Ts)],
@racket[(vector 'tuple fields)].

@defstruct*[model ([name string?] [url string?] [version string?] [patient-class string?] [birth-date-path string?]
                   [classes hash?] [conversions hash?] [contexts hash?])]
@defstruct*[class-info ([name string?] [base any/c] [identifier any/c] [primary-code-path any/c] [retrievable? boolean?] [elements hash?])]
@defproc[(find-model [name string?] [version (or/c string? #f)]) (or/c model? #f)]{The models shipped: FHIR 4.0.1 (4.0.0 shares it).}
@deftogether[(
@defproc[(model-class [m model?] [name string?]) (or/c class-info? #f)]
@defproc[(model-element-type [m model?] [type string?] [element string?]) (or/c any/c #f)]
@defproc[(model-subtype? [m model?] [a string?] [b string?]) boolean?]
@defproc[(model-conversion [m model?] [type string?]) (or/c (cons/c any/c string?) #f)]
@defproc[(model-context-type [m model?] [context string?]) (or/c string? #f)]
@defproc[(model-resolve-type [m model?] [name string?]) (or/c string? #f)])]{
A class; an element's type, searching the base chain; the subtype relation; a type's implicit
conversion (the target CType and the FHIRHelpers function); a context's type; a bare name resolved
(@racket["Encounter"] to @racket["FHIR.Encounter"]).}

@section{Command line}

@verbatim{
raco cql <file.cql>                                   the ELM JSON
raco cql <file.cql> --data <dir|file.json> [--valuesets <dir>] [--libraries <dir>] [--define <name>]…
                                                      run it over FHIR data (patient context)
raco cql --tck [-v]                                   the cqframework/cql-tests suite (floor-ratcheted)
}

@tt{--data} takes a directory of resources or Bundles, or one file; includes are found as
@tt{<Name>.cql} in @tt{--libraries} (default: beside the file); @tt{--valuesets} supplies
expansions. Each definition, or the ones named with @tt{--define}, prints as @tt{name: value}. A
leading @hash-lang[] line is ignored. @tt{--format text|json|outcome} chooses how errors are
reported, all of them. Exit codes: 0, 1 on a CQL error or a suite regression, 2 on usage.
