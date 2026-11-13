#lang scribble/manual
@(require (for-label racket/base racket/contract
                     cql/parser/parse cql/compile cql/elm/json cql/interp cql/run cql/measure
                     cql/value cql/error cql/model/modelinfo cql/cqm cql/workspace))

@; tt : code inside a sentence, exactly as typed (scribble's own tt would turn -- into a dash and
@; 'x' into curly quotes)
@(require (only-in scribble/core element))
@(define (tt . text) (element 'tt text))

@title{cql: Clinical Quality Language}

@hyperlink["https://cql.hl7.org/"]{Clinical Quality Language} (CQL) is how health care writes down
clinical logic so that a computer can run it and a clinician can still read it: "women aged 50 to
74 with no mammogram in the last two years", "patients whose latest HbA1c is above 9 %". It is the
language quality measures and decision-support rules are published in.

This package runs CQL. You write a @italic{library}, a file of named definitions, and run it over a
patient's record. Each definition gets a value for that patient: true or false, a number, a date, a
list of the records that matched. A mistake in the library is reported at its line before anything
runs.

Part of the @hyperlink["https://oloveluck.github.io/fhir/"]{fhir} family of FHIR languages and tools.

@table-of-contents[]

@section{What you can do with it}

@itemlist[
@item{@bold{Ask a question of one patient's record}: @secref["first"].}
@item{@bold{Check a library as you write it}: every definition is checked against the shape of the
data, and a misspelt name is reported with the one you probably meant.}
@item{@bold{Compute a quality measure}: which patients count in the denominator and the numerator
(@secref["measures"]).}
@item{@bold{Use a library from a program}: as a function from a patient's data to answers
(@secref["module"]).}
@item{@bold{Work over data that is not FHIR}: QI-Core, US Core, QDM, or a model of your own
(@secref["models"]).}]

@section[#:tag "first"]{A first example}

@bold{1. Install} the tools as the @hyperlink["https://oloveluck.github.io/fhir/"]{family's front
page} describes. The command is then @tt{raco cql} (or @tt{cql}, if you built the standalone tools).

@bold{2. Have a patient.} A patient's record is a folder of FHIR resources as JSON files. Here it is
three files in @tt{patient/}: the patient, and two HbA1c results (a blood test used to follow
diabetes), each naming the patient it belongs to.

@verbatim|{
{"resourceType": "Patient", "id": "ann", "gender": "female",
 "birthDate": "1974-12-25"}

{"resourceType": "Observation", "id": "a1c-1", "status": "final",
 "code": {"coding": [{"system": "http://loinc.org", "code": "4548-4"}]},
 "subject": {"reference": "Patient/ann"}, "effectiveDateTime": "2025-03-01",
 "valueQuantity": {"value": 7.2, "unit": "%",
                   "system": "http://unitsofmeasure.org", "code": "%"}}
}|

The second result is the same with id @tt{a1c-2}, date @tt{2025-09-01} and value @tt{9.4}.

@bold{3. Write a library}, @tt{adults.cql}:

@verbatim|{library Adults version '1.0.0'
using FHIR version '4.0.1'
include FHIRHelpers version '4.0.1'

codesystem "LOINC": 'http://loinc.org'
code "HbA1c": '4548-4' from "LOINC"

context Patient

define "Female": Patient.gender = 'female'
define "Age": AgeInYears()
define "Adult Female": "Female" and "Age" >= 18

define "HbA1c Results": [Observation: "HbA1c"]
define "Tested": exists "HbA1c Results"
define "Highest HbA1c": Max("HbA1c Results" O return (O.value as Quantity).value)}|

Reading it from the top:

@itemlist[
@item{@tt{using FHIR} says the data is FHIR, so the library can talk about @tt{Patient} and
@tt{Observation}. @tt{include FHIRHelpers} brings in the standard helper library that lets you
compare FHIR values with plain ones (@tt{Patient.gender = 'female'}). It comes with this package.}
@item{@tt{codesystem} and @tt{code} give a name to the code for an HbA1c test, so the logic below can
say @tt{"HbA1c"} instead of a number.}
@item{@tt{context Patient} means every definition is about one patient at a time.}
@item{Each @tt{define} names an answer. A definition can use the ones above it:
@tt{"Adult Female"} is built from @tt{"Female"} and @tt{"Age"}.}
@item{@tt{[Observation: "HbA1c"]} is a @italic{retrieve}: the patient's Observations with that
code. The last line takes the number out of each and keeps the largest.}]

@bold{4. Run it} over the patient:

@verbatim{
$ raco cql adults.cql --data patient/ --now 2026-01-01 \
    --define "Adult Female" --define "Tested" --define "Highest HbA1c"
Adult Female: true
Tested: true
Highest HbA1c: 9.4
}

@litchar{--define} picks the answers to print; without it you get every definition, including the
two matching Observations in full. @litchar{--now} fixes today's date, which @tt{AgeInYears} needs:
leave it out and the age is as of today.

@bold{5. Make a mistake.} Write @tt{AgeInYear()} and the library is refused, at the place:

@verbatim{
$ raco cql adults.cql
adults.cql:11:15: cql compile error: in define "Age": unknown function 'AgeInYear'
  (did you mean `AgeInYears` or `AgeInYearsAt`?)
 11 | define "Age": AgeInYear()
    |               ^^^^^^^^^
}

Run without @litchar{--data}, a correct library prints its @italic{ELM}: the form CQL is translated
into before it runs, which is what other CQL engines exchange (@secref["words"]).

@section{Writing a library}

A file is one library: a header, then definitions.

@itemlist[
@item{@bold{The data model.} @tt{using FHIR version '4.0.1'} is what makes @tt{[Observation]} and
@tt{O.code} mean something. Other models are in @secref["models"].}
@item{@bold{Names.} A name with spaces is written in double quotes (@tt{"Adult Female"}); text is
written in single quotes (@tt{'female'}).}
@item{@bold{Retrieves.} @tt{[Observation]} is all of the patient's Observations;
@tt{[Observation: "HbA1c"]} only those with that code; @tt{[Observation: "Diabetes Labs"]} those
whose code is in a value set you named with @tt{valueset "Diabetes Labs": '<its url>'}.}
@item{@bold{Queries.} @tt{"HbA1c Results" O where O.status = 'final' return O.value} reads "for each
result O, keep the final ones, give its value". @tt{sort by}, @tt{First}, @tt{Last}, @tt{Count},
@tt{Max} and the rest work on the list.}
@item{@bold{Parameters.} @tt{parameter "Measurement Period" Interval<DateTime>} declares an input
that is given when the library runs.}
@item{@bold{Other libraries.} @tt{include Common version '1.0.0' called C} lets you write
@tt{C."Some Definition"}. An included library is looked for beside the file.}
@item{@bold{Nothing there.} A missing value is @tt{null}, and most operations on null give null:
comparing a result with no value to 9 is neither true nor false. @tt{Coalesce}, @tt{exists} and
@tt{is null} are how you say what should happen then.}]

Two things catch most newcomers:

@itemlist[
@item{@bold{Forgetting FHIRHelpers.} Without @tt{include FHIRHelpers}, @tt{Patient.gender = 'female'}
compares a FHIR value with a plain one. The library is refused with a message naming the include to
add.}
@item{@bold{Dates that can be written two ways.} An Observation's time is either one moment
(@tt{effectiveDateTime}) or a span (@tt{effectivePeriod}). @tt{O.effective during "Measurement
Period"} reads only the span, so a result with a single moment gives null. Say which you mean:
@tt{(O.effective as dateTime) during "Measurement Period"}. The translator warns about this, with
the cast to write.}]

The language itself is described at @hyperlink["https://cql.hl7.org/"]{cql.hl7.org}. The
repository's @tt{guide.md} has a cheat sheet and worked recipes (the latest value per patient, a
time window, "never tested"), each one run by the tests.

@section{Running it}

@subsection{Command line}

@tabular[#:style 'boxed #:sep @hspace[2]
(list (list @tt{raco cql lib.cql} "the ELM JSON")
      (list @tt{raco cql lib.cql --data patient/} "run it; every definition as name: value")
      (list @tt{raco cql lib.cql --data bundle.json --define Tested} "just the named definitions; repeat it")
      (list @tt{raco cql lib.cql --data p/ --now 2026-01-01} @elem{what @tt{Today()} and @tt{Now()} answer, so a result can be repeated})
      (list @tt{raco cql lib.cql --data p/ --valuesets vs/} "ValueSet resources (with expansions) for the library's value sets")
      (list @tt{raco cql measure.cql --data p/ --libraries libs/} @elem{includes found as @tt{libs/<Name>.cql}; else beside the file; else the bundled FHIRHelpers 4.0.1 and 4.4.000})
      (list @tt{raco cql lib.cql --data p/ --parameter "Measurement Period=Interval[…]"} "a parameter as a CQL expression; repeat it")
      (list @tt{raco cql lib.cql --data patient.json} "a QDM patient document (cqm-models) is one patient's record too")
      (list @tt{raco cql --tck [-v]} "the cqframework/cql-tests suite, floor-ratcheted"))]

@litchar{--data} is one patient's record: a folder of JSON files, one file, or a Bundle. Every
mistake in a library is reported, not just the first; @litchar{--format text|json|outcome} chooses
the form (text for a person, JSON or a FHIR OperationOutcome for a program). The command exits with
0 when all is well, 1 on a CQL error and 2 when it was called wrongly.

@subsection[#:tag "module"]{As a module, from Racket}

A CQL file whose first line is @tt{#lang cql} is a Racket module, so a program can require it and
call it. The library is checked when the program is built: a mistake in the CQL stops the build, at
its line.

@racketblock[
(require json (only-in "adults.cql" evaluate))
(define (read-file f) (call-with-input-file f read-json))
(define record (map read-file '("patient/patient.json" "patient/a1c-1.json" "patient/a1c-2.json")))
(define answers (evaluate record #:now "2026-01-01"))
(hash-ref answers "Adult Female")                           (code:comment "#t")
(hash-ref answers "Age")                                    (code:comment "51")
(evaluate record #:now "2026-01-01" #:define "Tested")      (code:comment "#t: one definition")
]

@racket[evaluate] takes one patient's records (the Patient among them) and answers every
definition by name. @racket[#:parameters] gives the library's parameters, @racket[#:valuesets] the
ValueSet resources it needs. The module also provides @racket[(results)] for a library that reads
no data, and the library's @racket[elm]. An included library is found beside the file, else among
the ones that come with the package (FHIRHelpers).

@subsection{Values}

The answers are Racket values. Integers, strings and booleans are Racket's; @tt{null} is @racket[cql-null]; lists are lists; a
Decimal is fhirpath's @racket[fp-decimal], a Quantity an @racket[fp-quantity], a date or time a
@racket[moment]; intervals, tuples, codes and concepts have their own structs
(@secref["cql-values"]). @racket[render] prints any of them the way the command line does.

@subsection{The pieces, from Racket}

The steps the module form does for you, one at a time: read the text, translate it to ELM, run it.


@racketblock[
(require cql/parser/parse cql/compile cql/elm/json cql/run fhir/result/result)
(define lib (ok-value (parse-cql text #:source "adults.cql")))
(elm->json-string (ok-value (compile-library lib)))                   (code:comment "the ELM")
(evaluate-library lib resources)                                       (code:comment "name -> value")
(code:comment "with includes found in a directory (then the bundled ones), parameters as CQL")
(define rl (load-runtime-library lib #:resolver (directory-resolver "cql/")
                                     #:data (resources->data resources)
                                     #:valuesets (valuesets->terminology value-sets)
                                     #:parameters (hash "Measurement Period"
                                                        (cql-value "Interval[@2024-01-01T00:00:00Z, @2025-01-01T00:00:00Z)"))))
(rlib-value rl "Tested")
]

@subsection[#:tag "measures"]{Measures}

A quality measure is a library plus a statement of which of its definitions are the measure's
populations: who is eligible (the denominator) and who met the goal (the numerator). FHIR writes
that statement as a Measure resource, and the operation @tt{$evaluate-measure} runs it and answers a
MeasureReport with the counts. It needs somewhere to find the patients, the libraries and the value
sets; here, a folder:

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

CQL does not know what a patient record looks like; a @italic{data model} tells it. The model
names the kinds of record (Observation, Encounter …), what each contains, and which element holds
its code. A library's @tt{using} line picks the model. These come installed:

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

You can add a model of your own without writing code. It is two pieces of data: its ModelInfo (the
standard description of a model, the same file other CQL tools read) and its
@deftech{representation}, which says how its records are written as JSON. A representation is a settings file
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

@section{In an editor, and for an AI agent}

@itemlist[
@item{@bold{In an editor.} The family's language server,
@hyperlink["https://oloveluck.github.io/fhir/fhir-lsp/index.html"]{fhir-lsp}, underlines a
library's mistakes as you type, lists its definitions, jumps from a name to where it is defined
(into an included library too) and completes names.}
@item{@bold{For an agent.} In an
@hyperlink["https://oloveluck.github.io/fhir/fhir-mcp/index.html"]{fhir-mcp} workspace a library
runs over every patient in the data at once, and @tt{explain} shows why one patient got the answer
they did: each definition's value, down to which records a retrieve kept. The repository's
@tt{guide.md} is the language written for an agent to read (fhir-mcp serves it as @tt{learn://cql}),
and @tt{.claude/skills/cql} is the same workflow for an agent using the command line.}]

@section{Limits}

@itemlist[
@item{@bold{FHIR version.} The FHIR model installed is 4.0.1 (R4). There is none for R5.}
@item{@bold{What a retrieve filters by.} The record's type and its code. A date range written inside
a retrieve is not applied (write it as a @tt{where}), and naming a profile in a retrieve does not
filter out records that do not conform to it.}
@item{@bold{One patient at a time.} The Patient context applies when the data holds exactly one
patient; records are tied to the patient through the links the model declares (in FHIR:
@tt{subject}, @tt{patient}, @tt{performer} …).}
@item{@bold{Value sets must be given.} Membership is decided from the ValueSet resources you supply:
their listed expansion, else the codes they enumerate. A value set defined by a rule ("all
descendants of …") is not worked out here, and a value set that is not supplied is an error rather
than an empty answer.}
@item{@bold{FHIR Measures.} Proportion measures, counted per patient, using the first library and
the first group's populations. Stratifiers, supplemental data and measure observations are not
computed, and @tt{$evaluate-measure} reads @tt{measure}, @tt{periodStart}, @tt{periodEnd} and
@tt{subject} only. (Measures in the cqm-models format get every kind of scoring:
@racket[cqm-measure-populations].)}
@item{@bold{A definition that does not translate} does not stop the rest of its library when the
library runs over data: that definition reports its error and the others are answered.}
@item{@bold{QDM versions.} A measure written for one version of QDM reads data of another as it is: a
QDM 5.3 measure's @tt{principalDiagnosis} is simply absent from QDM 5.5 data.}
@item{@bold{ELM} is written as JSON only, not XML.}]

@section[#:tag "words"]{Words used here}

@itemlist[
@item{@bold{Library}: one CQL file: a header and a list of definitions.}
@item{@bold{Definition} (a @tt{define}): a named expression; its value for a patient is an answer.}
@item{@bold{Retrieve}: the square-bracket expression that fetches a patient's records of one kind,
@tt{[Observation: "HbA1c"]}.}
@item{@bold{ELM} (Expression Logical Model): the standard machine-readable form of a library. CQL is
translated to ELM, and the ELM is what runs. Tools exchange it as JSON.}
@item{@bold{Data model}, @bold{ModelInfo}: the description of the records a library talks about.
ModelInfo is the standard file format for one.}
@item{@bold{FHIRHelpers}: the standard library that converts between FHIR's values and CQL's own
(a FHIR @tt{code} and a plain string, a FHIR Quantity and a CQL Quantity).}
@item{@bold{Value set}: a named set of codes; @bold{code system}: where codes come from (LOINC,
SNOMED CT).}
@item{@bold{Measure}, @bold{population}: a quality measure and its counted groups: initial
population, denominator, numerator, exclusions.}
@item{@bold{QI-Core}, @bold{US Core}: FHIR as profiled for United States quality measurement and
interoperability. @bold{QDM}: the Quality Data Model, an older, non-FHIR model for measures;
@bold{cqm-models} is the JSON format its tools (Bonnie, MADiE, cqm-execution) exchange.}]

@section{How it is tested}

Against published work, not only its own examples:

@itemlist[
@item{HL7's CQL test suite: 1820 of 1823 expressions give the expected value. The three that do not
are places where the suite disagrees with itself.}
@item{Published libraries, compared definition by definition with the ELM the reference translator
produces: 770 of 803 identical for 32 FHIR libraries, 1854 of 2144 for the 98 QI-Core libraries of
the 2024 quality measures.}
@item{Published measures over their own test patients: 71 of 76 for four FHIR measures, 188 of 190
for four QI-Core measures; for QDM, all 31 of cqm-execution's own checks.}]

The repository's @tt{TESTING.md} names every case that differs and why.

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
