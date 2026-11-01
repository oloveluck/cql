#lang brag

;; CQL grammar (https://cql.hl7.org/), expression + definition subset, translated to brag.
;;
;; Like the FHIRPath grammar this is a precedence cascade (brag has no precedence directives):
;; lowest-binding (implies) at the entry, highest (postfix/index) at the leaves. Punctuation is
;; hidden with `/`; operator-keyword wrappers are spliced with `@` so the operator surfaces inline
;; for lowering. Keyword tokens ("define", "and", "Interval", …) and named tokens (NUMBER, STRING,
;; IDENTIFIER, DATE, …, LONGNUMBER) come from lexer.rkt. Query/retrieve/timing productions are
;; deferred to a later slice.

;; ---- library structure ----------------------------------------------------------------------
cql-library    : library-header? definition*

library-header : /"library" qualified-id version-spec?
version-spec   : /"version" STRING
called-spec    : /"called" identifier

definition     : using-def | include-def | parameter-def | codesystem-def | valueset-def
               | context-def | statement-def

using-def      : /"using" qualified-id version-spec? called-spec?
include-def    : /"include" qualified-id version-spec? called-spec?
parameter-def  : /"parameter" identifier type-specifier? param-default?
param-default  : /"default" expr
codesystem-def : /"codesystem" identifier /":" STRING version-spec?
valueset-def   : /"valueset" identifier /":" STRING version-spec?
context-def    : /"context" identifier
statement-def  : expression-def | function-def
expression-def : /"define" access-modifier? identifier /":" expr
function-def   : /"define" access-modifier? /"function" identifier /"(" operand-list? /")" returns-spec? /":" func-body
access-modifier : "public" | "private"
returns-spec   : /"returns" type-specifier
operand-list   : operand (/"," operand)*
operand        : identifier type-specifier
func-body      : expr | "external"

;; ---- expression precedence cascade (low -> high binding) ------------------------------------
expr                : query | implies-expr

;; ---- queries (over lists / singletons; no FHIR retrieve in this slice) ----------------------
query               : source-clause query-tail
                    | /"from" source-clause (/"," source-clause)* query-tail
source-clause       : /"(" expr /")" alias | member alias
alias               : identifier
query-tail          : let-clause* where-clause? return-or-agg? sort-clause?
let-clause          : /"let" let-item (/"," let-item)*
let-item            : identifier /":" expr
where-clause        : /"where" expr
return-or-agg       : return-clause | aggregate-clause
return-clause       : /"return" return-mod? expr
return-mod          : "all" | "distinct"
aggregate-clause    : /"aggregate" return-mod? identifier starting-clause? /":" expr
starting-clause     : /"starting" expr
sort-clause         : /"sort" sort-dir
sort-dir            : "asc" | "ascending" | "desc" | "descending"
implies-expr        : or-expr           (@implies-op or-expr)*
or-expr             : and-expr          (@or-op and-expr)*
and-expr            : membership-expr   (@and-op membership-expr)*
membership-expr     : equality-expr     (@membership-op equality-expr)*
equality-expr       : comparison-expr   (@equality-op comparison-expr)*
comparison-expr     : setop-expr        comp-tail*
comp-tail           : @comparison-op setop-expr | timing-rel | between-tail
;; X between A and B  ==  X >= A and X <= B (operands additive so `and` stays the delimiter)
between-tail        : /"between" additive-expr /"and" additive-expr
timing-rel          : timing-op setop-expr
timing-op           : "before" prec-of?
                    | "after" prec-of?
                    | "same" prec? /"as"
                    | "same" prec? /"or" "before"
                    | "same" prec? /"or" "after"
                    | "on" /"or" "before" prec-of?
                    | "on" /"or" "after" prec-of?
                    | "meets" prec-of?
                    | "meets" "before" prec-of?
                    | "meets" "after" prec-of?
                    | "overlaps" prec-of?
                    | "overlaps" "before" prec-of?
                    | "overlaps" "after" prec-of?
                    | "starts" prec-of?
                    | "ends" prec-of?
                    | "includes" prec-of?
                    | "properly" "includes" prec-of?
                    | "included" /"in" prec-of?
                    | "properly" "included" /"in" prec-of?
                    | "during" prec-of?
                    | "properly" "during" prec-of?
prec-of             : prec /"of"
prec                : identifier
setop-expr          : additive-expr     (@setop-op additive-expr)*
additive-expr       : multiplicative-expr (@additive-op multiplicative-expr)*
multiplicative-expr : power-expr        (@multiplicative-op power-expr)*
power-expr          : type-expr         (@power-op type-expr)*
type-expr           : unary-expr
                    | cast-expr
                    | convert-expr
                    | unary-expr type-op-tail+
type-op-tail        : @type-op type-specifier
                    | "is" "null"
                    | "is" "not" "null"
cast-expr           : /"cast" unary-expr /"as" type-specifier
convert-expr        : /"convert" unary-expr /"to" type-specifier
unary-expr          : @unary-op unary-expr | interval-prefix | postfix-expr
;; interval/list prefix operators
interval-prefix     : "start" /"of" unary-expr
                    | "end" /"of" unary-expr
                    | "width" /"of" unary-expr
                    | "point" /"from" unary-expr
                    | "singleton" /"from" unary-expr
                    | "collapse" unary-expr
                    | "distinct" unary-expr
                    | "flatten" unary-expr
                    | "predecessor" /"of" unary-expr
                    | "successor" /"of" unary-expr
                    | "expand" unary-expr (/"per" unary-expr)?

postfix-expr        : primary postfix-tail*
postfix-tail        : invocation | indexer
invocation          : /"." (function-call | member)
indexer             : /"[" expr /"]"

primary             : literal
                    | if-expr
                    | case-expr
                    | interval-selector
                    | typed-list-selector
                    | typed-tuple-selector
                    | braced
                    | minmax
                    | instance-selector
                    | duration-of
                    | difference-between
                    | component-from
                    | function-call
                    | member
                    | /"(" expr /")"

;; duration/difference between two temporals (operands are additive-expr so the `and` delimiter isn't
;; consumed as logical `and`, and the whole thing binds tighter than comparison — `months between A and B > 5`)
minmax              : "minimum" type-specifier | "maximum" type-specifier

;; instance selectors: Code { code: '…' } / Concept {…} / Quantity {…} / System.ValueSet {…}
instance-selector   : "Code" /"{" tuple-body? /"}"
                    | "Concept" /"{" tuple-body? /"}"
                    | qualified-id /"{" tuple-body? /"}"

;; duration in <units> of X == <units> between start of X and end of X
duration-of         : /"duration" /"in" identifier /"of" unary-expr
;; NOTE: `<units> between A and B` (duration) is NOT a separate production — it parses as
;; `<identifier> between-tail` at the comparison level and the AST lowering recognises the unit word
;; (mirroring the reference grammar's context-sensitive durationBetween).
difference-between   : /"difference" /"in" identifier /"between" additive-expr /"and" additive-expr

;; <precision> from <expr> — component extraction (year/month/…/timezone/date/time from X).
;; The precision word stays a plain identifier (so calendar units like `year` still work in quantities);
;; the compiler validates it. `from` is the disambiguating keyword.
component-from      : identifier /"from" unary-expr

if-expr             : /"if" expr /"then" expr /"else" expr
case-expr           : /"case" expr? case-item+ case-else? /"end"
case-item           : /"when" expr /"then" expr
case-else           : /"else" expr

interval-selector   : /"Interval" interval-open expr /"," expr interval-close
interval-open       : "[" | "("
interval-close      : "]" | ")"

typed-list-selector : /"List" type-arg? /"{" list-body? /"}"
typed-tuple-selector : /"Tuple" /"{" tuple-body? /"}"
braced              : /"{" braced-body? /"}"
braced-body         : tuple-body | list-body
list-body           : expr (/"," expr)*
tuple-body          : tuple-element (/"," tuple-element)*
tuple-element       : identifier /":" expr
type-arg            : /"<" type-specifier /">"

function-call       : identifier /"(" arg-list? /")"
arg-list            : expr (/"," expr)*
member              : identifier

;; ---- type specifiers ------------------------------------------------------------------------
type-specifier      : interval-type | list-type | named-type
named-type          : qualified-id
interval-type       : /"Interval" /"<" type-specifier /">"
list-type           : /"List" /"<" type-specifier /">"

;; ---- leaves ---------------------------------------------------------------------------------
qualified-id        : identifier (/"." identifier)*
;; `code`/`concept`/… double as identifiers in many positions
identifier          : IDENTIFIER | "as" | "is" | "in" | "contains" | "code" | "concept"

literal             : ratio | quantity | LONGNUMBER | STRING | BOOL | DATE | DATETIME | TIME | "null"
;; a ratio literal is quantity : quantity (a NUMBER-led ":" is unambiguous — tuple keys are identifiers)
ratio               : quantity /":" quantity
quantity            : NUMBER unit?
unit                : identifier | STRING

;; ---- operator wrappers ----------------------------------------------------------------------
implies-op          : "implies"
and-op              : "and"
power-op            : "^"
or-op               : "or" | "xor"
membership-op       : "in" | "contains"
equality-op         : "=" | "!=" | "~" | "!~"
comparison-op       : "<=" | ">=" | "<" | ">"
setop-op            : "union" | "intersect" | "except"
additive-op         : "+" | "-" | "&"
multiplicative-op   : "*" | "/" | "div" | "mod"
type-op             : "is" | "as"
unary-op            : "+" | "-" | "not" | "exists"
