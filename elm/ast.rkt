#lang racket/base

;;; ast.rkt — the ELM (Expression Logical Model) node structs, the compile target of CQL.
;;;
;;; ELM's canonical form is JSON; each node is a JSON object with a `type` discriminator. We model it
;;; as prefab structs (mirroring fhirpath/parser/ast.rkt) so ELM values are immutable literal data the
;;; #lang can embed, the compiler builds, the JSON emitter (json.rkt) serializes, and the interpreter
;;; (../interp.rkt) walks. Binary/n-ary/unary operators all share the ELM OperatorExpression shape —
;;; they differ only by the `type` string — so we use generic `elm-nary`/`elm-unary` carrying that
;;; string rather than ~40 near-identical structs. resultTypeName is carried where the compiler infers
;;; it (an optional System type name string).

(require racket/contract)

(provide (struct-out elm-library) (struct-out elm-using)
         (struct-out elm-include) (struct-out elm-parameter-def) (struct-out elm-codesystem-def)
         (struct-out elm-valueset-def) (struct-out elm-code-def) (struct-out elm-concept-def)
         (struct-out elm-context-def) (struct-out elm-retrieve) (struct-out elm-relationship)
         (struct-out elm-sort-by) (struct-out elm-alias-ref) (struct-out elm-query-let-ref)
         (struct-out elm-identifier-ref) (struct-out elm-term-ref) (struct-out elm-code)
         (struct-out elm-concept) (struct-out elm-in-valueset) (struct-out elm-precision-op)
         (struct-out elm-choice-type) (struct-out elm-tuple-type) (struct-out elm-minmax) (struct-out elm-def) (struct-out elm-function-def)
         (struct-out elm-operand-def)
         (struct-out elm-literal) (struct-out elm-null) (struct-out elm-quantity) (struct-out elm-ratio)
         (struct-out elm-date) (struct-out elm-time) (struct-out elm-datetime) (struct-out elm-component)
         (struct-out elm-between) (struct-out elm-timing) (struct-out elm-query) (struct-out elm-instance)
         (struct-out elm-nary) (struct-out elm-unary)
         (struct-out elm-interval) (struct-out elm-list) (struct-out elm-tuple) (struct-out elm-tuple-elem)
         (struct-out elm-if) (struct-out elm-case) (struct-out elm-case-item)
         (struct-out elm-property) (struct-out elm-expression-ref) (struct-out elm-function-ref)
         (struct-out elm-operand-ref) (struct-out elm-parameter-ref)
         (struct-out elm-is) (struct-out elm-as) (struct-out elm-to)
         (struct-out elm-named-type) (struct-out elm-interval-type) (struct-out elm-list-type)
         (contract-out
          [system-type-name (-> string? string?)]      ; "Integer" -> "{urn:hl7-org:elm-types:r1}Integer"
          [elm-expr?        (-> any/c boolean?)]))

;; the System model namespace all built-in ELM types live in
(define system-ns "urn:hl7-org:elm-types:r1")
(define (system-type-name simple) (format "{~a}~a" system-ns simple))

;; ---- library / statements --------------------------------------------------
;; An ELM library: its identifier, then one list per ELM library section (each possibly empty).
(struct elm-library      (id version usings includes parameters codesystems valuesets codes concepts
                             contexts statements) #:prefab)
(struct elm-using        (local-id uri version)         #:prefab)
(struct elm-include      (local-id path version)        #:prefab)
(struct elm-parameter-def (name access default type)    #:prefab) ; default: elm|#f ; type: type-spec|#f
(struct elm-codesystem-def (name id version access)     #:prefab)
(struct elm-valueset-def (name id version access codesystems) #:prefab) ; codesystems: (listof elm-term-ref)
(struct elm-code-def     (name id display access codesystem) #:prefab) ; codesystem: elm-term-ref
(struct elm-concept-def  (name display access codes)    #:prefab) ; codes: (listof elm-term-ref)
(struct elm-context-def  (name)                         #:prefab)
(struct elm-def          (name context access expression) #:prefab) ; an ExpressionDef
(struct elm-function-def (name context access operands result-type expression fluent?) #:prefab)
(struct elm-operand-def  (name type)                    #:prefab)

;; ---- expressions -----------------------------------------------------------
;; every expression node optionally carries an inferred result-type name (a System simple name) —
;; kept on the nodes where the compiler computes it; #f when unknown.
(struct elm-literal      (value-type value)  #:prefab) ; value-type: System simple name ; value: string
(struct elm-null         (value-type)        #:prefab) ; typed Null (value-type may be #f)
(struct elm-quantity     (value unit)        #:prefab) ; value: string (decimal lexeme) ; unit: string|#f
(struct elm-ratio        (numerator denominator) #:prefab)
;; canonical ELM temporal constructors: each operand is an ELM expr or #f (absent -> lower precision)
(struct elm-date         (year month day)   #:prefab)
(struct elm-time         (hour minute second millisecond) #:prefab)
(struct elm-datetime     (year month day hour minute second millisecond timezone-offset) #:prefab)
;; component extraction: (<precision> from operand) — precision a symbol e.g. 'year 'month 'timezoneoffset
(struct elm-component    (precision operand) #:prefab)
;; DurationBetween / DifferenceBetween: kind 'duration|'difference ; precision a symbol
(struct elm-between      (kind precision a b) #:prefab)
;; timing comparison: op 'before|'after|'same-as|'same-or-before|'same-or-after ; precision symbol|#f
(struct elm-timing       (op precision a b) #:prefab)
;; a query over lists/singletons. sources/lets: (Listof (cons string elm)) ; where/ret: elm|#f ;
;; sort: 'asc|'desc|#f ; agg: (list name distinct? starting-elm|#f body-elm)|#f
;; relationships: (listof elm-relationship) ; sort: #f | (listof elm-sort-by)
(struct elm-query        (sources lets relationships where ret ret-distinct sort agg) #:prefab)
(struct elm-relationship (kind alias expression such-that) #:prefab) ; kind 'with|'without
;; kind 'direction|'column|'expression ; direction "asc"|"desc" ; target: #f | path string | elm
(struct elm-sort-by      (kind direction target) #:prefab)
;; [Type: codeProperty codeComparator codes] — data-type a qualified type name {uri}Name
(struct elm-retrieve     (data-type template-id code-property code-comparator codes) #:prefab)
;; an Instance selector (Code{…}/Concept{…}/Quantity{…}/ClassType{…}): class-type string + named elements
(struct elm-instance     (class-type elements) #:prefab) ; elements: (Listof elm-tuple-elem)
(struct elm-nary         (op operands result-type) #:prefab) ; op: ELM type string e.g. "Add" ; operands: (listof elm)
(struct elm-unary        (op operand result-type)  #:prefab) ; op: "Negate"/"Not"/"Exists"/…
(struct elm-interval     (low high low-closed high-closed) #:prefab)
(struct elm-list         (elements)          #:prefab)
(struct elm-tuple        (elements)          #:prefab) ; (listof elm-tuple-elem)
(struct elm-tuple-elem   (name value)        #:prefab)
(struct elm-if           (cond then els)     #:prefab)
(struct elm-case         (comparand items els) #:prefab) ; comparand #f for a standard case
(struct elm-case-item    (when then)         #:prefab)
(struct elm-property     (source path scope) #:prefab) ; source elm|#f ; scope: alias name|#f
(struct elm-alias-ref    (name)              #:prefab) ; a query alias used as a value
(struct elm-query-let-ref (name)             #:prefab)
(struct elm-identifier-ref (name)            #:prefab) ; a name resolved against the sort row
;; kind "ValueSetRef"|"CodeSystemRef"|"CodeRef"|"ConceptRef" ; library #f when local
(struct elm-term-ref     (kind name library) #:prefab)
(struct elm-code         (code system display) #:prefab) ; Code 'c' from "CS" ; system elm-term-ref
(struct elm-concept      (codes display)     #:prefab)
(struct elm-in-valueset  (kind code valueset as-expression?) #:prefab) ; kind "InValueSet"|"AnyInValueSet" ;
;;   as-expression?: the value set is written as `valuesetExpression` as well (`"VS" contains x`), not as `valueset` alone (`x in "VS"`)
(struct elm-precision-op (op precision operands) #:prefab) ; CalculateAgeAt etc. ; precision "Year"…
;; minimum/maximum T: which 'min|'max ; value-type the qualified type name ; boundary the value as ELM
;; (the interpreter evaluates the boundary; JSON writes MinValue/MaxValue)
(struct elm-minmax       (which value-type boundary) #:prefab)
(struct elm-expression-ref (name library)    #:prefab) ; library #f when local
;; signature: the operand type-specs of the overload the translator chose (#f when unknown) — the
;; interpreter picks that overload among same-named functions
(struct elm-function-ref (name library operands signature) #:prefab)
(struct elm-operand-ref  (name)              #:prefab)
(struct elm-parameter-ref (name library)     #:prefab)
(struct elm-is           (operand type)      #:prefab) ; type: an ELM type-specifier struct
(struct elm-as           (operand type strict) #:prefab)
(struct elm-to           (operand to-type)   #:prefab) ; a Convert/To* — to-type: System simple name

;; ---- type specifiers -------------------------------------------------------
(struct elm-named-type    (name)    #:prefab) ; name: full qualified type name string
(struct elm-interval-type (point)   #:prefab)
(struct elm-list-type     (element) #:prefab)
(struct elm-choice-type   (types)   #:prefab)
(struct elm-tuple-type    (elements) #:prefab) ; (listof (cons name-string type-spec))

(define (elm-expr? x)
  (or (elm-literal? x) (elm-null? x) (elm-quantity? x) (elm-ratio? x) (elm-nary? x) (elm-unary? x)
      (elm-date? x) (elm-time? x) (elm-datetime? x) (elm-component? x) (elm-between? x) (elm-timing? x)
      (elm-query? x) (elm-instance? x)
      (elm-interval? x) (elm-list? x) (elm-tuple? x) (elm-if? x) (elm-case? x) (elm-property? x)
      (elm-expression-ref? x) (elm-function-ref? x) (elm-operand-ref? x) (elm-parameter-ref? x)
      (elm-is? x) (elm-as? x) (elm-to? x) (elm-retrieve? x) (elm-alias-ref? x) (elm-query-let-ref? x)
      (elm-identifier-ref? x) (elm-term-ref? x) (elm-code? x) (elm-concept? x) (elm-in-valueset? x)
      (elm-precision-op? x) (elm-minmax? x)))

(module+ test
  (require rackunit)
  (check-equal? (system-type-name "Integer") "{urn:hl7-org:elm-types:r1}Integer")
  (check-true (elm-expr? (elm-literal "Integer" "5")))
  (check-true (elm-expr? (elm-nary "Add" (list (elm-literal "Integer" "1")) "Integer"))))
