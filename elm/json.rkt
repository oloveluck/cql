#lang racket/base

;;; json.rkt — ELM struct -> canonical ELM JSON.
;;;
;;; ELM is plain JSON (NOT a FHIR resource), so we emit jsexpr directly and stringify with
;;; racket/json — routing through serialize/to-json.rkt would inject FHIR resourceType/_field
;;; behavior. `drop-false` (shared util) omits absent optional attributes. Each expression object
;;; carries its ELM `type` discriminator; a full library wraps in {"library": {…}}.

(require racket/contract racket/match
         json
         (only-in fhir/serialize/jsexpr-util drop-false)
         "ast.rkt")

(provide (contract-out
          [elm->jsexpr      (-> any/c jsexpr?)]        ; a library or a bare expression node
          [elm->json-string (-> any/c string?)]
          [library->jsexpr  (-> elm-library? jsexpr?)]))

(define (elm->json-string x) (jsexpr->string (elm->jsexpr x)))

;; opt-expr : (or ELM #f) -> (or jsexpr #f)   emit an operand only when present
(define (opt-expr e) (and e (expr->jsexpr e)))

;; component extraction maps to distinct ELM node types; the datetime components share
;; DateTimeComponentFrom with a `precision` attribute.
(define (component-elm-type prec)
  (case prec
    [(timezoneoffset timezone) "TimezoneOffsetFrom"]
    [(date) "DateFrom"] [(time) "TimeFrom"]
    [else "DateTimeComponentFrom"]))
(define timing-elm-types
  (hasheq 'before "Before" 'after "After" 'same-as "SameAs" 'same-or-before "SameOrBefore"
          'same-or-after "SameOrAfter" 'meets "Meets" 'meets-before "MeetsBefore" 'meets-after "MeetsAfter"
          'overlaps "Overlaps" 'overlaps-before "OverlapsBefore" 'overlaps-after "OverlapsAfter"
          'starts "Starts" 'ends "Ends" 'includes "Includes" 'properly-includes "ProperIncludes"
          'included-in "IncludedIn" 'properly-included-in "ProperIncludedIn" 'during "IncludedIn"
          'in "In" 'contains "Contains" 'proper-in "ProperIn" 'proper-contains "ProperContains"))
(define (timing-elm-type op) (hash-ref timing-elm-types op (lambda () (symbol->string op))))
(define (component-precision-name prec)
  (case prec
    [(year) "Year"] [(month) "Month"] [(day) "Day"] [(hour) "Hour"]
    [(minute) "Minute"] [(second) "Second"] [(millisecond) "Millisecond"]
    [else (symbol->string prec)]))

(define (elm->jsexpr x)
  (if (elm-library? x) (library->jsexpr x) (expr->jsexpr x)))

;; ---- library ---------------------------------------------------------------
(define (library->jsexpr lib)
  (hasheq
   'library
   (drop-false
    (hasheq
     'identifier (drop-false (hasheq 'id (or (elm-library-id lib) 'null)
                                     'version (elm-library-version lib)))
     'schemaIdentifier (hasheq 'id "urn:hl7-org:elm" 'version "r1")
     'usings (and (pair? (elm-library-usings lib))
                  (hasheq 'def (map using->jsexpr (elm-library-usings lib))))
     'includes (section (elm-library-includes lib)
                        (lambda (i) (drop-false (hasheq 'localIdentifier (elm-include-local-id i)
                                                        'path (elm-include-path i) 'version (elm-include-version i)))))
     'parameters (section (elm-library-parameters lib)
                          (lambda (p) (drop-false (hasheq 'name (elm-parameter-def-name p)
                                                          'accessLevel (access->json (elm-parameter-def-access p))
                                                          'default (opt-expr (elm-parameter-def-default p))
                                                          'parameterTypeSpecifier (and (elm-parameter-def-type p)
                                                                                       (type->jsexpr (elm-parameter-def-type p)))))))
     'codeSystems (section (elm-library-codesystems lib)
                           (lambda (c) (drop-false (hasheq 'name (elm-codesystem-def-name c) 'id (elm-codesystem-def-id c)
                                                           'version (elm-codesystem-def-version c)
                                                           'accessLevel (access->json (elm-codesystem-def-access c))))))
     'valueSets (section (elm-library-valuesets lib)
                         (lambda (v) (drop-false (hasheq 'name (elm-valueset-def-name v) 'id (elm-valueset-def-id v)
                                                         'version (elm-valueset-def-version v)
                                                         'accessLevel (access->json (elm-valueset-def-access v))
                                                         'codeSystem (and (pair? (elm-valueset-def-codesystems v))
                                                                          (map term-ref-body (elm-valueset-def-codesystems v)))))))
     'codes (section (elm-library-codes lib)
                     (lambda (c) (drop-false (hasheq 'name (elm-code-def-name c) 'id (elm-code-def-id c)
                                                     'display (elm-code-def-display c)
                                                     'accessLevel (access->json (elm-code-def-access c))
                                                     'codeSystem (term-ref-body (elm-code-def-codesystem c))))))
     'concepts (section (elm-library-concepts lib)
                        (lambda (c) (drop-false (hasheq 'name (elm-concept-def-name c) 'display (elm-concept-def-display c)
                                                        'accessLevel (access->json (elm-concept-def-access c))
                                                        'code (map term-ref-body (elm-concept-def-codes c))))))
     'contexts (section (elm-library-contexts lib) (lambda (c) (hasheq 'name (elm-context-def-name c))))
     'statements (hasheq 'def (map statement->jsexpr (elm-library-statements lib)))))))

;; ELM operators whose operands are NAMED attributes rather than `operand` (aggregates read `source`,
;; Split reads stringToSplit/separator, …): op -> attribute names, in argument order
(define operator-attributes
  (hash "First" '(source) "Last" '(source) "Count" '(source) "Sum" '(source) "Min" '(source) "Max" '(source)
        "Avg" '(source) "Median" '(source) "Mode" '(source) "StdDev" '(source) "Variance" '(source)
        "PopulationStdDev" '(source) "PopulationVariance" '(source) "AllTrue" '(source) "AnyTrue" '(source)
        "Product" '(source) "GeometricMean" '(source)
        "Split" '(stringToSplit separator) "SplitOnMatches" '(stringToSplit separatorPattern)
        "Combine" '(source separator) "Substring" '(stringToSub startIndex length)
        "PositionOf" '(pattern string) "LastPositionOf" '(pattern string) "IndexOf" '(source element)
        "Slice" '(source startIndex endIndex) "Round" '(operand precision)
        "Message" '(source condition code severity message)))
;; operator-body : string (listof elm) boolean -> jsexpr   unary operands are an object, n-ary a list
(define (operator-body op operands unary?)
  (match (hash-ref operator-attributes op #f)
    [#f (hasheq 'type op 'operand (if unary? (expr->jsexpr (car operands)) (map expr->jsexpr operands)))]
    [names (for/fold ([h (hasheq 'type op)]) ([nm (in-list names)] [o (in-list operands)])
             (hash-set h nm (expr->jsexpr o)))]))

;; section : (listof X) (X -> jsexpr) -> jsexpr|#f   an ELM {def: […]} section, absent when empty
(define (section xs f) (and (pair? xs) (hasheq 'def (map f xs))))
;; a reference inside a definition (codeSystem of a code, codes of a concept): name + libraryName, no type
(define (term-ref-body r) (drop-false (hasheq 'name (elm-term-ref-name r) 'libraryName (elm-term-ref-library r))))
(define (sort-by->jsexpr b)
  (case (elm-sort-by-kind b)
    [(direction) (hasheq 'type "ByDirection" 'direction (elm-sort-by-direction b))]
    [(column) (hasheq 'type "ByColumn" 'direction (elm-sort-by-direction b) 'path (elm-sort-by-target b))]
    [else (hasheq 'type "ByExpression" 'direction (elm-sort-by-direction b)
                  'expression (expr->jsexpr (elm-sort-by-target b)))]))

(define (using->jsexpr u)
  (drop-false (hasheq 'localIdentifier (elm-using-local-id u)
                      'uri (elm-using-uri u)
                      'version (elm-using-version u))))

(define (statement->jsexpr s)
  (cond
    [(elm-def? s)
     (drop-false (hasheq 'name (elm-def-name s)
                         'context (or (elm-def-context s) "Unfiltered")
                         'accessLevel (and (elm-def-access s) (access->json (elm-def-access s)))
                         'expression (expr->jsexpr (elm-def-expression s))))]
    [(elm-function-def? s)
     (drop-false (hasheq 'type "FunctionDef" 'fluent (elm-function-def-fluent? s)
                         'name (elm-function-def-name s)
                         'context (or (elm-function-def-context s) "Unfiltered")
                         'accessLevel (access->json (elm-function-def-access s))
                         'operand (and (pair? (elm-function-def-operands s))
                                       (map operand->jsexpr (elm-function-def-operands s)))
                         'resultTypeSpecifier (and (elm-function-def-result-type s)
                                                   (type->jsexpr (elm-function-def-result-type s)))
                         ;; an external function has no body
                         'external (eq? (elm-function-def-expression s) 'external)
                         'expression (and (not (eq? (elm-function-def-expression s) 'external))
                                          (expr->jsexpr (elm-function-def-expression s)))))]
    [else (error 'elm->jsexpr "unknown statement: ~s" s)]))

(define (operand->jsexpr o)
  (drop-false (hasheq 'name (elm-operand-def-name o)
                      'operandTypeSpecifier (and (elm-operand-def-type o)
                                                 (type->jsexpr (elm-operand-def-type o))))))

(define (access->json a) (if (eq? a 'private) "Private" "Public"))

;; ---- expressions -----------------------------------------------------------
;; attach the inferred resultTypeName (as a qualified System name) when present
(define (with-rt h rt)
  (if rt (hash-set h 'resultTypeName (system-type-name rt)) h))

(define (expr->jsexpr e)
  (cond
    [(elm-literal? e)
     (with-rt (hasheq 'type "Literal"
                      'valueType (system-type-name (elm-literal-value-type e))
                      'value (elm-literal-value e))
              (elm-literal-value-type e))]
    [(elm-null? e)
     (drop-false (hasheq 'type "Null"
                         'valueType (and (elm-null-value-type e)
                                         (system-type-name (elm-null-value-type e)))))]
    [(elm-quantity? e)
     (with-rt (drop-false (hasheq 'type "Quantity"
                                  'value (elm-quantity-value e)
                                  'unit (elm-quantity-unit e)))
              "Quantity")]
    [(elm-ratio? e)
     (hasheq 'type "Ratio"
             'numerator (expr->jsexpr (elm-ratio-numerator e))
             'denominator (expr->jsexpr (elm-ratio-denominator e)))]
    [(elm-date? e)
     (drop-false (hasheq 'type "Date" 'year (opt-expr (elm-date-year e))
                         'month (opt-expr (elm-date-month e)) 'day (opt-expr (elm-date-day e))))]
    [(elm-time? e)
     (drop-false (hasheq 'type "Time" 'hour (opt-expr (elm-time-hour e))
                         'minute (opt-expr (elm-time-minute e)) 'second (opt-expr (elm-time-second e))
                         'millisecond (opt-expr (elm-time-millisecond e))))]
    [(elm-datetime? e)
     (drop-false (hasheq 'type "DateTime" 'year (opt-expr (elm-datetime-year e))
                         'month (opt-expr (elm-datetime-month e)) 'day (opt-expr (elm-datetime-day e))
                         'hour (opt-expr (elm-datetime-hour e)) 'minute (opt-expr (elm-datetime-minute e))
                         'second (opt-expr (elm-datetime-second e)) 'millisecond (opt-expr (elm-datetime-millisecond e))
                         'timezoneOffset (opt-expr (elm-datetime-timezone-offset e))))]
    [(elm-component? e)
     ;; ELM DateTimeComponentFrom / TimezoneOffsetFrom / DateFrom / TimeFrom
     ;; (only DateTimeComponentFrom names a precision: the others ARE one)
     (let ([type (component-elm-type (elm-component-precision e))])
       (drop-false (hasheq 'type type
                           'operand (expr->jsexpr (elm-component-operand e))
                           'precision (and (equal? type "DateTimeComponentFrom")
                                           (component-precision-name (elm-component-precision e))))))]
    [(elm-between? e)
     (hasheq 'type (if (eq? (elm-between-kind e) 'difference) "DifferenceBetween" "DurationBetween")
             'precision (component-precision-name (elm-between-precision e))
             'operand (list (expr->jsexpr (elm-between-a e)) (expr->jsexpr (elm-between-b e))))]
    [(elm-timing? e)
     (drop-false (hasheq 'type (timing-elm-type (elm-timing-op e))
                         'precision (and (elm-timing-precision e) (component-precision-name (elm-timing-precision e)))
                         'operand (list (expr->jsexpr (elm-timing-a e)) (expr->jsexpr (elm-timing-b e)))))]
    [(elm-instance? e)
     (hasheq 'type "Instance" 'classType (elm-instance-class-type e)
             'element (for/list ([te (in-list (elm-instance-elements e))])
                        (hasheq 'name (symbol->string (elm-tuple-elem-name te))
                                'value (expr->jsexpr (elm-tuple-elem-value te)))))]
    [(elm-query? e)
     ;; NOTE: a reduced ELM Query shape (full ELM AliasedQuerySource/RelationshipClause detail is a
     ;; deferred canonicalization) — sufficient to describe the query; the interpreter runs it.
     (drop-false
      (hasheq 'type "Query"
              'source (for/list ([s (in-list (elm-query-sources e))])
                        (hasheq 'alias (car s) 'expression (expr->jsexpr (cdr s))))
              ;; the reference translator always emits relationship, even empty
              'relationship (for/list ([r (in-list (elm-query-relationships e))])
                              (hasheq 'type (if (eq? (elm-relationship-kind r) 'with) "With" "Without")
                                      'alias (elm-relationship-alias r)
                                      'expression (expr->jsexpr (elm-relationship-expression r))
                                      'suchThat (expr->jsexpr (elm-relationship-such-that r))))
              'let (and (pair? (elm-query-lets e))
                        (for/list ([l (in-list (elm-query-lets e))])
                          (hasheq 'identifier (car l) 'expression (expr->jsexpr (cdr l)))))
              'where (and (elm-query-where e) (expr->jsexpr (elm-query-where e)))
              'return (and (elm-query-ret e)
                           ;; distinct: #t written, 'all = false written, #f (unstated) omitted
                           (let ([h (hasheq 'expression (expr->jsexpr (elm-query-ret e)))])
                             (case (elm-query-ret-distinct e)
                               [(#t) (hash-set h 'distinct #t)] [(all) (hash-set h 'distinct #f)] [else h])))
              'sort (and (elm-query-sort e) (hasheq 'by (map sort-by->jsexpr (elm-query-sort e))))))]
    [(elm-retrieve? e)
     (drop-false (hasheq 'type "Retrieve" 'dataType (elm-retrieve-data-type e)
                         'templateId (elm-retrieve-template-id e)
                         'codeProperty (elm-retrieve-code-property e)
                         'codeComparator (elm-retrieve-code-comparator e)
                         'codes (opt-expr (elm-retrieve-codes e))))]
    [(elm-alias-ref? e) (hasheq 'type "AliasRef" 'name (elm-alias-ref-name e))]
    [(elm-query-let-ref? e) (hasheq 'type "QueryLetRef" 'name (elm-query-let-ref-name e))]
    [(elm-identifier-ref? e) (hasheq 'type "IdentifierRef" 'name (elm-identifier-ref-name e))]
    [(elm-term-ref? e) (hash-set (term-ref-body e) 'type (elm-term-ref-kind e))]
    [(elm-code? e)
     (drop-false (hasheq 'type "Code" 'code (elm-code-code e) 'system (term-ref-body (elm-code-system e))
                         'display (elm-code-display e)))]
    [(elm-concept? e)
     (drop-false (hasheq 'type "Concept" 'code (map expr->jsexpr (elm-concept-codes e)) 'display (elm-concept-display e)))]
    [(elm-in-valueset? e)
     (hasheq 'type (elm-in-valueset-kind e)
             (if (regexp-match? #rx"^Any" (elm-in-valueset-kind e)) 'codes 'code) (expr->jsexpr (elm-in-valueset-code e))
             'valueset (expr->jsexpr (elm-in-valueset-valueset e)))]
    [(elm-minmax? e)
     (hasheq 'type (if (eq? (elm-minmax-which e) 'min) "MinValue" "MaxValue") 'valueType (elm-minmax-value-type e))]
    [(elm-precision-op? e)
     (hasheq 'type (elm-precision-op-op e) 'precision (elm-precision-op-precision e)
             'operand (map expr->jsexpr (elm-precision-op-operands e)))]
    [(elm-nary? e)
     (with-rt (operator-body (elm-nary-op e) (elm-nary-operands e) #f) (elm-nary-result-type e))]
    [(elm-unary? e)
     (with-rt (operator-body (elm-unary-op e) (list (elm-unary-operand e)) #t) (elm-unary-result-type e))]
    [(elm-interval? e)
     ;; lowClosed/highClosed are meaningful booleans — must NOT be dropped when #f; a closed flag
     ;; that is an expression (a promoted interval reads the original's) is lowClosedExpression
     (define (closed h key expr-key v)
       (if (boolean? v) (hash-set h key v) (hash-set h expr-key (expr->jsexpr v))))
     (closed (closed (drop-false (hasheq 'type "Interval"
                                         'low (and (elm-interval-low e) (expr->jsexpr (elm-interval-low e)))
                                         'high (and (elm-interval-high e) (expr->jsexpr (elm-interval-high e)))))
                     'lowClosed 'lowClosedExpression (elm-interval-low-closed e))
             'highClosed 'highClosedExpression (elm-interval-high-closed e))]
    [(elm-list? e)
     (hasheq 'type "List" 'element (map expr->jsexpr (elm-list-elements e)))]
    [(elm-tuple? e)
     (hasheq 'type "Tuple"
             'element (map (lambda (te)
                             (hasheq 'name (symbol->string (elm-tuple-elem-name te))
                                     'value (expr->jsexpr (elm-tuple-elem-value te))))
                           (elm-tuple-elements e)))]
    [(elm-if? e)
     (hasheq 'type "If"
             'condition (expr->jsexpr (elm-if-cond e))
             'then (expr->jsexpr (elm-if-then e))
             'else (expr->jsexpr (elm-if-els e)))]
    [(elm-case? e)
     (drop-false (hasheq 'type "Case"
                         'comparand (and (elm-case-comparand e) (expr->jsexpr (elm-case-comparand e)))
                         'caseItem (map (lambda (ci)
                                          (hasheq 'when (expr->jsexpr (elm-case-item-when ci))
                                                  'then (expr->jsexpr (elm-case-item-then ci))))
                                        (elm-case-items e))
                         'else (and (elm-case-els e) (expr->jsexpr (elm-case-els e)))))]
    [(elm-property? e)
     (drop-false (hasheq 'type "Property" 'source (opt-expr (elm-property-source e)) 'path (elm-property-path e)
                         'scope (elm-property-scope e)))]
    [(elm-expression-ref? e)
     (drop-false (hasheq 'type "ExpressionRef" 'name (elm-expression-ref-name e)
                         'libraryName (elm-expression-ref-library e)))]
    [(elm-function-ref? e)
     (drop-false (hasheq 'type "FunctionRef" 'name (elm-function-ref-name e)
                         'libraryName (elm-function-ref-library e)
                         'operand (and (pair? (elm-function-ref-operands e))
                                       (map expr->jsexpr (elm-function-ref-operands e)))
                         'signature (and (elm-function-ref-signature e) (map type->jsexpr (elm-function-ref-signature e)))))]
    [(elm-operand-ref? e) (hasheq 'type "OperandRef" 'name (elm-operand-ref-name e))]
    [(elm-parameter-ref? e)
     (drop-false (hasheq 'type "ParameterRef" 'name (elm-parameter-ref-name e)
                         'libraryName (elm-parameter-ref-library e)))]
    [(elm-is? e)
     ;; an implicit test names its type as a string (isType)
     (hasheq 'type "Is" 'operand (expr->jsexpr (elm-is-operand e))
             (if (string? (elm-is-type e)) 'isType 'isTypeSpecifier)
             (if (string? (elm-is-type e)) (elm-is-type e) (type->jsexpr (elm-is-type e))))]
    [(elm-as? e)
     ;; an implicit cast names its type as a string (asType); an explicit one as a specifier;
     ;; strict is always written (the reference translator writes false too)
     (hasheq 'type "As" 'operand (expr->jsexpr (elm-as-operand e))
             (if (string? (elm-as-type e)) 'asType 'asTypeSpecifier)
             (if (string? (elm-as-type e)) (elm-as-type e) (type->jsexpr (elm-as-type e)))
             'strict (and (elm-as-strict e) #t))]
    [(elm-to? e)
     (with-rt (hasheq 'type (string-append "To" (elm-to-to-type e))
                      'operand (expr->jsexpr (elm-to-operand e)))
              (elm-to-to-type e))]
    [else (error 'elm->jsexpr "unknown expression: ~s" e)]))

;; ---- type specifiers -------------------------------------------------------
(define (type->jsexpr t)
  (cond
    [(elm-named-type? t) (hasheq 'type "NamedTypeSpecifier" 'name (elm-named-type-name t))]
    [(elm-interval-type? t)
     (hasheq 'type "IntervalTypeSpecifier" 'pointType (type->jsexpr (elm-interval-type-point t)))]
    [(elm-list-type? t)
     (hasheq 'type "ListTypeSpecifier" 'elementType (type->jsexpr (elm-list-type-element t)))]
    [(elm-choice-type? t) (hasheq 'type "ChoiceTypeSpecifier" 'choice (map type->jsexpr (elm-choice-type-types t)))]
    [(elm-tuple-type? t)
     (hasheq 'type "TupleTypeSpecifier"
             'element (for/list ([el (in-list (elm-tuple-type-elements t))])
                        (hasheq 'name (car el) 'elementType (type->jsexpr (cdr el)))))]
    [else (error 'elm->jsexpr "unknown type specifier: ~s" t)]))

(module+ test
  (require rackunit)
  ;; a literal
  (check-equal? (expr->jsexpr (elm-literal "Integer" "5"))
                (hasheq 'type "Literal" 'valueType "{urn:hl7-org:elm-types:r1}Integer"
                        'value "5" 'resultTypeName "{urn:hl7-org:elm-types:r1}Integer"))
  ;; a binary op with operands
  (check-equal? (expr->jsexpr (elm-nary "Add" (list (elm-literal "Integer" "1") (elm-literal "Integer" "2")) "Integer"))
                (hasheq 'type "Add"
                        'operand (list (expr->jsexpr (elm-literal "Integer" "1"))
                                       (expr->jsexpr (elm-literal "Integer" "2")))
                        'resultTypeName "{urn:hl7-org:elm-types:r1}Integer"))
  ;; interval with closed flags
  (check-equal? (hash-ref (expr->jsexpr (elm-interval (elm-literal "Integer" "1") (elm-literal "Integer" "10") #t #f)) 'highClosed) #f)
  ;; a whole library round-trips to a JSON string containing the library wrapper
  (define lib (elm-library "Demo" "1.0" '() '() '() '() '() '() '() '() (list (elm-def "X" #f 'public (elm-literal "Integer" "1")))))
  (define js (library->jsexpr lib))
  (check-true (hash-has-key? js 'library))
  (check-equal? (hash-ref (hash-ref (hash-ref js 'library) 'identifier) 'id) "Demo")
  (check-true (string? (elm->json-string lib))))
