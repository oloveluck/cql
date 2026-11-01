#lang racket/base

;;; json.rkt — ELM struct -> canonical ELM JSON.
;;;
;;; ELM is plain JSON (NOT a FHIR resource), so we emit jsexpr directly and stringify with
;;; racket/json — routing through serialize/to-json.rkt would inject FHIR resourceType/_field
;;; behavior. `drop-false` (shared util) omits absent optional attributes. Each expression object
;;; carries its ELM `type` discriminator; a full library wraps in {"library": {…}}.

(require racket/contract
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
(define (timing-elm-type op)
  (case op [(before) "Before"] [(after) "After"] [(same-as) "SameAs"]
    [(same-or-before) "SameOrBefore"] [(same-or-after) "SameOrAfter"] [else (symbol->string op)]))
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
     'statements (hasheq 'def (map statement->jsexpr (elm-library-statements lib)))))))

(define (using->jsexpr u)
  (drop-false (hasheq 'localIdentifier (elm-using-local-id u)
                      'uri (elm-using-uri u)
                      'version (elm-using-version u))))

(define (statement->jsexpr s)
  (cond
    [(elm-def? s)
     (drop-false (hasheq 'name (elm-def-name s)
                         'context (or (elm-def-context s) "Unfiltered")
                         'accessLevel (access->json (elm-def-access s))
                         'expression (expr->jsexpr (elm-def-expression s))))]
    [(elm-function-def? s)
     (drop-false (hasheq 'type "FunctionDef"
                         'name (elm-function-def-name s)
                         'context (or (elm-function-def-context s) "Unfiltered")
                         'accessLevel (access->json (elm-function-def-access s))
                         'operand (and (pair? (elm-function-def-operands s))
                                       (map operand->jsexpr (elm-function-def-operands s)))
                         'resultTypeSpecifier (and (elm-function-def-result-type s)
                                                   (type->jsexpr (elm-function-def-result-type s)))
                         'expression (expr->jsexpr (elm-function-def-expression s))))]
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
     (hasheq 'type (component-elm-type (elm-component-precision e))
             'operand (expr->jsexpr (elm-component-operand e))
             'precision (component-precision-name (elm-component-precision e)))]
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
                        (hasheq 'type "AliasedQuerySource" 'alias (car s) 'expression (expr->jsexpr (cdr s))))
              'let (and (pair? (elm-query-lets e))
                        (for/list ([l (in-list (elm-query-lets e))])
                          (hasheq 'identifier (car l) 'expression (expr->jsexpr (cdr l)))))
              'where (and (elm-query-where e) (expr->jsexpr (elm-query-where e)))
              'return (and (elm-query-ret e)
                           (drop-false (hasheq 'distinct (elm-query-ret-distinct e) 'expression (expr->jsexpr (elm-query-ret e)))))
              'sort (and (elm-query-sort e)
                         (hasheq 'by (list (hasheq 'type "ByDirection" 'direction (symbol->string (elm-query-sort e))))))))]
    [(elm-nary? e)
     (with-rt (hasheq 'type (elm-nary-op e)
                      'operand (map expr->jsexpr (elm-nary-operands e)))
              (elm-nary-result-type e))]
    [(elm-unary? e)
     (with-rt (hasheq 'type (elm-unary-op e) 'operand (expr->jsexpr (elm-unary-operand e)))
              (elm-unary-result-type e))]
    [(elm-interval? e)
     ;; lowClosed/highClosed are meaningful booleans — must NOT be dropped when #f
     (hash-set*
      (drop-false (hasheq 'type "Interval"
                          'low (and (elm-interval-low e) (expr->jsexpr (elm-interval-low e)))
                          'high (and (elm-interval-high e) (expr->jsexpr (elm-interval-high e)))))
      'lowClosed (elm-interval-low-closed e)
      'highClosed (elm-interval-high-closed e))]
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
     (hasheq 'type "Property" 'source (expr->jsexpr (elm-property-source e)) 'path (elm-property-path e))]
    [(elm-expression-ref? e)
     (drop-false (hasheq 'type "ExpressionRef" 'name (elm-expression-ref-name e)
                         'libraryName (elm-expression-ref-library e)))]
    [(elm-function-ref? e)
     (drop-false (hasheq 'type "FunctionRef" 'name (elm-function-ref-name e)
                         'libraryName (elm-function-ref-library e)
                         'operand (and (pair? (elm-function-ref-operands e))
                                       (map expr->jsexpr (elm-function-ref-operands e)))))]
    [(elm-operand-ref? e) (hasheq 'type "OperandRef" 'name (elm-operand-ref-name e))]
    [(elm-parameter-ref? e)
     (drop-false (hasheq 'type "ParameterRef" 'name (elm-parameter-ref-name e)
                         'libraryName (elm-parameter-ref-library e)))]
    [(elm-is? e)
     (hasheq 'type "Is" 'operand (expr->jsexpr (elm-is-operand e))
             'isTypeSpecifier (type->jsexpr (elm-is-type e)))]
    [(elm-as? e)
     (drop-false (hasheq 'type "As" 'operand (expr->jsexpr (elm-as-operand e))
                         'asTypeSpecifier (type->jsexpr (elm-as-type e))
                         'strict (elm-as-strict e)))]
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
  (define lib (elm-library "Demo" "1.0" '() (list (elm-def "X" #f 'public (elm-literal "Integer" "1")))))
  (define js (library->jsexpr lib))
  (check-true (hash-has-key? js 'library))
  (check-equal? (hash-ref (hash-ref (hash-ref js 'library) 'identifier) 'id) "Demo")
  (check-true (string? (elm->json-string lib))))
