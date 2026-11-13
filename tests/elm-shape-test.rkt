#lang racket/base

;;; elm-shape-test.rkt — the ELM written for constructs where it once departed from the reference
;;; translator's (and, for the JSON, from the ELM schema), each with the value it still evaluates to.
;;; The corpora (elm-corpus-test, qicore-elm-corpus-test) count definitions; these pin each shape:
;;;   a union chain's grouping at any length; `ValueSet contains x` read as `x in ValueSet`;
;;;   Expand/Collapse as binary operators; a point promoted to an interval beside one; `/` over
;;;   Decimals; Skip, Take and Tail as Slices; the return a multi-source query implies; a query's
;;;   aggregate clause; list elements
;;;   meeting at one type and a typed list selector's type; branches of different types meeting at
;;;   their choice; a choice inside Coalesce; Combine over a model's strings.

(require rackunit racket/list racket/match
         (only-in cql/parser/parse parse-cql parse-cql-expr)
         (only-in cql/compile compile-expr compile-library current-library-resolver)
         (only-in cql/elm/json elm->jsexpr)
         (only-in cql/interp run-elm make-ienv)
         (only-in cql/value cql-null? cql-tuple? render)
         (only-in cql/run evaluate-library bundled-library)
         (only-in fhirpath/decimal fp-decimal?)
         (only-in fhir/result/result ok? ok-value err-error))

;; elm : expression-text -> jsexpr     value : expression-text -> CqlValue
(define (compiled text) (ok-value (compile-expr (ok-value (parse-cql-expr text)))))
(define (elm text) (elm->jsexpr (compiled text)))
(define (value text) (run-elm (compiled text) (make-ienv)))
(define (at j . keys) (for/fold ([j j]) ([k (in-list keys)]) (if (number? k) (list-ref j k) (hash-ref j k))))
(define (type-of j) (hash-ref j 'type))

;; ---- union chains: adjacent operands pair up, the pairs fold left, an odd operand comes last ----
;; shape : jsexpr -> tree of 'x   the grouping of a Union tree
(define (shape j) (if (equal? (hash-ref j 'type #f) "Union") (map shape (hash-ref j 'operand)) 'x))
(define (chain n) (apply string-append (add-between (for/list ([i (in-range n)]) (format "{~a}" i)) " union ")))
(check-equal? (shape (elm (chain 2))) '(x x))
(check-equal? (shape (elm (chain 3))) '((x x) x))
(check-equal? (shape (elm (chain 5))) '(((x x) (x x)) x))
;; seven and more were regrouped a second time on the way down: (((x x) ((x x) (x x))) x)
(check-equal? (shape (elm (chain 7))) '((((x x) (x x)) (x x)) x))
(check-equal? (shape (elm (chain 8))) '((((x x) (x x)) (x x)) (x x)))
(check-equal? (shape (elm (chain 11))) '((((((x x) (x x)) (x x)) (x x)) (x x)) x))
(check-equal? (value (chain 7)) '(0 1 2 3 4 5 6))
;; a parenthesized right operand is its own chain
(check-equal? (shape (elm "{1} union ({2} union {3} union {4} union {5})")) '(x ((x x) (x x))))

;; ---- Expand and Collapse are binary: with no `per`, the second operand is Null -------------------
(check-equal? (map type-of (at (elm "expand { Interval[1, 3] }") 'operand)) '("List" "Null"))
(check-equal? (map type-of (at (elm "collapse { Interval[1, 3], Interval[2, 5] }") 'operand)) '("List" "Null"))
(check-equal? (map type-of (at (elm "expand { Interval[1, 3] } per 2")  'operand)) '("List" "Literal"))
(check-equal? (render (value "expand { Interval[1, 3] }")) (render (value "{ Interval[1, 1], Interval[2, 2], Interval[3, 3] }")))
(check-equal? (render (value "collapse { Interval[1, 3], Interval[2, 5] }")) (render (value "{ Interval[1, 5] }")))

;; ---- a point beside an interval under an operator relating intervals is the interval [P, P] ------
(let ([j (elm "5 before Interval[7, 9]")])
  (check-equal? (type-of j) "Before")
  (check-equal? (type-of (at j 'operand 0)) "If")
  (check-equal? (type-of (at j 'operand 0 'condition)) "IsNull")
  (check-equal? (type-of (at j 'operand 0 'then)) "Null")
  (check-equal? (at j 'operand 0 'else 'low) (at j 'operand 0 'else 'high))
  (check-equal? (type-of (at j 'operand 1)) "Interval"))
(check-equal? (type-of (at (elm "Interval[1, 3] after 0") 'operand 1)) "If")
;; membership keeps the point: `P during I` is In(P, I)
(check-equal? (map type-of (at (elm "5 during Interval[1, 9]") 'operand)) '("Literal" "Interval"))
(check-equal? (value "5 before Interval[7, 9]") #t)
(check-equal? (value "5 after Interval[7, 9]") #f)
(check-true (cql-null? (value "(null as Integer) before Interval[7, 9]")))

;; ---- `/` is defined on Decimals: Integer operands are converted --------------------------------
(check-equal? (map type-of (at (elm "1 / 2") 'operand)) '("ToDecimal" "ToDecimal"))
(check-equal? (map type-of (at (elm "1.0 / 2") 'operand)) '("Literal" "ToDecimal"))
(check-equal? (map type-of (at (elm "6 'mg' / 2") 'operand)) '("Quantity" "ToQuantity"))
(check-equal? (render (value "1 / 2")) "0.5")

;; ---- a multi-source query without a return returns, distinct, the tuple of its aliases -----------
(let ([j (elm "from ({1, 2}) A, ({3}) B")])
  (check-equal? (at j 'return 'distinct) #t)
  (check-equal? (type-of (at j 'return 'expression)) "Tuple")
  (check-equal? (map (lambda (e) (cons (hash-ref e 'name) (at e 'value 'name))) (at j 'return 'expression 'element))
                '(("A" . "A") ("B" . "B"))))
(check-false (hash-has-key? (elm "from ({1, 2}) A where A > 1") 'return))          ; one source: no return
(check-false (hash-has-key? (elm "from ({1, 2}) A, ({3}) B aggregate R starting 0: R + A + B") 'return))
(check-equal? (length (value "from ({1, 2}) A, ({3}) B")) 2)
(check-true (andmap cql-tuple? (value "from ({1, 2}) A, ({3}) B")))
(check-equal? (length (value "from ({1, 1}) A, ({3}) B")) 1)                        ; distinct
(check-equal? (value "from ({1, 2}) A, ({3}) B aggregate R starting 0: R + A + B") 9)

;; ---- Skip, Take and Tail are Slices (ELM has no operators of those names) -----------------------
(define (slice-of text) (let ([j (elm text)]) (list (type-of j) (type-of (hash-ref j 'startIndex)) (type-of (hash-ref j 'endIndex)))))
(check-equal? (slice-of "Skip({1, 2, 3}, 1)") '("Slice" "Literal" "Null"))
(check-equal? (slice-of "Take({1, 2, 3}, 2)") '("Slice" "Literal" "Coalesce"))
(check-equal? (slice-of "Tail({1, 2, 3})") '("Slice" "Literal" "Null"))
(check-equal? (elm "({1, 2, 3}).take(2)") (elm "Take({1, 2, 3}, 2)"))              ; the method is the function
(check-equal? (elm "({1, 2, 3}).skip(1)") (elm "Skip({1, 2, 3}, 1)"))
(check-equal? (elm "({1, 2, 3}).tail()") (elm "Tail({1, 2, 3})"))
(check-equal? (list (value "Skip({1, 2, 3}, 1)") (value "Take({1, 2, 3}, 2)") (value "Tail({1, 2, 3})")) '((2 3) (1 2) (2 3)))
(check-equal? (list (value "Take({1, 2, 3}, null as Integer)") (value "({1, 2, 3}).take(null as Integer)")) '(() ()))
(check-equal? (value "Skip({1, 2, 3}, null as Integer)") '(1 2 3))
;; a NEGATIVE count is then Slice's negative index, which the HL7 suite counts from the end
;; (Slice({1,2,3,4,5}, 1, -1) = {2,3,4}); the specification's prose for Take and Skip says the empty
;; list. The reference's translation decides here, as for every other shape in this file.
(check-equal? (list (value "Take({1, 2, 3}, -1)") (value "Skip({1, 2, 3}, -1)")) '((1 2) (3)))
(check-regexp-match #rx"Take takes a list and a count, got 1 argument.*put the query in parentheses"
                    (format "~a" (err-error (compile-expr (ok-value (parse-cql-expr "Take({1, 2, 3})"))))))

;; ---- an aggregate clause is written, its accumulator read as a QueryLetRef ----------------------
(let ([j (elm "from ({1, 2, 3}) A aggregate R starting 0: R + A")])
  (check-equal? (at j 'aggregate 'identifier) "R")
  (check-equal? (at j 'aggregate 'distinct) #f)
  (check-equal? (type-of (at j 'aggregate 'starting)) "Literal")
  (check-equal? (map type-of (at j 'aggregate 'expression 'operand)) '("QueryLetRef" "AliasRef")))
(check-equal? (at (elm "from ({1, 1}) A aggregate distinct R starting 0: R + A") 'aggregate 'distinct) #t)
(check-false (hash-has-key? (elm "from ({1, 2}) A where A > 1") 'aggregate))
(check-equal? (value "from ({1, 2, 3}) A aggregate R starting 0: R + A") 6)

;; ---- list elements meet at one type; a typed list selector's elements are its type's ------------
(check-equal? (map type-of (at (elm "{ 1, 2.0 }") 'element)) '("ToDecimal" "Literal"))
(check-equal? (map type-of (at (elm "{ 1, 2 }") 'element)) '("Literal" "Literal"))
(check-equal? (map type-of (at (elm "List<Decimal> { 1, 2 }") 'element)) '("ToDecimal" "ToDecimal"))
(check-true (andmap fp-decimal? (value "List<Decimal> { 1, 2 }")))                 ; they were Integers
(let ([j (elm "List<Choice<Integer, Decimal>> { 1, 1.0 }")])
  (check-equal? (map type-of (at j 'element)) '("As" "As"))
  (check-equal? (map (lambda (c) (hash-ref c 'name)) (at j 'element 0 'asTypeSpecifier 'choice))
                '("{urn:hl7-org:elm-types:r1}Integer" "{urn:hl7-org:elm-types:r1}Decimal")))
(check-equal? (elm "List<Integer> { }") (elm "{ }"))
(check-equal? (value "List<Integer> { 1, 2 }") '(1 2))

;; ---- branches of different types meet at their choice, each cast to it --------------------------
(let ([j (elm "if true then 1 else 'a'")])
  (check-equal? (map type-of (list (at j 'then) (at j 'else))) '("As" "As"))
  (check-equal? (map (lambda (c) (hash-ref c 'name)) (at j 'then 'asTypeSpecifier 'choice))
                '("{urn:hl7-org:elm-types:r1}Integer" "{urn:hl7-org:elm-types:r1}String"))
  (check-equal? (at j 'then 'asTypeSpecifier) (at j 'else 'asTypeSpecifier)))
(check-equal? (type-of (at (elm "if true then 1 else 2") 'then)) "Literal")        ; one type: no cast
(check-equal? (type-of (at (elm "if true then 1 else 2.0") 'then)) "ToDecimal")    ; promotable: promoted
(check-equal? (type-of (at (elm "if true then 1 else null") 'else)) "As")          ; a null takes the type
(check-equal? (value "if true then 1 else 'a'") 1)
(check-equal? (value "case when false then 1 when true then 'a' else 2.0 end") "a")

;; ---- over a data model: `ValueSet contains x`, a choice in Coalesce, Combine ---------------------
(define head "library T version '1'\nusing FHIR version '4.0.1'\ninclude FHIRHelpers version '4.0.1'\nvalueset \"VS\": 'http://x/vs'\ncontext Patient\n")
(define (library text) (ok-value (parse-cql (string-append head text))))
(define (definitions lib)
  (define c (parameterize ([current-library-resolver bundled-library]) (compile-library lib)))
  (check-true (ok? c) (if (ok? c) "" (format "~a" (err-error c))))
  (for/hash ([d (in-list (at (elm->jsexpr (ok-value c)) 'library 'statements 'def))])
    (values (hash-ref d 'name) (hash-ref d 'expression))))
(define patient (hasheq 'resourceType "Patient" 'id "p" 'name (list (hasheq 'given '("Ann" "Marie") 'family "Lee"))))
(define observation
  (hasheq 'resourceType "Observation" 'id "o" 'status "final" 'subject (hasheq 'reference "Patient/p")
          'code (hasheq 'coding (list (hasheq 'system "http://loinc.org" 'code "1-1"))) 'valueBoolean #t))
(define vs (hasheq 'resourceType "ValueSet" 'url "http://x/vs"
                   'expansion (hasheq 'contains (list (hasheq 'system "http://loinc.org" 'code "1-1")))))
(define lib
  (library (string-append
            "define \"In\": exists ([Observation] O where O.code in \"VS\")\n"
            "define \"Contains\": exists ([Observation] O where \"VS\" contains O.code)\n"
            "define \"Flag\": First([Observation] O return Coalesce(O.value, false))\n"
            "define \"Given\": Combine(First(Patient.name).given, ' ')\n")))
(define defs (definitions lib))
;; `"VS" contains x` is InValueSet with the code and the value set, as `x in "VS"` is (it compiled to
;; Contains(ValueSetRef, x), which failed when run: "In: not a list or interval")
(check-equal? (type-of (at (hash-ref defs "Contains") 'operand 'where)) "InValueSet")
(check-equal? (at (hash-ref defs "Contains") 'operand 'where 'code) (at (hash-ref defs "In") 'operand 'where 'code))
;; (written first, the value set is named as `valuesetExpression` too)
(check-equal? (at (hash-ref defs "Contains") 'operand 'where 'valueset) (at (hash-ref defs "In") 'operand 'where 'valueset))
(check-equal? (at (hash-ref defs "Contains") 'operand 'where 'valuesetExpression) (at (hash-ref defs "In") 'operand 'where 'valueset))
(check-false (hash-has-key? (at (hash-ref defs "In") 'operand 'where) 'valuesetExpression))
;; a choice among Coalesce's arguments is read through the member converting to the others' type
(let ([args (at (hash-ref defs "Flag") 'source 'return 'expression 'operand)])
  (check-equal? (hash-ref (first args) 'name) "ToBoolean")
  (check-equal? (at (first args) 'operand 0 'asType) "{http://hl7.org/fhir}boolean")
  (check-equal? (type-of (second args)) "Literal"))
;; Combine's list of model strings is converted item by item, like any System operator's list argument
(check-equal? (type-of (at (hash-ref defs "Given") 'source)) "Query")
(check-equal? (at (hash-ref defs "Given") 'source 'return 'expression 'name) "ToString")
(define results (evaluate-library lib (list patient observation) #:valuesets (list vs)))
(check-equal? (hash-ref results "In") #t)
(check-equal? (hash-ref results "Contains") #t)
(check-equal? (hash-ref results "Flag") #t)
(check-equal? (hash-ref results "Given") "Ann Marie")
