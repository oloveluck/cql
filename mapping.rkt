#lang racket/base

;;; mapping.rkt — a ModelInfo element's `target` mapping, parsed.
;;;
;;; A mapping model (QI-Core, US Core: a ModelInfo with a targetUrl) is a view over another model's
;;; data: each element may say how its value is computed from the target model's structure. The
;;; mapping language is small; compile.rkt turns a parsed mapping into ELM over the target model.
;;;
;;;   mapping     ::= alternative (';' alternative)*
;;;   alternative ::= [Type ':'] term                 a choice element's member and how it is read
;;;   term        ::= Name ('.' Name)* '(' term ')'    a function: FHIRHelpers.ToConcept(%value)
;;;                 | ('%value' | '%parent') step*
;;;   step        ::= '.' Name ['[x]']                  an element (a choice element's [x] dropped)
;;;                 | '[' condition (',' condition)* ']'   the items meeting every condition
;;;   condition   ::= Name ('.' Name)* '=' '\'' text '\''
;;;
;;; A Mapping is (listof (cons type|#f Term)).
;;; A Term is one of
;;;   (map-call name Term)          name: "FHIRHelpers.ToConcept"
;;;   (map-path root steps)         root: 'value | 'parent ; steps: (listof Step)
;;; A Step is (map-field name) | (map-filter (listof (cons (listof name) text))).
;;; A mapping that is just a type name (a class-level target: "Observation") is not an element
;;; mapping and is not read here.

(require racket/contract racket/match racket/list racket/string)

(provide (struct-out map-call) (struct-out map-path) (struct-out map-field) (struct-out map-filter)
         (contract-out [parse-mapping (-> string? (listof pair?))]))

(struct map-call (name arg) #:prefab)
(struct map-path (root steps) #:prefab)
(struct map-field (name) #:prefab)
(struct map-filter (conditions) #:prefab)

;; parse-mapping : string -> Mapping   raises on text outside the grammar
(define (parse-mapping s)
  (for/list ([alt (in-list (split-top s #\;))])
    (match (regexp-match #px"^\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*:(?!:)(.*)$" alt)
      [(list _ type rest) #:when (not (regexp-match? #rx"^[%]" (string-trim alt))) (cons type (parse-term (string-trim rest)))]
      [_ (cons #f (parse-term (string-trim alt)))])))

;; split-top : string char -> (listof string)   split at the char outside brackets and quotes
(define (split-top s ch)
  (let loop ([cs (string->list s)] [depth 0] [quote? #f] [cur '()] [acc '()])
    (cond [(null? cs) (reverse (cons (list->string (reverse cur)) acc))]
          [else
           (define c (car cs))
           (cond [(char=? c #\') (loop (cdr cs) depth (not quote?) (cons c cur) acc)]
                 [quote? (loop (cdr cs) depth quote? (cons c cur) acc)]
                 [(memv c '(#\( #\[)) (loop (cdr cs) (add1 depth) quote? (cons c cur) acc)]
                 [(memv c '(#\) #\])) (loop (cdr cs) (sub1 depth) quote? (cons c cur) acc)]
                 [(and (char=? c ch) (= depth 0)) (loop (cdr cs) depth quote? '() (cons (list->string (reverse cur)) acc))]
                 [else (loop (cdr cs) depth quote? (cons c cur) acc)])])))

;; parse-term : string -> Term
(define (parse-term s)
  (match s
    [(pregexp #px"^([A-Za-z][A-Za-z0-9_]*(?:\\.[A-Za-z][A-Za-z0-9_]*)*)\\((.*)\\)$" (list _ name inner))
     (map-call name (parse-term (string-trim inner)))]
    [(pregexp #px"^%(value|parent)(.*)$" (list _ root rest))
     (map-path (string->symbol root) (parse-steps rest))]
    [_ (error 'parse-mapping "not a mapping: ~a" s)]))

;; parse-steps : string -> (listof Step)
(define (parse-steps s)
  (let loop ([s s] [acc '()])
    (match s
      ["" (reverse acc)]
      [(pregexp #px"^\\.([A-Za-z][A-Za-z0-9_]*)(?:\\[x\\])?(.*)$" (list _ name rest)) (loop rest (cons (map-field name) acc))]
      [(pregexp #px"^\\[" (list _))
       (define close (matching-bracket s))
       (loop (substring s (add1 close)) (cons (map-filter (map parse-condition (split-top (substring s 1 close) #\,))) acc))]
      [_ (error 'parse-mapping "not a step: ~a" s)])))
(define (matching-bracket s)
  (let loop ([i 1] [depth 1] [quote? #f])
    (define c (string-ref s i))
    (cond [(char=? c #\') (loop (add1 i) depth (not quote?))]
          [quote? (loop (add1 i) depth quote?)]
          [(char=? c #\[) (loop (add1 i) (add1 depth) quote?)]
          [(char=? c #\]) (if (= depth 1) i (loop (add1 i) (sub1 depth) quote?))]
          [else (loop (add1 i) depth quote?)])))
(define (parse-condition c)
  (match (string-trim c)
    [(pregexp #px"^([A-Za-z][A-Za-z0-9_.]*)\\s*=\\s*'([^']*)'$" (list _ path text)) (cons (string-split path ".") text)]
    [x (error 'parse-mapping "not a condition: ~a" x)]))

(module+ test
  (require rackunit)
  (check-equal? (parse-mapping "%value.value") (list (cons #f (map-path 'value (list (map-field "value"))))))
  (check-equal? (parse-mapping "FHIRHelpers.ToConcept(%value)") (list (cons #f (map-call "FHIRHelpers.ToConcept" (map-path 'value '())))))
  (check-equal? (parse-mapping "FHIRHelpers.ToConcept(%parent.extension[url='http://x/y'].value)")
                (list (cons #f (map-call "FHIRHelpers.ToConcept"
                                         (map-path 'parent (list (map-field "extension") (map-filter (list (cons '("url") "http://x/y"))) (map-field "value")))))))
  (check-equal? (parse-mapping "%parent.component[code.coding.system='http://loinc.org',code.coding.code='8480-6']")
                (list (cons #f (map-path 'parent (list (map-field "component")
                                                       (map-filter (list (cons '("code" "coding" "system") "http://loinc.org")
                                                                         (cons '("code" "coding" "code") "8480-6"))))))))
  (check-equal? (parse-mapping "System.Concept:FHIRHelpers.ToConcept(%value);System.ValueSet:FHIRHelpers.ToValueSet(%parent.medication[x].extension[url='u'].value.value)")
                (list (cons "System.Concept" (map-call "FHIRHelpers.ToConcept" (map-path 'value '())))
                      (cons "System.ValueSet" (map-call "FHIRHelpers.ToValueSet"
                                                        (map-path 'parent (list (map-field "medication") (map-field "extension")
                                                                                (map-filter (list (cons '("url") "u"))) (map-field "value") (map-field "value")))))))
  (check-exn #rx"not a mapping" (lambda () (parse-mapping "Observation(")))
  ;; every mapping the shipped QI-Core and US Core ModelInfos carry parses
  (require (only-in "model/modelinfo.rkt" find-model model-classes class-info-targets))
  (define all (for*/list ([mv '(("QICore" "4.1.1") ("QICore" "5.0.0") ("QICore" "6.0.0") ("USCore" "3.1.1") ("USCore" "6.1.0"))]
                          [ci (in-hash-values (model-classes (find-model (car mv) (cadr mv))))]
                          [t (in-hash-values (class-info-targets ci))])
                t))
  (check-true (> (length all) 5000))
  (check-equal? (filter (lambda (t) (with-handlers ([exn:fail? (lambda (_) #t)]) (parse-mapping t) #f)) all) '()))
