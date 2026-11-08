#lang racket/base

;;; model-neutral-test.rkt — the engine names no data model.
;;;
;;; The CQL engine reads every data model the same way: a model's classes from its ModelInfo
;;; (model/modelinfo.rkt), how its data looks from its representation (representation.rkt), both data.
;;; So a model designed tomorrow needs a ModelInfo and a representation declaration, and no change
;;; here. This test is what keeps that true: the engine's modules may not name FHIR, its elements or
;;; its helper library in their code (comments and their own living examples aside). FHIR is shipped
;;; as data: model/fhir-4.0.1.rktd and its representation beside it. "Patient" is allowed: it is
;;; CQL's own context name (`context Patient`, `AgeInYears()`), not a model's.

(require rackunit racket/runtime-path racket/list racket/string)

(define-runtime-path root "..")
(define engine
  '("compile.rkt" "interp.rkt" "run.rkt" "representation.rkt" "code-path.rkt" "value.rkt"
    "interp/base.rkt" "interp/temporal.rkt" "interp/arith.rkt" "interp/compare.rkt" "interp/interval.rkt"
    "model/modelinfo.rkt" "model/modelinfo-xml.rkt"))
(define model-names #px"FHIR|hl7\\.org/fhir|resourceType|CodeableConcept|Coding|FHIRHelpers|subject|beneficiary")

;; code-of : text -> (listof (cons line-number code))   each line without its comment, the module's
;; living examples (`(module+ test`) left out; a `;` inside a string is not a comment
(define (code-of text)
  (define body (car (string-split text "(module+ test")))
  (for/list ([line (in-list (string-split body "\n" #:trim? #f))] [n (in-naturals 1)])
    (cons n (strip-comment line))))
(define (strip-comment line)
  (let loop ([cs (string->list line)] [in-string? #f] [escaped? #f] [out '()])
    (cond [(null? cs) (list->string (reverse out))]
          [escaped? (loop (cdr cs) in-string? #f (cons (car cs) out))]
          [(and in-string? (char=? (car cs) #\\)) (loop (cdr cs) #t #t (cons (car cs) out))]
          [(char=? (car cs) #\") (loop (cdr cs) (not in-string?) #f (cons (car cs) out))]
          [(and (not in-string?) (char=? (car cs) #\;)) (list->string (reverse out))]
          [else (loop (cdr cs) in-string? #f (cons (car cs) out))])))

(define mentions
  (for*/list ([f (in-list engine)]
              [l (in-list (code-of (call-with-input-file (build-path root f) (lambda (in) (read-string 10000000 in)))))]
              #:when (regexp-match? model-names (cdr l)))
    (format "~a:~a: ~a" f (car l) (string-trim (cdr l)))))
(for ([m (in-list mentions)]) (printf "  names a model: ~a\n" m))
(check-equal? mentions '() "the engine's code names a data model")

;; the check itself works: it sees a name in code, not in a comment or a string's semicolon
(check-equal? (strip-comment "(define x \"a;b\") ; FHIR here") "(define x \"a;b\") ")
(check-true (regexp-match? model-names (cdr (car (code-of "(f (quote resourceType))\n")))))
