#lang racket/base

;;; elm-corpus-test.rkt — 32 real CQL libraries from published FHIR packages (FHIRHelpers/FHIRCommon,
;;; the CQF Measures eCQM examples, the CQL IG's examples), each with the reference translator's ELM
;;; (see elm-corpus-harness.rkt and tests/vendor/elm-corpus/SOURCE.txt).
;;;
;;; RATCHETED BASELINE — floors only rise. Today almost nothing parses: the corpus is real-world CQL
;;; and needs exactly the deferred slice (code/concept definitions, retrieves `[Encounter: "vs"]`,
;;; `sort by`, FHIR model types, library includes). It exists so that slice is measured from day one.

(require rackunit racket/runtime-path racket/list
         "elm-corpus-harness.rkt")

(define-runtime-path corpus "vendor/elm-corpus")
(define rs (check-elm-corpus corpus))
(define (sum f) (apply + (map f rs)))
(printf "  ELM corpus: ~a libraries; ~a parse, ~a compile; ~a/~a reference definitions present, ~a ELM-equal\n"
        (length rs) (count lib-result-parse-ok? rs) (count lib-result-compile-ok? rs)
        (sum lib-result-defs-present) (sum lib-result-defs-total) (sum lib-result-defs-equal))
(check-equal? (length rs) 32)
(check-true (>= (count lib-result-parse-ok? rs) 2) "fewer corpus libraries parse than the floor")
(check-true (>= (count lib-result-compile-ok? rs) 2) "fewer corpus libraries compile than the floor")
(check-true (>= (sum lib-result-defs-equal) 0))
