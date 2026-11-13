#lang racket/base

;;; qdm-elm-corpus-test.rkt — the QDM libraries of cqm-execution's measure fixtures (QDM 5.0.2 to
;;; 5.6, 42 libraries; tests/vendor/qdm/SOURCE.txt), each with the ELM its measure document carries,
;;; against this translator (elm-corpus-harness.rkt's comparison).
;;;
;;; RATCHETED BASELINE — floors only rise. The reference ELM comes from translators several versions
;;; older than the one the FHIR corpora were made with, so part of the difference is the translator's
;;; age, not this one's reading: `Today()` where newer translators write ToDateTime(Today()),
;;; TimezoneFrom for TimezoneOffsetFrom, and the like (the report below classifies them). One library
;;; (StatinTherapy) has a definition the reference translator could not translate either (a query alias
;;; used inside `sort by`; its ELM holds Null there).

(require rackunit racket/runtime-path racket/list
         "elm-corpus-harness.rkt")

(define-runtime-path corpus "vendor/qdm/measures")
(define rs (check-elm-corpus corpus))
(define (sum f) (apply + (map f rs)))
(print-corpus-report rs "QDM ELM corpus")
(check-equal? (length rs) 42)
(check-true (>= (count lib-result-parse-ok? rs) 42) "fewer QDM libraries parse than the floor")
(check-true (>= (count lib-result-compile-ok? rs) 41) "fewer QDM libraries compile than the floor")
(check-true (>= (sum lib-result-defs-equal) 559) "fewer ELM-equal QDM definitions than the floor")
