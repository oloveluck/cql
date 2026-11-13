#lang racket/base

;;; qicore-elm-corpus-test.rkt — 98 CQL libraries written against QI-Core 4.1.1 (cqframework's
;;; ecqm-content-qicore-2024: the 2024 eCQMs, their shared libraries, FHIRHelpers 4.4.000), each with
;;; the reference translator's ELM (3.25.0; tests/vendor/qicore-2024/SOURCE.txt). A QI-Core library is
;;; typed by the QI-Core ModelInfo and its ELM reads FHIR: retrieves name the FHIR class with the
;;; profile as templateId, and every element with a `target` mapping is that mapping's ELM
;;; (compile.rkt, "mapping models"). The harness is elm-corpus-harness.rkt's.
;;;
;;; RATCHETED BASELINE — floors only rise. Every library parses and compiles; the ELM-equal count is
;;; agreement with the reference translator, definition by definition, and the classified report
;;; below is the work list.

(require rackunit racket/runtime-path racket/list
         "elm-corpus-harness.rkt")

(define-runtime-path corpus "vendor/qicore-2024/elm-corpus")
(define rs (check-elm-corpus corpus))
(define (sum f) (apply + (map f rs)))
(print-corpus-report rs "QI-Core ELM corpus")
(check-equal? (length rs) 98)
(check-true (>= (count lib-result-parse-ok? rs) 98) "fewer QI-Core libraries parse than the floor")
(check-true (>= (count lib-result-compile-ok? rs) 98) "fewer QI-Core libraries compile than the floor")
(check-true (>= (sum lib-result-defs-equal) 1932) "fewer ELM-equal QI-Core definitions than the floor")
