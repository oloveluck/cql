#lang racket/base

;;; error.rkt — the CQL error payload carried by result/result.rkt's `err`.
;;;
;;; CQL parse/translate errors don't violate a FHIR spec-url (the repo's validation-error hierarchy),
;;; so they get their own small payload: a phase tag + a human message (+ optional source detail).

(require racket/contract)

(provide (struct-out cql-error)
         (contract-out [cql-error-string (-> cql-error? string?)]))

;; phase: 'parse | 'translate | 'interpret
(struct cql-error (phase message) #:transparent)

(define (cql-error-string e)
  (format "CQL ~a error: ~a" (cql-error-phase e) (cql-error-message e)))
