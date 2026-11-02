#lang racket/base

;;; error.rkt — the CQL error payload carried by result/result.rkt's `err`.
;;;
;;; CQL parse/translate errors don't violate a FHIR spec-url (the repo's validation-error hierarchy),
;;; so they get their own small payload: a phase tag + a human message (+ optional source detail).

(require racket/contract)

(provide (except-out (struct-out cql-error) cql-error)
         (rename-out [make-cql-error* cql-error])
         (contract-out [cql-error-string (-> cql-error? string?)]))

;; phase: 'parse | 'translate | 'interpret ; diagnostic: the machine-readable form (fhir/diagnostic),
;; with the source position, when there is one (parse errors), else #f
(struct cql-error (phase message diagnostic) #:transparent)
(define (make-cql-error* phase message [diagnostic #f]) (cql-error phase message diagnostic))

(define (cql-error-string e)
  (if (cql-error-diagnostic e)
      (cql-error-message e)                     ; already "file:line:col: cql parse error: …"
      (format "CQL ~a error: ~a" (cql-error-phase e) (cql-error-message e))))
