#lang racket/base

;;; base.rkt — the module language for #lang fhir/cql.
;;;
;;; The reader (lang/reader.rkt) parses a CQL source file into one library AST and emits a single
;;; (cql-library-form <prefab-ast>) form here. This custom #%module-begin compiles it to ELM
;;; (compile.rkt), runs the ELM (interp.rkt) to get each public define's value, and provides the CQL
;;; AST, the ELM AST, the ELM JSON, and the results. Run the file to print the ELM JSON.

(require (for-syntax racket/base)
         (only-in fhir/cql/compile compile-library)
         (only-in fhir/cql/elm/json elm->json-string)
         (only-in fhir/cql/interp run-library)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in fhir/cql/error cql-error-string))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [cql-module-begin #%module-begin])
         cql-library-form)

;; (cql-library-form ast) yields the prefab library-AST literal; module-begin picks it up.
(define-syntax-rule (cql-library-form ast) (quote ast))

(define-syntax (cql-module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     #'(#%module-begin
        (define forms (list form ...))
        (define library (and (pair? forms) (car forms)))         ; the CQL library AST
        (define elm-result (and library (compile-library library)))
        (define elm (and elm-result (ok? elm-result) (ok-value elm-result))) ; the ELM library AST
        (define elm-json (and elm (elm->json-string elm)))       ; canonical ELM JSON string
        (define results (if elm (run-library elm) (hash)))       ; define-name -> interpreted value
        (provide library elm elm-json results)
        (module+ main
          (cond
            [elm (displayln elm-json)]
            [elm-result (eprintf "CQL compile error: ~a\n" (cql-error-string (err-error elm-result)))]
            [else (eprintf "empty CQL library\n")])))]))
