#lang racket/base

;;; base.rkt — the module language for #lang cql.
;;;
;;; The reader (lang/reader.rkt) parses a CQL source file into one library AST and emits a single
;;; (cql-library-form <prefab-ast>) form here. This custom #%module-begin compiles it to ELM
;;; (compile.rkt) and provides:
;;;   library  the CQL AST          elm  the ELM AST          elm-json  the canonical ELM JSON
;;;   (evaluate resources #:parameters h #:valuesets vss)   the library over FHIR resources (a
;;;            patient's data, the Patient among them): define-name -> value — how other modules
;;;            (and the other FHIR languages' modules) use a CQL library
;;;   (results)  every define evaluated with no data (for libraries without retrieves)
;;; Nothing runs when the module is required. Run the file to print the ELM JSON.

(require (for-syntax racket/base)
         (only-in cql/compile compile-library)
         (only-in cql/elm/json elm->json-string)
         (only-in cql/interp run-library)
         (only-in cql/run evaluate-library)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in cql/error cql-error-string))

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
        (define (results) (if elm (run-library elm) (hash)))     ; define-name -> value, no data
        (define (evaluate resources #:parameters [ps (hash)] #:valuesets [vss '()])
          (evaluate-library library resources #:parameters ps #:valuesets vss))
        (provide library elm elm-json results evaluate)
        (module+ main
          (cond
            [elm (displayln elm-json)]
            [elm-result (eprintf "CQL compile error: ~a\n" (cql-error-string (err-error elm-result)))]
            [else (eprintf "empty CQL library\n")])))]))
