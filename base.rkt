#lang racket/base

;;; base.rkt — the module language for #lang cql.
;;;
;;; The reader (lang/reader.rkt) parses a CQL source file into one library AST and emits a single
;;; (cql-library-form <prefab-ast>) form here. This custom #%module-begin compiles it to ELM
;;; (compile.rkt) and provides:
;;;   library  the CQL AST          elm  the ELM AST          elm-json  the canonical ELM JSON
;;; Includes resolve to <Name>.cql beside the file, else to a bundled library (FHIRHelpers).
;;;   (evaluate resources #:parameters h #:valuesets vss)   the library over FHIR resources (a
;;;            patient's data, the Patient among them): define-name -> value — how other modules
;;;            (and the other FHIR languages' modules) use a CQL library
;;;   (results)  every define evaluated with no data (for libraries without retrieves)
;;;   resource  the library as a FHIR Library (cql/library), registered with fhir/registry when the
;;;            module is instantiated — the module system as the FHIR registry — so a Measure or an
;;;            `include` finds it by url or name without a data source
;;; Nothing else runs when the module is required. Run the file to print the ELM JSON.

(require (for-syntax racket/base syntax/parse (only-in cql/parser/ast cql-library?))
         (only-in cql/compile compile-library current-library-resolver)
         (only-in cql/elm/json elm->json-string)
         (only-in cql/interp run-library)
         (only-in cql/run evaluate-library directory-resolver)
         (only-in racket/path path-only)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in fhir/registry register-resources!)
         (only-in cql/library library-resource)
         (only-in cql/error cql-error-string))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [cql-module-begin #%module-begin])
         cql-library-form)

;; (cql-library-form ast text) yields the prefab library-AST literal; module-begin picks it up.
(define-syntax-rule (cql-library-form ast text) (quote ast))

;; the module body the reader produces: one (cql-library-form <library>), or none for an empty file
(begin-for-syntax
  (define-syntax-class library-form
    #:description "a (cql-library-form <library> <source-text>) form, as the #lang cql reader writes it"
    (pattern ((~datum cql-library-form) ast text:str)
             #:fail-unless (cql-library? (syntax->datum #'ast)) "not a CQL library AST")))

(define-syntax (cql-module-begin stx)
  (syntax-parse stx
    [(_ (~optional form:library-form))
     #'(#%module-begin
        (define library (~? form #f))                            ; the CQL library AST
        (define resource (~? (library-resource form.text) #f))   ; the Library resource, or #f without a header
        (when resource (register-resources! (list resource) #:source (variable-reference->module-source (#%variable-reference))))
        ;; includes: <Name>.cql beside this file, else a bundled library (FHIRHelpers)
        (define resolver (directory-resolver (let ([src (variable-reference->module-source (#%variable-reference))])
                                               (and (path? src) (path-only src)))))
        (define elm-result (and library (parameterize ([current-library-resolver resolver]) (compile-library library))))
        (define elm (and elm-result (ok? elm-result) (ok-value elm-result))) ; the ELM library AST
        (define elm-json (and elm (elm->json-string elm)))       ; canonical ELM JSON string
        (define (results) (if elm (run-library elm) (hash)))     ; define-name -> value, no data
        (define (evaluate resources #:parameters [ps (hash)] #:valuesets [vss '()])
          (evaluate-library library resources #:parameters ps #:valuesets vss #:resolver resolver))
        (provide library elm elm-json results evaluate resource)
        (module+ main
          (cond
            [elm (displayln elm-json)]
            [elm-result (eprintf "CQL compile error: ~a\n" (cql-error-string (err-error elm-result)))]
            [else (eprintf "empty CQL library\n")])))]))
