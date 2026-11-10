#lang s-exp syntax/module-reader

cql/base

#:read        cql-read
#:read-syntax cql-read-syntax
#:whole-body-readers? #t

;; The reader for #lang cql. A CQL source file is ONE library: parse the whole body with
;; parse-cql and emit a single (cql-library-form <prefab-ast> <source-text>) form into the cql/base
;; module language, which compiles it to ELM and registers it as a Library (the text is its
;; content). A parse failure raises with the CQL error message.

(require (only-in cql/parser/parse parse-cql)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in cql/error cql-error-string cql-error-diagnostic at-the-name)
         (only-in fhir/diagnostic raise-diagnostic diagnostic->string)
         (only-in cql/compile compile-library/diagnostics current-library-resolver)
         (only-in cql/run directory-resolver)
         (only-in racket/port port->string))

(define (source-name src) (cond [(path? src) (path->string src)] [(string? src) src] [else #f]))
;; a parse error raises exn:fail:diagnostic positioned in this file (fhir/diagnostic)
(define (cql-read-syntax src in)
  (define text (port->string in))
  (define r (parse-cql text #:source (source-name src)))
  (cond
    [(ok? r)
     (translate! (ok-value r) src text)
     (list (datum->syntax #f (list 'cql-library-form (ok-value r) text)))]
    [else (let ([d (cql-error-diagnostic (err-error r))])
            (if d (raise-diagnostic d) (error 'cql (cql-error-string (err-error r)))))]))

;; translate! : library source -> void   every definition translated when the module compiles
;; (`raco make`, `racket`): the first translation error fails the compile at its `define` (as
;; `raco cql` reports it); warnings (the choice-timing trap) are printed and do not. Includes resolve
;; beside the file, then the bundled libraries — as the module itself will.
(define (translate! lib src text)
  (define dir (and (path? src) (let-values ([(d _n _x) (split-path (path->complete-path src))]) (and (path? d) d))))
  (define-values (_elm errors warnings)
    (parameterize ([current-library-resolver (directory-resolver (or dir (current-directory)))])
      (compile-library/diagnostics lib)))
  (for ([w (in-list warnings)]) (eprintf "~a\n" (diagnostic->string w)))
  (when (pair? errors)
    (define d (cql-error-diagnostic (car errors)))
    (if d (raise-diagnostic (at-the-name d text)) (error 'cql (cql-error-string (car errors))))))

(define (cql-read in)
  (map syntax->datum (cql-read-syntax #f in)))
