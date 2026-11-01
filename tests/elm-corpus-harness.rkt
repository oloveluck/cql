#lang racket/base

;;; elm-corpus-harness.rkt — real CQL libraries against the reference translator's ELM.
;;;
;;; tests/vendor/elm-corpus/<package>/<Library>.cql + <Library>.elm.json come from published FHIR
;;; packages (see its SOURCE.txt): the CQL an IG ships and the ELM the cqframework cql-to-elm
;;; translator produced from it. For each library this harness records
;;;   parse     — parse-cql succeeds
;;;   compile   — compile-library succeeds
;;;   defs      — for every `define` in the reference ELM: whether ours has a definition of that name,
;;;               and whether its expression is STRUCTURALLY EQUAL once translator-only metadata is
;;;               dropped (localId, locator, annotation, resultTypeName, resultTypeSpecifier,
;;;               signature) — the ELM differential
;;; Nothing here evaluates CQL (that needs retrieves + a data source); this is the translation oracle.
;;;
;;; Data: (lib-result package name parse-ok? compile-ok? error defs-total defs-present defs-equal
;;;                   mismatches)   mismatches : (listof (list def-name first-difference))

(require racket/contract racket/list racket/string racket/file racket/path racket/match json
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library)
         (only-in cql/elm/json elm->jsexpr)
         (only-in cql/error cql-error-string cql-error?)
         (only-in fhir/result/result ok? ok-value err-error))

(provide
 (struct-out lib-result)
 (contract-out
  [check-elm-corpus (-> path-string? (listof lib-result?))]
  [normalize-elm (-> any/c any/c)]))

(struct lib-result (package name parse-ok? compile-ok? error defs-total defs-present defs-equal mismatches)
  #:transparent)

(define dropped-keys '(localId locator annotation resultTypeName resultTypeSpecifier signature))

;; normalize-elm : jsexpr -> jsexpr   translator-only metadata removed, numbers as strings
(define (normalize-elm j)
  (cond [(hash? j) (for/hash ([(k v) (in-hash j)] #:unless (memq k dropped-keys)) (values k (normalize-elm v)))]
        [(list? j) (map normalize-elm j)]
        [(number? j) (number->string j)]
        [else j]))

(define (defs-of lib-js)
  (define stmts (let ([s (hash-ref lib-js 'statements #f)]) (if (hash? s) (hash-ref s 'def '()) '())))
  (for/hash ([d (in-list stmts)] #:when (hash? d)) (values (hash-ref d 'name #f) d)))

(define (first-diff a b path)
  (cond [(and (hash? a) (hash? b))
         (or (for/or ([(k v) (in-hash a)])
               (if (hash-has-key? b k) (first-diff v (hash-ref b k) (cons k path)) (list 'missing (reverse (cons k path)))))
             (for/or ([k (in-hash-keys b)]) (and (not (hash-has-key? a k)) (list 'extra (reverse (cons k path))))))]
        [(and (list? a) (list? b))
         (if (= (length a) (length b))
             (for/or ([x a] [y b] [i (in-naturals)]) (first-diff x y (cons i path)))
             (list 'length (reverse path) (length a) (length b)))]
        [(equal? a b) #f]
        [else (list 'differs (reverse path) a b)]))

(define (error-text e) (if (cql-error? e) (cql-error-string e) (format "~a" e)))

(define (check-library pkg cql-file elm-file)
  (define name (path->string (path-replace-extension (file-name-from-path cql-file) #"")))
  (define ref (hash-ref (call-with-input-file elm-file read-json) 'library))
  (define ref-defs (defs-of ref))
  (define pr (with-handlers ([exn:fail? (lambda (e) (exn-message e))]) (parse-cql (file->string cql-file))))
  (define (fail parse? msg)
    (lib-result pkg name parse? #f msg (hash-count ref-defs) 0 0 '()))
  (cond
    [(string? pr) (fail #f pr)]
    [(not (ok? pr)) (fail #f (error-text (err-error pr)))]
    [else
     (define cr (with-handlers ([exn:fail? (lambda (e) (exn-message e))]) (compile-library (ok-value pr))))
     (cond
       [(string? cr) (fail #t cr)]
       [(not (ok? cr)) (fail #t (error-text (err-error cr)))]
       [else
        (define ours (defs-of (hash-ref (elm->jsexpr (ok-value cr)) 'library)))
        (define-values (present equal mismatches)
          (for/fold ([p 0] [e 0] [ms '()]) ([(n d) (in-hash ref-defs)])
            (define o (hash-ref ours n #f))
            (cond
              [(not o) (values p e ms)]
              [else
               (define rd (normalize-elm (hash-ref d 'expression (hash))))
               (define od (normalize-elm (hash-ref o 'expression (hash))))
               (if (equal? rd od)
                   (values (add1 p) (add1 e) ms)
                   (values (add1 p) e (cons (list n (first-diff rd od '())) ms)))])))
        (lib-result pkg name #t #t #f (hash-count ref-defs) present equal (reverse mismatches))])]))

(define (check-elm-corpus dir)
  (for*/list ([pkg (in-list (sort (map path->string (directory-list dir)) string<?))]
              #:when (directory-exists? (build-path dir pkg))
              [f (in-list (sort (directory-list (build-path dir pkg) #:build? #t) string<? #:key path->string))]
              #:when (regexp-match? #rx"[.]cql$" (path->string f)))
    (check-library pkg f (path-replace-extension f #".elm.json"))))
