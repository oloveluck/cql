#lang racket/base

;;; code-path-test.rkt — a retrieve's CodePath (code-path.rkt) against the generic navigation.
;;;
;;; A code-filtered retrieve reads each resource's codes along a CodePath, resolved once against the
;;; ModelInfo; the generic navigation (interp.rkt's `generic-codes`: navigate-path, then node-codes,
;;; over node-property) is the reference. For every (type, code property) a retrieve can use, and
;;; every resource of that type in the corpora, the CodePath must give the same codes in the same
;;; order, or say 'unshaped (then the retrieve reads that resource generically). The pairs:
;;;   - every retrievable class of the FHIR 4.0.1 ModelInfo, at its primaryCodePath;
;;;   - every code-filtered retrieve in the eCQM and ELM-corpus libraries.
;;; The resources: the eCQM corpus's test data and the examples of the R4 IGs in fhir-spec (US Core
;;; 3 versions, IPS, mCODE, Genomics Reporting, CQF Measures) — and, for each, MUTANTS: its code
;;; element replaced by every departure from the declared shape the reader must handle (null, a
;;; string, a number, a boolean, a list, a value carrying a resourceType, codings that are strings,
;;; nulls and objects with object-valued elements).

(require rackunit racket/list racket/string racket/file racket/runtime-path json
         (only-in fhir/package read-package)
         (only-in fhir/spec-package spec-package-path)
         (only-in cql/parser/parse parse-cql)
         (only-in cql/compile compile-library current-library-resolver)
         (only-in cql/elm/ast elm-retrieve? elm-retrieve-data-type elm-retrieve-code-property elm-retrieve-codes)
         (only-in cql/interp generic-codes)
         (only-in cql/representation record->node)
         (only-in cql/code-path resolve-code-path code-path-codes)
         cql/model/modelinfo
         (only-in cql/run read-json-files)
         (only-in fhir/result/result ok? ok-value)
         "elm-corpus-harness.rkt")

(define-runtime-path ecqm "vendor/ecqm")
(define-runtime-path elm-corpus "vendor/elm-corpus")

;; ---- the resources ----------------------------------------------------------------------------
(define (flatten-bundles rs)
  (append-map (lambda (r) (if (equal? (hash-ref r 'resourceType #f) "Bundle")
                              (flatten-bundles (for/list ([e (in-list (hash-ref r 'entry '()))] #:when (hash? (hash-ref e 'resource #f)))
                                                 (hash-ref e 'resource)))
                              (list r)))
              rs))
(define ig-packages '("hl7.fhir.us.core-6.1.0.tgz" "hl7.fhir.us.core-8.0.0.tgz" "hl7.fhir.us.core.tgz" "hl7.fhir.uv.ips.tgz"
                      "hl7.fhir.us.mcode-4.0.0.tgz" "hl7.fhir.uv.genomics-reporting-2.0.0.tgz" "hl7.fhir.us.cqfmeasures-5.0.0.tgz"))
(define ig-resources
  (append* (for/list ([p (in-list ig-packages)])
             (define tgz (spec-package-path p #:optional? #t))
             (if tgz
                 (let-values ([(_m rs exs) (read-package tgz)]) (map cdr (append rs exs)))
                 '()))))
(define resources
  (flatten-bundles (append (read-json-files (build-path ecqm "tests")) ig-resources)))
(define by-type
  (for/fold ([h (hash)]) ([r (in-list resources)] #:when (string? (hash-ref r 'resourceType #f)))
    (hash-update h (hash-ref r 'resourceType) (lambda (l) (cons r l)) '())))

;; ---- the (type, property) pairs ---------------------------------------------------------------
(define m (find-model "FHIR" "4.0.1"))
(define model-pairs
  (for/list ([(name ci) (in-hash (model-classes m))]
             #:when (class-info-retrievable? ci)
             #:when (string? (class-info-primary-code-path ci)))
    (cons (regexp-replace #rx"^FHIR[.]" name "") (class-info-primary-code-path ci))))

;; retrieves-in : ELM -> (listof (cons type property))   every code-filtered retrieve in a tree
(define (retrieves-in x)
  (cond [(and (elm-retrieve? x) (elm-retrieve-codes x) (string? (elm-retrieve-code-property x)))
         (list (cons (regexp-replace #rx"^{http://hl7.org/fhir}" (elm-retrieve-data-type x) "") (elm-retrieve-code-property x)))]
        [(pair? x) (append (retrieves-in (car x)) (retrieves-in (cdr x)))]
        [(struct? x) (retrieves-in (vector->list (struct->vector x)))]
        [(prefab-struct-key x) (retrieves-in (cdr (vector->list (struct->vector x))))]
        [else '()]))
(define (library-retrieves lib resolver)
  (parameterize ([current-library-resolver resolver])
    (define c (compile-library lib))
    (if (ok? c) (retrieves-in (ok-value c)) '())))
(define (parse-file f) (let ([r (parse-cql (file->string f))]) (and (ok? r) (ok-value r))))
(define ecqm-resolver (lambda (n _v) (let ([g (build-path ecqm "cql" (format "~a.cql" n))]) (and (file-exists? g) (parse-file g)))))
(define library-pairs
  (append
   (append* (for/list ([f (in-list (directory-list (build-path ecqm "cql") #:build? #t))]
                       #:when (regexp-match? #rx"[.]cql$" (path->string f))
                       [lib (in-value (parse-file f))] #:when lib)
              (library-retrieves lib ecqm-resolver)))
   (append* (for/list ([e (in-list (corpus-libraries elm-corpus))])
              (library-retrieves (caddr e) (cadddr e))))))
(check-true (> (length library-pairs) 50) "the libraries' code-filtered retrieves were not found")
(define pairs (remove-duplicates (append model-pairs library-pairs)))

;; ---- mutants: the code element replaced by each departure from its declared shape -------------
(define (departures v)
  (list 'null "x" 7 #t (list v) (list v 'null) (list "x" 'null v)
        (hasheq 'resourceType "Basic")
        (hasheq 'coding "x")
        (hasheq 'coding (list "x" 'null (hasheq 'code 5 'system (list "s")) (hasheq 'code (hasheq 'value "c")) (hasheq 'resourceType "Basic")))
        (hasheq 'system "s" 'code "c" 'display 'null)
        (hasheq 'code (hasheq 'value "c"))))
;; mutants : resource property -> (listof resource)   each top-level key the property's first step can
;; be written under (the element, or a choice member) replaced by each departure
(define (mutants r property)
  (define first-step (car (string-split property ".")))
  (for*/list ([(k v) (in-hash r)]
              #:when (string-prefix? (symbol->string k) first-step)
              [d (in-list (departures v))])
    (hash-set r k d)))

;; ---- the comparison ---------------------------------------------------------------------------
(define resolved 0) (define compared 0) (define unshaped 0)
(define differences '())
(for ([pr (in-list pairs)])
  (define-values (type property) (values (car pr) (cdr pr)))
  (define path (resolve-code-path m (string-append "FHIR." type) property))
  (when path
    (set! resolved (add1 resolved))
    (for* ([r (in-list (hash-ref by-type type '()))]
           [x (in-list (cons r (mutants r property)))])
      (define c (code-path-codes path x))
      (set! compared (add1 compared))
      (cond [(eq? c 'unshaped) (set! unshaped (add1 unshaped))]
            [(not (equal? c (generic-codes (record->node m x) property)))
             (set! differences (cons (list type property x c (generic-codes (record->node m x) property)) differences))]))))

(printf "  code paths: ~a (type, property) pairs, ~a resolved statically; ~a resources and mutants compared, ~a read generically ('unshaped), ~a differ\n"
        (length pairs) resolved compared unshaped (length differences))
(printf "    no CodePath (read generically): ~a\n"
        (for/list ([pr (in-list pairs)] #:unless (resolve-code-path m (string-append "FHIR." (car pr)) (cdr pr)))
          (format "~a.~a" (car pr) (cdr pr))))
(for ([d (in-list (take differences (min 5 (length differences))))])
  (printf "    ~a.~a\n      json: ~s\n      path: ~s\n      generic: ~s\n" (car d) (cadr d) (caddr d) (cadddr d) (list-ref d 4)))
(check-equal? (length differences) 0 "a CodePath read different codes than the generic navigation")
;; not vacuous (floors at the measured counts): the pairs resolve, every resource and mutant is read,
;; and the generic escape is the exception (the mutants carrying a resourceType, mostly)
(check-true (>= (length pairs) 65))
(check-true (>= resolved 64))
(check-true (>= compared 19472))
(check-true (< (* 5 unshaped) compared))
