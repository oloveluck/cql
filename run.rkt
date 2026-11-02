#lang racket/base

;;; run.rkt — run CQL over FHIR data: libraries with their includes, a data source, terminology,
;;; and measure populations.
;;;
;;;   (define rl (load-runtime-library ast #:resolver r #:data (resources->data rs)
;;;                                    #:valuesets (valuesets->terminology vss)))
;;;   (rlib-value rl "Initial Population")            ; one definition
;;;   (measure-populations measure-json rl)           ; proportion scoring for one patient
;;;
;;; Data: a Resources is (listof jsexpr), all for one context patient (the retrieve source returns a
;;; type's resources from it). Terminology: value set url -> its expansion (system, code) pairs.
;;; Parameters (e.g. "Measurement Period") apply to every library in the evaluation, the way a
;;; measure evaluator passes them; a library's own default is used otherwise.

(require racket/contract racket/match racket/list racket/string racket/file json
         "parser/ast.rkt"
         (only-in "parser/parse.rkt" parse-cql)
         (only-in "compile.rkt" compile-library current-library-resolver)
         (only-in "interp.rkt" make-rlib rlib-value runtime rlib? eval-elm make-ienv)
         (only-in "elm/ast.rkt" elm-library-parameters elm-parameter-def-name elm-parameter-def-default)
         (only-in "value.rkt" current-evaluation-offset)
         (only-in "error.rkt" cql-error? cql-error-string)
         (only-in fhir/result/result ok? ok-value err-error))

(provide
 (contract-out
  [load-runtime-library (->* (cql-library?)
                             (#:resolver procedure? #:data procedure? #:valuesets procedure? #:parameters hash?
                              #:offset (or/c exact-integer? #f))
                             rlib?)]
  [resources->data      (->* ((listof hash?)) (#:patient-context? boolean?) procedure?)]
  [valuesets->terminology (-> (listof hash?) procedure?)]
  [measure-populations  (-> hash? rlib? (hash/c string? exact-nonnegative-integer?))]
  [read-json-files      (-> path-string? (listof hash?))])
 rlib-value)

;; load-runtime-library : cql-library … -> rlib   compiles the library and (recursively, once each)
;; the libraries it includes, binding include aliases to their runtime libraries
(define (load-runtime-library lib #:resolver [resolver (lambda (n v) #f)] #:data [data (lambda (t) '())]
                              #:valuesets [valuesets (lambda (u) #f)] #:parameters [params (hash)]
                              #:offset [offset #f])
  ;; offset: the evaluation request's timezone offset in minutes (DateTimes written without one take it)
  (define rt (runtime data valuesets offset))
  (define memo (make-hash))
  (define (compile1 ast)
    (parameterize ([current-library-resolver resolver])
      (let ([r (compile-library ast)])
        (if (ok? r) (ok-value r)
            (error 'cql-run "~a: ~a" (cql-library-id ast)
                   (let ([e (err-error r)]) (if (cql-error? e) (cql-error-string e) e)))))))
  ;; the main library's parameter defaults apply to the libraries it includes, the way a measure
  ;; evaluator passes the evaluation's parameters to every library (an include often declares
  ;; "Measurement Period" without a default)
  (define main-defaults
    (parameterize ([current-evaluation-offset offset])
      (for/hash ([p (in-list (elm-library-parameters (compile1 lib)))]
                 #:when (elm-parameter-def-default p)
                 #:unless (hash-has-key? params (elm-parameter-def-name p)))
        (values (elm-parameter-def-name p) (eval-elm (elm-parameter-def-default p) (make-ienv))))))
  (define all-params (for/fold ([h params]) ([(k v) (in-hash main-defaults)]) (hash-set h k v)))
  (define (build ast)
    (define key (cons (cql-library-id ast) (cql-library-version ast)))
    (hash-ref! memo key
               (lambda ()
                 (define elm (compile1 ast))
                 (define includes
                   (for/hash ([d (in-list (cql-library-defs ast))] #:when (cql-include? d))
                     (define src (or (resolver (cql-include-lib d) (cql-include-version d))
                                     (error 'cql-run "library ~a not found" (cql-include-lib d))))
                     (values (symbol->string (or (cql-include-alias d) (cql-include-lib d))) (build src))))
                 (make-rlib elm includes rt all-params))))
  (build lib))

;; resources->data : (listof jsexpr) -> (type-name -> (listof jsexpr))   Bundles are unwrapped
(define (resources->data rs #:patient-context? [ctx? #t])
  (define flat (append-map (lambda (r) (if (equal? (hash-ref r 'resourceType #f) "Bundle")
                                           (for/list ([e (in-list (hash-ref r 'entry '()))] #:when (hash? (hash-ref e 'resource #f)))
                                             (hash-ref e 'resource))
                                           (list r)))
                           rs))
  ;; Patient context: with one Patient in the data, a resource that names a different patient (its
  ;; subject / patient / beneficiary reference) is not that patient's, so a retrieve never sees it
  (define patients (filter (lambda (r) (equal? (hash-ref r 'resourceType #f) "Patient")) flat))
  (define pid (and (= 1 (length patients)) (hash-ref (car patients) 'id #f)))
  (define (mine? r)
    (define ref (for/or ([k (in-list '(subject patient beneficiary))])
                  (let ([v (hash-ref r k #f)]) (and (hash? v) (hash-ref v 'reference #f)))))
    (or (not ctx?) (not pid) (not (string? ref)) (equal? ref (string-append "Patient/" pid)) (equal? ref pid)
        (regexp-match? (pregexp (string-append "/Patient/" (regexp-quote pid) "$")) ref)))
  (define by-type (for/fold ([h (hash)]) ([r (in-list (filter mine? flat))])
                    (hash-update h (hash-ref r 'resourceType "") (lambda (l) (append l (list r))) '())))
  (lambda (type) (hash-ref by-type type '())))

;; valuesets->terminology : (listof ValueSet jsexpr) -> (url -> (hash (cons system code) -> #t) | #f)
;; from each value set's expansion (else its enumerated compose concepts)
(define (valuesets->terminology vss)
  (define idx
    (for/hash ([v (in-list vss)] #:when (hash-ref v 'url #f))
      (define from-expansion
        (for/list ([c (in-list (hash-ref (hash-ref v 'expansion (hash)) 'contains '()))])
          (cons (hash-ref c 'system #f) (hash-ref c 'code #f))))
      (define from-compose
        (for*/list ([inc (in-list (hash-ref (hash-ref v 'compose (hash)) 'include '()))]
                    [c (in-list (hash-ref inc 'concept '()))])
          (cons (hash-ref inc 'system #f) (hash-ref c 'code #f))))
      (values (hash-ref v 'url) (for/hash ([p (in-list (if (pair? from-expansion) from-expansion from-compose))]) (values p #t)))))
  (lambda (url) (hash-ref idx (car (string-split url "|")) #f)))

;; read-json-files : dir -> (listof jsexpr)   every *.json under dir (MeasureReports excluded)
(define (read-json-files dir)
  (for/list ([f (in-directory dir)]
             #:when (regexp-match? #rx"[.]json$" (path->string f))
             #:unless (regexp-match? #rx"(?i:measurereport)" (path->string f))
             [j (in-value (call-with-input-file f read-json))]
             #:when (hash? j))
    j))

;; measure-populations : Measure jsexpr rlib -> (hash code -> 0|1)   one patient, proportion scoring
;; (patient-based): IP; denominator ⊆ IP; exclusion ⊆ denominator; numerator ⊆ denominator minus
;; exclusions; exception ⊆ denominator minus exclusions and numerator
(define (measure-populations measure rl)
  (define crit
    (for*/hash ([g (in-list (hash-ref measure 'group '()))]
                [p (in-list (hash-ref g 'population '()))])
      (values (hash-ref (car (hash-ref (hash-ref p 'code) 'coding)) 'code)
              (hash-ref (hash-ref p 'criteria) 'expression))))
  (define (holds? code) (let ([e (hash-ref crit code #f)]) (and e (truthy? (rlib-value rl e)))))
  (define ip (holds? "initial-population"))
  (define den (and ip (if (hash-has-key? crit "denominator") (holds? "denominator") #t)))
  (define excl (and den (holds? "denominator-exclusion")))
  (define num (and den (not excl) (holds? "numerator")))
  (define exc (and den (not excl) (not num) (holds? "denominator-exception")))
  (for/hash ([(code _e) (in-hash crit)])
    (values code (if (case code
                       [("initial-population") ip] [("denominator") den] [("denominator-exclusion") excl]
                       [("numerator") num] [("denominator-exception") exc] [else #f])
                     1 0))))
;; a population definition is boolean (patient-based) or a list (non-empty = in)
(define (truthy? v) (or (eq? v #t) (and (pair? v) #t)))
