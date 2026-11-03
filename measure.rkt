#lang racket/base

;;; measure.rkt — Measure evaluation over any FHIR data source, and Measure/$evaluate-measure as an
;;; operation value (fhir/operation).
;;;
;;; Everything comes from `current-data-source` (fhir/data-source), the way a measure processor
;;; holds it — a Bundle in memory, a directory of JSON files, or fhir-server's store:
;;;   - the Measure's Library (Measure.library canonical -> Library.url) and the libraries it includes
;;;     (by CQL library name = Library.name), their CQL in Library.content (text/cql, base64)
;;;   - value sets: the data source's ValueSets (expansion, else enumerated compose), then the
;;;     ValueSets of `current-sd-packages` (read once per package list), then `#:valuesets`
;;;   - each patient's data: the data source's resources of that patient, per type
;;; A MeasureReport is `individual` for one subject, `summary` (population counts over every Patient
;;; in the source, and the proportion score) without one. Proportion scoring, patient-based (cql/run).
;;;
;;;   ($evaluate-measure measure "periodStart" "2021-01-01" "periodEnd" "2021-12-31" ["subject" "Patient/x"])
;;; uses the R4 OperationDefinition (it returns a MeasureReport, as CQF Measures does). The
;;; evaluation timezone is periodStart's offset, else `current-measure-offset` (minutes, default 0).

(require racket/contract racket/list racket/string racket/match net/base64
         fhir/operation fhir/data-source
         (only-in fhir/r4/operations evaluate-measure-definition)
         (only-in fhir/package read-package)
         (only-in fhir/sd/sd-index sd-context)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in "parser/parse.rkt" parse-cql parse-cql-expr)
         (only-in "compile.rkt" compile-expr)
         (only-in "interp.rkt" eval-elm make-ienv)
         (only-in "value.rkt" current-evaluation-offset)
         (only-in "error.rkt" cql-error? cql-error-string)
         "run.rkt")

(provide $evaluate-measure current-measure-offset
         (contract-out
          [evaluate-measure (->* (hash? string? string?)
                                 (#:subject (or/c string? #f) #:valuesets procedure? #:offset (or/c exact-integer? #f))
                                 hash?)]
          [measurement-period (-> string? string? (or/c exact-integer? #f) any/c)]
          [period-offset (-> string? (or/c exact-integer? #f))]))

(define current-measure-offset (make-parameter 0))

;; ---- libraries ------------------------------------------------------------------------------
;; library-cql : Library jsexpr -> string|#f   its text/cql content
(define (library-cql lib)
  (for/or ([c (in-list (hash-ref lib 'content '()))])
    (and (regexp-match? #rx"^text/cql" (hash-ref c 'contentType ""))
         (hash-ref c 'data #f)
         (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 (hash-ref c 'data)))))))
(define (parse-library lib)
  (define src (library-cql lib))
  (and src (let ([r (parse-cql src)])
             (if (ok? r) (ok-value r)
                 (operation-error "processing" "Library ~a: ~a" (hash-ref lib 'url (hash-ref lib 'id ""))
                                  (cql-error-string (err-error r)))))))
(define (libraries) ((data-source-all (current-data-source)) "Library"))
;; find-library : canonical|name -> cql-library|#f   by url (|version ignored), else by name (also the
;; canonical's last segment: CQL loaded locally for a published Measure)
(define (find-library key)
  (define k (car (string-split key "|")))
  (define last-seg (last (string-split k "/")))
  (define libs (libraries))
  (define hit (or (findf (lambda (l) (equal? (hash-ref l 'url #f) k)) libs)
                  (findf (lambda (l) (equal? (hash-ref l 'name #f) k)) libs)
                  (findf (lambda (l) (equal? (hash-ref l 'name #f) last-seg)) libs)))
  (and hit (parse-library hit)))
;; include resolver: the Library named like the CQL library (version preferred)
(define (include-resolver name version)
  (define libs (filter (lambda (l) (equal? (hash-ref l 'name #f) (symbol->string name))) (libraries)))
  (define hit (or (findf (lambda (l) (equal? (hash-ref l 'version #f) version)) libs) (and (pair? libs) (car libs))))
  (and hit (parse-library hit)))

;; ---- terminology ----------------------------------------------------------------------------
;; the packages' ValueSets as a lookup, once per package list
(define package-valuesets (make-hash))
(define (packages-valuesets)
  (define pkgs (sd-context))
  (hash-ref! package-valuesets pkgs
             (lambda ()
               (valuesets->terminology
                (append* (for/list ([p (in-list pkgs)])
                           (define-values (_m rs _e) (read-package p #:select (lambda (f) (regexp-match? #rx"^ValueSet-" f))))
                           (map cdr rs)))))))

;; ---- evaluation -----------------------------------------------------------------------------
;; measurement-period : periodStart periodEnd offset -> Interval<DateTime>   whole days, end inclusive
(define (measurement-period start end offset)
  (define (day s) (substring s 0 (min 10 (string-length s))))
  (parameterize ([current-evaluation-offset offset])
    (eval-elm (ok-value (compile-expr (ok-value (parse-cql-expr
                                                 (format "Interval[@~aT00:00:00.000, @~aT23:59:59.999]" (day start) (day end))))))
              (make-ienv))))

;; period-offset : string -> minutes|#f   "2021-01-01T00:00:00-07:00" -> -420
(define (period-offset s)
  (match (regexp-match #px"T[0-9:.]+(Z|([+-])([0-9]{2}):([0-9]{2}))$" s)
    [(list _ "Z" _ ...) 0]
    [(list _ _ sign h m) (* (if (equal? sign "-") -1 1) (+ (* 60 (string->number h)) (string->number m)))]
    [_ #f]))

;; evaluate-measure : Measure periodStart periodEnd … -> MeasureReport jsexpr
(define (evaluate-measure measure start end #:subject [subject #f]
                          #:valuesets [extra-vs (lambda (u) #f)] #:offset [offset #f])
  (define ds (current-data-source))
  (define lib-url (let ([l (hash-ref measure 'library '())]) (and (pair? l) (car l))))
  (define lib (or (and lib-url (find-library lib-url))
                  (operation-error "not-found" "Measure ~a: its Library ~a is not in the data source" (hash-ref measure 'id "") lib-url)))
  (define source-vs (valuesets->terminology ((data-source-all ds) "ValueSet")))
  (define pkg-vs (packages-valuesets))
  (define (valuesets url) (or (source-vs url) (pkg-vs url) (extra-vs url)))
  (define mp (measurement-period start end offset))
  (define (populations pid)
    (define cache (make-hash))
    (define (data type) (hash-ref! cache type (lambda () ((data-source-of-patient ds) type pid))))
    (define rl (load-runtime-library lib #:resolver include-resolver #:data data
                                     #:valuesets valuesets #:parameters (hash "Measurement Period" mp) #:offset offset))
    (with-handlers ([cql-error? (lambda (e) (operation-error "processing" "Patient/~a: ~a" pid (cql-error-string e)))])
      (measure-populations measure rl)))
  (define pids (if subject
                   (list (regexp-replace #rx"^Patient/" subject ""))
                   (for/list ([p (in-list ((data-source-all ds) "Patient"))]) (hash-ref p 'id))))
  (define counts
    (for/fold ([acc (hash)]) ([pid (in-list pids)])
      (for/fold ([acc acc]) ([(k v) (in-hash (populations pid))]) (hash-update acc k (lambda (n) (+ n v)) 0))))
  (measure-report measure counts start end subject))

(define population-order '("initial-population" "denominator" "denominator-exclusion" "denominator-exception" "numerator"))
(define (measure-report measure counts start end subject)
  (define (c k) (hash-ref counts k 0))
  (define denom (- (c "denominator") (c "denominator-exclusion") (c "denominator-exception")))
  (define group
    (hasheq 'population (for/list ([k (in-list population-order)] #:when (hash-has-key? counts k))
                          (hasheq 'code (hasheq 'coding (list (hasheq 'system "http://terminology.hl7.org/CodeSystem/measure-population"
                                                                      'code k)))
                                  'count (c k)))))
  (define group* (if (and (not subject) (> denom 0))
                     (hash-set group 'measureScore (hasheq 'value (exact->inexact (/ (c "numerator") denom))))
                     group))
  (define base (hasheq 'resourceType "MeasureReport" 'status "complete"
                       'type (if subject "individual" "summary")
                       'measure (or (hash-ref measure 'url #f) (string-append "Measure/" (hash-ref measure 'id "")))
                       'period (hasheq 'start start 'end end)
                       'group (list group*)))
  (if subject (hash-set base 'subject (hasheq 'reference (if (regexp-match? #rx"/" subject) subject (string-append "Patient/" subject)))) base))

;; ---- the operation --------------------------------------------------------------------------
;; the Measure: piped (instance level), or `measure` = url or id from the data source
(define (find-measure key)
  (define k (car (string-split key "|")))
  (define ms ((data-source-all (current-data-source)) "Measure"))
  (or (findf (lambda (m) (equal? (hash-ref m 'id #f) k)) ms)
      (findf (lambda (m) (equal? (hash-ref m 'url #f) k)) ms)))

(define $evaluate-measure
  (make-operation
   evaluate-measure-definition
   (lambda (in subject)
     (define key (input in "measure"))
     (define measure (or subject (and key (find-measure key))
                         (operation-error "not-found" "$evaluate-measure: Measure ~a is not in the data source" (or key "(none given)"))))
     (define start (input in "periodStart"))
     (define end (input in "periodEnd"))
     (evaluate-measure measure start end #:subject (input in "subject")
                       #:offset (or (period-offset start) (current-measure-offset))))))

(register-operation! $evaluate-measure)
