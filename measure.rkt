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
;;;   ($evaluate-measure measure #:periodStart "2021-01-01" #:periodEnd "2021-12-31" [#:subject "Patient/x"])
;;; uses the R4 OperationDefinition (it returns a MeasureReport, as CQF Measures does). The
;;; evaluation timezone is periodStart's offset, else `current-measure-offset` (minutes, default 0).

(require racket/contract racket/list racket/string racket/match net/base64
         fhir/operation fhir/data-source
         (only-in fhir/registry registered-resources)
         (only-in fhir/r4/operations evaluate-measure-definition)
         (only-in fhir/package read-package)
         (only-in fhir/sd/sd-index sd-context)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in "parser/parse.rkt" parse-cql parse-cql-expr)
         (only-in "parser/ast.rkt" cql-library-defs cql-parameter? cql-parameter-name cql-parameter-type
                  interval-type named-type)
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
          [measurement-period (->* (string? string? (or/c exact-integer? #f)) (#:date? boolean?) any/c)]
          ;; the report from population counts (one hash per Measure.group), for other evaluators of
          ;; the same measure (cql-to-sql's SQL) to build reports identically
          [measure-report (-> hash? (listof hash?) string? string? (or/c string? #f) hash?)]
          [period-offset (-> string? (or/c exact-integer? #f))]
          ;; packages-valuesets : -> (url -> members|#f)   the ValueSets of the packages in context
          ;; (sd-context), indexed once per package list
          [packages-valuesets (-> procedure?)]))

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
;; libraries : -> (listof Library jsexpr)   the data source's, then the registered ones (#lang cql modules)
(define (libraries) (append ((data-source-all (current-data-source)) "Library") (registered-resources "Library")))
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
;; measurement-period : periodStart periodEnd offset [#:date?] -> Interval<DateTime>|Interval<Date>
;; whole days, end inclusive; Interval<Date> when the library declares the parameter so (HEDIS)
(define (measurement-period start end offset #:date? [date? #f])
  (define (day s) (substring s 0 (min 10 (string-length s))))
  (parameterize ([current-evaluation-offset offset])
    (eval-elm (ok-value (compile-expr (ok-value (parse-cql-expr
                                                 (if date?
                                                     (format "Interval[@~a, @~a]" (day start) (day end))
                                                     (format "Interval[@~aT00:00:00.000, @~aT23:59:59.999]" (day start) (day end)))))))
              (make-ienv))))

;; period-parameter : cql-library -> (values name date?)   the library's measurement period parameter:
;; its declared name, matched without case ("Measurement Period", HEDIS's "Measurement period"), and
;; whether it is an Interval<Date>; "Measurement Period" and DateTime when it declares none
(define (period-parameter lib)
  (define p (for/first ([d (in-list (cql-library-defs lib))]
                        #:when (and (cql-parameter? d)
                                    (string-ci=? (format "~a" (cql-parameter-name d)) "Measurement Period")))
              d))
  (values (if p (format "~a" (cql-parameter-name p)) "Measurement Period")
          (match (and p (cql-parameter-type p))
            [(interval-type (named-type (or 'Date 'System.Date))) #t]
            [_ #f])))

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
  (define-values (mp-name mp-date?) (period-parameter lib))
  (define mp (measurement-period start end offset #:date? mp-date?))
  ;; include-resolver memoised for this evaluation, over which the data source does not change:
  ;; without it every patient re-read every Library from the source, base64-decoded it and re-hashed
  ;; its CQL, for every include
  (define resolver (caching-resolver include-resolver))
  (define (populations pid)
    (define cache (make-hash))
    (define (data type) (hash-ref! cache type (lambda () ((data-source-of-patient ds) type pid))))
    (define rl (load-runtime-library lib #:resolver resolver #:data data
                                     #:valuesets valuesets #:parameters (hash mp-name mp) #:offset offset))
    (with-handlers ([cql-error? (lambda (e) (operation-error "processing" "Patient/~a: ~a" pid (cql-error-string e)))])
      (measure-group-populations measure rl)))
  (define pids (if subject
                   (list (regexp-replace #rx"^Patient/" subject ""))
                   (for/list ([p (in-list ((data-source-all ds) "Patient"))]) (hash-ref p 'id))))
  ;; a subject the data source does not hold is "no such patient", not a patient in no population
  ;; (an individual report of zeros read as "evaluated: not in the initial population")
  (when (and subject (not ((data-source-read ds) "Patient" (car pids))))
    (operation-error "not-found" "$evaluate-measure: the subject Patient/~a is not in the data source" (car pids)))
  (define groups (hash-ref measure 'group '()))
  (define counts                                          ; one count hash per group
    (for/fold ([acc (map (lambda (_g) (hash)) groups)]) ([pid (in-list pids)])
      (for/list ([acc-g (in-list acc)] [pops (in-list (populations pid))])
        (for/fold ([a acc-g]) ([(k v) (in-hash pops)]) (hash-update a k (lambda (n) (+ n v)) 0)))))
  (measure-report measure counts start end subject))

;; measure-report : Measure (listof counts) periodStart periodEnd subject|#f -> MeasureReport jsexpr
;; a report group per Measure.group (its id and code carried over), populations in canonical order,
;; the proportion score on a summary
(define population-order '("initial-population" "denominator" "denominator-exclusion" "denominator-exception" "numerator"))
(define (measure-report measure counts start end subject)
  (define (report-group g counts)
    (define (c k) (hash-ref counts k 0))
    (define denom (- (c "denominator") (c "denominator-exclusion") (c "denominator-exception")))
    (define pops
      (for/list ([k (in-list population-order)] #:when (hash-has-key? counts k))
        (hasheq 'code (hasheq 'coding (list (hasheq 'system "http://terminology.hl7.org/CodeSystem/measure-population"
                                                    'code k)))
                'count (c k))))
    (define base (for/fold ([h (hasheq 'population pops)]) ([k (in-list '(id code))] #:when (hash-has-key? g k))
                   (hash-set h k (hash-ref g k))))
    (if (and (not subject) (> denom 0))
        (hash-set base 'measureScore (hasheq 'value (exact->inexact (/ (c "numerator") denom))))
        base))
  (define base (hasheq 'resourceType "MeasureReport" 'status "complete"
                       'type (if subject "individual" "summary")
                       'measure (or (hash-ref measure 'url #f) (string-append "Measure/" (hash-ref measure 'id "")))
                       'period (hasheq 'start start 'end end)
                       'group (for/list ([g (in-list (hash-ref measure 'group '()))] [c (in-list counts)]) (report-group g c))))
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

;; ---- living examples ------------------------------------------------------------------------
(module+ test
  (require rackunit (only-in "value.rkt" cql-interval-low cql-interval-high) (only-in fhirpath/temporal-ops moment->string))
  ;; the period's offset is its start's, in minutes
  (check-equal? (period-offset "2021-01-01T00:00:00-07:00") -420)
  (check-equal? (period-offset "2021-01-01T00:00:00+05:30") 330)
  (check-equal? (period-offset "2021-01-01T00:00:00Z") 0)
  (check-false (period-offset "2021-01-01"))
  ;; the Measurement Period is whole days, end inclusive, in the evaluation's zone (0 = UTC)
  (define mp (measurement-period "2021-01-01" "2021-12-31" 0))
  (check-equal? (moment->string (cql-interval-low mp)) "@2021-01-01T00:00:00.000Z")
  (check-equal? (moment->string (cql-interval-high mp)) "@2021-12-31T23:59:59.999Z")
  ;; … or an Interval<Date> when the library declares it so
  (define mpd (measurement-period "2025-01-01" "2025-12-31" #f #:date? #t))
  (check-equal? (moment->string (cql-interval-low mpd)) "@2025-01-01")
  (check-equal? (moment->string (cql-interval-high mpd)) "@2025-12-31")
  ;; the period parameter by its declared name, without case, and its type
  (define (pp src) (call-with-values (lambda () (period-parameter (ok-value (parse-cql src)))) list))
  (check-equal? (pp "library H\nparameter \"Measurement period\" Interval<Date>") '("Measurement period" #t))
  (check-equal? (pp "library E\nparameter \"Measurement Period\" Interval<DateTime>") '("Measurement Period" #f))
  (check-equal? (pp "library N\ndefine X: 1") '("Measurement Period" #f))
  ;; a report group per Measure.group, with its id and code, each scored on its own counts
  (define two-groups
    (hasheq 'url "http://example.org/Measure/M"
            'group (list (hasheq 'id "all" 'population '()) (hasheq 'id "medicare" 'code (hasheq 'text "Medicare") 'population '()))))
  (define rep (measure-report two-groups
                              (list (hash "initial-population" 4 "denominator" 4 "denominator-exclusion" 1 "numerator" 2)
                                    (hash "initial-population" 2 "denominator" 2 "numerator" 1))
                              "2025-01-01" "2025-12-31" #f))
  (check-equal? (map (lambda (g) (hash-ref g 'id)) (hash-ref rep 'group)) '("all" "medicare"))
  (check-equal? (hash-ref (cadr (hash-ref rep 'group)) 'code) (hasheq 'text "Medicare"))
  (check-equal? (map (lambda (g) (hash-ref (hash-ref g 'measureScore) 'value)) (hash-ref rep 'group))
                (list (/ 2.0 3) 0.5)))
