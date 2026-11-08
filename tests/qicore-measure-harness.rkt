#lang racket/base

;;; qicore-measure-harness.rkt — QI-Core eCQMs over their test cases, populations against the
;;; MeasureReports MADiE exported with them (tests/vendor/qicore-2024, see its SOURCE.txt).
;;;
;;; The libraries are written against QI-Core 4.1.1 and compile to ELM over FHIR (compile.rkt's
;;; mapping models); the data are FHIR JSON. Each case directory is one patient's record (MADiE's
;;; export: the patient, its resources, its expected individual MeasureReport); README.txt names the
;;; cases. Expected populations are the MeasureReport's group.population counts; the Measurement
;;; Period is the report's period (whole days, end inclusive), evaluated at UTC — MADiE's test data
;;; and periods are written in UTC. A case's resources are the patient's as authored: some reference
;;; `Patient/Patient-1`, MADiE's id before export, and MADiE evaluates every resource of a case for
;;; its patient, so the strict patient-context filter is off here (as in measure-harness.rkt).
;;;
;;; Data: (qicore-case measure case name expected actual error)   expected/actual : hash code -> count

(require racket/contract racket/list racket/string racket/file racket/match json
         (only-in cql/parser/parse parse-cql parse-cql-expr)
         (only-in cql/compile compile-expr)
         (only-in cql/interp eval-elm make-ienv)
         (only-in cql/error cql-error? cql-error-string)
         (only-in cql/value current-evaluation-offset)
         cql/run
         (only-in fhir/result/result ok? ok-value))

(provide (struct-out qicore-case)
         (contract-out [run-qicore-measures (-> path-string? (listof qicore-case?))]))

(struct qicore-case (measure case name expected actual error) #:transparent)

;; run-qicore-measures : root -> (listof qicore-case)   every case of every measure in root/measure
(define (run-qicore-measures root)
  (define libdir (build-path root "elm-corpus" "ecqm-content-qicore-2024"))
  (define res (caching-resolver
               (lambda (n _v)
                 (define f (build-path libdir (format "~a.cql" n)))
                 (and (file-exists? f) (let ([r (parse-cql (file->string f))]) (and (ok? r) (ok-value r)))))))
  (define tx (valuesets->terminology (read-json-files (build-path root "valueset"))))
  (for*/list ([mfile (in-list (sort (map path->string (directory-list (build-path root "measure"))) string<?))]
              [m (in-value (path->string (path-replace-extension mfile #"")))]
              [measure (in-value (call-with-input-file (build-path root "measure" mfile) read-json))]
              [names (in-value (case-names (build-path root "tests" m "README.txt")))]
              [c (in-list (sort (map path->string (directory-list (build-path root "tests" m))) string<?))]
              #:when (directory-exists? (build-path root "tests" m c)))
    (define dir (build-path root "tests" m c))
    (define mr (for/first ([f (in-directory dir)] #:when (regexp-match? #rx"MeasureReport" (path->string f)))
                 (call-with-input-file f read-json)))
    (define expected
      (for*/hash ([g (in-list (hash-ref mr 'group '()))] [p (in-list (hash-ref g 'population '()))])
        (values (hash-ref (car (hash-ref (hash-ref p 'code) 'coding)) 'code) (hash-ref p 'count 0))))
    (define per (hash-ref mr 'period))
    (define actual-or-error
      (with-handlers ([cql-error? cql-error-string] [exn:fail? exn-message])
        (measure-populations
         measure
         (load-runtime-library (res (string->symbol m) #f) #:resolver res
                               #:data (resources->data (read-json-files dir) #:patient-context? #f)
                               #:valuesets tx #:offset 0
                               #:parameters (hash "Measurement Period" (period (hash-ref per 'start) (hash-ref per 'end)))))))
    (if (string? actual-or-error)
        (qicore-case m c (hash-ref names c c) expected #f actual-or-error)
        (qicore-case m c (hash-ref names c c) expected actual-or-error #f))))

;; period : date date -> Interval<DateTime>   [start 00:00:00.000, end 23:59:59.999] at UTC
(define (period s e)
  (parameterize ([current-evaluation-offset 0])
    (eval-elm (ok-value (compile-expr (ok-value (parse-cql-expr (format "Interval[@~aT00:00:00.000Z, @~aT23:59:59.999Z]" s e)))))
              (make-ienv))))

;; case-names : README path -> (hash case-id -> name)   "uuid = DENEXPass FrailtyDxOverlapsMP"
(define (case-names f)
  (if (file-exists? f)
      (for*/hash ([l (in-list (file->lines f))] [m (in-value (regexp-match #px"^(\\S+) = (.*)$" (string-trim l)))] #:when m)
        (values (cadr m) (caddr m)))
      (hash)))
