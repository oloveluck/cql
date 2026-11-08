#lang racket/base

;;; qdm-measure-harness.rkt — QDM measures in the cqm-models format over their test patients
;;; (tests/vendor/qdm, cqm-execution's fixtures; see its SOURCE.txt).
;;;
;;; Each measure directory holds its libraries (CQL, compiled here from source; the reference ELM sits
;;; beside them for the ELM corpus), its measure document and its value sets (cqm-models' format,
;;; read by run.rkt's valuesets->terminology). Each patient is a cqm-models QDM patient document: its
;;; records are cqm.rkt's cqm-patient-records, read through model/qdm.representation.rktd. The
;;; Measurement Period is the measure document's, evaluated at UTC (cqm-execution's default offset).
;;; One patient per document, so no patient-context filter. Populations: cqm.rkt's
;;; cqm-measure-populations, per population set then stratum (cqm-execution's population_index).
;;;
;;; Data: (qdm-case measure patient results error stored)
;;;   results : (listof PopulationResult) | #f ; stored : the document's expectedValues for this measure

(require racket/contract racket/list racket/file racket/match json
         (only-in cql/parser/parse parse-cql parse-cql-expr)
         (only-in cql/compile compile-expr)
         (only-in cql/interp eval-elm make-ienv)
         (only-in cql/error cql-error? cql-error-string)
         (only-in cql/value current-evaluation-offset)
         cql/run cql/cqm
         (only-in fhir/result/result ok? ok-value))

(provide (struct-out qdm-case)
         (contract-out [run-qdm-measures (-> path-string? (listof qdm-case?))]))

(struct qdm-case (measure patient results error stored) #:transparent)

(define (run-qdm-measures root)
  (for*/list ([mdir (in-list (sort (map path->string (directory-list (build-path root "measures"))) string<?))]
              [pdir (in-value (build-path root "patients" mdir))]
              #:when (directory-exists? pdir)
              [pf (in-list (sort (map path->string (directory-list pdir)) string<?))])
    (define m (call-with-input-file (build-path root "measures" mdir "measure.json") read-json))
    (define p (call-with-input-file (build-path pdir pf) read-json))
    (define (lib n)
      (define f (build-path root "measures" mdir (format "~a.cql" n)))
      (and (file-exists? f) (let ([r (parse-cql (file->string f))]) (and (ok? r) (ok-value r)))))
    (define res (caching-resolver (lambda (n _v) (lib n))))
    (define-values (s e) (cqm-measure-period m))
    (define stored (for/list ([x (in-list (hash-ref p 'expectedValues '()))]
                              #:when (equal? (string-upcase (hash-ref x 'measure_id "")) (string-upcase (hash-ref m 'hqmf_set_id ""))))
                     x))
    (define r
      (with-handlers ([cql-error? cql-error-string] [exn:fail? exn-message])
        (cqm-measure-populations
         m
         (load-runtime-library (lib (string->symbol (hash-ref m 'main_cql_library))) #:resolver res
                               #:data (resources->data (cqm-patient-records p) #:patient-context? #f)
                               #:valuesets (valuesets->terminology (list (call-with-input-file (build-path root "measures" mdir "value_sets.json") read-json)))
                               #:offset 0
                               #:parameters (hash "Measurement Period" (period s e))))))
    (if (string? r)
        (qdm-case mdir pf #f r stored)
        (qdm-case mdir pf r #f stored))))

;; period : DateTime DateTime -> Interval<DateTime>   at UTC
(define (period s e)
  (parameterize ([current-evaluation-offset 0])
    (eval-elm (ok-value (compile-expr (ok-value (parse-cql-expr (format "Interval[@~aZ, @~aZ]" s e))))) (make-ienv))))
