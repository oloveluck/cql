#lang racket/base

;;; error.rkt — the CQL error payload carried by result/result.rkt's `err`.
;;;
;;; CQL parse/translate errors don't violate a FHIR spec-url (the repo's validation-error hierarchy),
;;; so they get their own small payload: a phase tag + a human message (+ optional source detail).

(require racket/contract racket/string racket/list
         (only-in fhir/diagnostic diagnostic diagnostic? diagnostic-lang diagnostic-phase diagnostic-severity
                  diagnostic-message diagnostic-loc loc loc? loc-source loc-position))

(provide (except-out (struct-out cql-error) cql-error)
         (rename-out [make-cql-error* cql-error])
         (contract-out [cql-error-string (-> cql-error? string?)]
                       ;; at-the-name : diagnostic text -> diagnostic   see below
                       [at-the-name (-> diagnostic? string? diagnostic?)]))

;; phase: 'parse | 'translate | 'interpret ; diagnostic: the machine-readable form (fhir/diagnostic),
;; with the source position, when there is one (parse errors), else #f
(struct cql-error (phase message diagnostic) #:transparent)
(define (make-cql-error* phase message [diagnostic #f]) (cql-error phase message diagnostic))

(define (cql-error-string e)
  (if (cql-error-diagnostic e)
      (cql-error-message e)                     ; already "file:line:col: cql parse error: …"
      (format "CQL ~a error: ~a" (cql-error-phase e) (cql-error-message e))))

;; at-the-name : diagnostic library-text -> diagnostic   a translation error about a name (`unknown
;; function 'AgeInYear'`, `unknown identifier 'Foo'`) is positioned at the definition; with the text,
;; it moves to the name itself inside that definition — its line and column — so an editor and a
;; reader land on `AgeInYear`, not on `define`. Anything else, or a name not found, is left as it was.
(define (at-the-name d text)
  (define m (regexp-match #px"unknown (?:function|identifier) '?([^' (]+(?: [^' (]+)*)'?" (diagnostic-message d)))
  (define l (diagnostic-loc d))
  (define start (and m (loc? l) (loc-position l) (sub1 (loc-position l))))
  (cond
    [(not (and start (<= start (string-length text)))) d]
    [else
     (define name (let ([n (cadr m)]) (last (string-split n "."))))
     ;; the definition: from its `define` to the next one, searched after its header's `:`
     (define end (let ([nx (regexp-match-positions #px"\n(?:define|context|parameter|code|valueset|codesystem|concept|include)\\b" text (add1 start))])
                   (if nx (caar nx) (string-length text))))
     (define body (let ([c (regexp-match-positions #rx":" text start end)]) (if c (cdar c) start)))
     (define at (regexp-match-positions (pregexp (string-append "(?<![A-Za-z0-9_])\"?" (regexp-quote name) "(?![A-Za-z0-9_])"))
                                        text body end))
     (cond
       [(not at) d]
       [else
        (define pos (caar at))
        (define before (substring text 0 pos))
        (define line (add1 (length (regexp-match* #rx"\n" before))))
        (define col (add1 (- pos (let ([nl (regexp-match-positions* #rx"\n" before)]) (if (null? nl) 0 (cdr (last nl)))))))
        (diagnostic (diagnostic-lang d) (diagnostic-phase d) (diagnostic-severity d) (diagnostic-message d)
                    (loc (loc-source l) line col (add1 pos) (- (cdar at) pos)))])]))
