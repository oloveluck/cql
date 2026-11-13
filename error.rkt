#lang racket/base

;;; error.rkt — the CQL error payload carried by result/result.rkt's `err`.
;;;
;;; CQL parse/translate errors don't violate a FHIR spec-url (the repo's validation-error hierarchy),
;;; so they get their own small payload: a phase tag + a human message (+ optional source detail).

(require racket/contract racket/string racket/list racket/match
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
;; reader land on `AgeInYear`, not on `define`. A unit that is not UCUM is placed at its quoted
;; literal (`'mEq/L'`), as a name is at the name. Anything else, or a name not found, is left as it was.
(define (at-the-name d text)
  (define l (diagnostic-loc d))
  (define mention (mentioned (diagnostic-message d)))
  (define start (and mention (loc? l) (loc-position l) (sub1 (loc-position l))))
  (define found
    (and start (<= start (string-length text))
         (let-values ([(from to) (definition-body text start)])
           (regexp-match-positions mention text from to))))
  (cond
    [(not found) d]
    [else
     (define pos (caar found))
     (define-values (line column) (line+column text pos))
     (diagnostic (diagnostic-lang d) (diagnostic-phase d) (diagnostic-severity d) (diagnostic-message d)
                 (loc (loc-source l) line column (add1 pos) (- (cdar found) pos)))]))

;; mentioned : message -> regexp|#f   what finds, in the source, the thing a message is about: the
;; quoted unit of "'<u>' is not a UCUM unit", the name (its last segment, quoted or not, as a whole
;; word) of "unknown function|identifier '<name>'"
(define (mentioned message)
  (match message
    [(pregexp #px"'([^']+)' is not a UCUM unit" (list _ unit)) (regexp (regexp-quote (string-append "'" unit "'")))]
    [(pregexp #px"unknown (?:function|identifier) '?([^' (]+(?: [^' (]+)*)'?" (list _ name))
     (pregexp (string-append "(?<![A-Za-z0-9_])\"?" (regexp-quote (last (string-split name "."))) "(?![A-Za-z0-9_])"))]
    [_ #f]))

;; definition-body : text position -> (values from to)   the definition that starts at the (0-based)
;; position: from after its header's `:` to the next definition (or the end of the text)
(define (definition-body text start)
  (define next (regexp-match-positions #px"\n(?:define|context|parameter|code|valueset|codesystem|concept|include)\\b" text (add1 start)))
  (define end (if next (caar next) (string-length text)))
  (define colon (regexp-match-positions #rx":" text start end))
  (values (if colon (cdar colon) start) end))

;; line+column : text position -> (values line column)   1-based, of a 0-based position
(define (line+column text pos)
  (define newlines (regexp-match-positions* #rx"\n" text 0 pos))
  (values (add1 (length newlines))
          (add1 (- pos (if (null? newlines) 0 (cdr (last newlines)))))))

(module+ test
  (require rackunit (only-in fhir/diagnostic loc-line loc-column loc-span))
  (define text "library T\ndefine \"A\": 1\ndefine \"B\":\n  1 + Foo\ndefine \"C\": 5 'mEq/L'\n")
  (define (at message position) (at-the-name (diagnostic 'cql 'compile 'error message (loc "t.cql" #f #f position 6)) text))
  (define (place d) (let ([l (diagnostic-loc d)]) (list (loc-line l) (loc-column l) (loc-position l) (loc-span l))))
  ;; the name, inside the definition the error is positioned at (B starts at position 25)
  (check-equal? (place (at "in define \"B\": unknown identifier 'Foo'" 25)) '(4 7 43 3))
  ;; the quoted unit
  (check-equal? (place (at "in define \"C\": 'mEq/L' is not a UCUM unit" 47)) '(5 15 61 7))
  ;; a message about nothing in the text, or a name the definition does not hold: as it was
  (check-equal? (place (at "in define \"B\": no overload matches" 25)) '(#f #f 25 6))
  (check-equal? (place (at "in define \"A\": unknown identifier 'Foo'" 11)) '(#f #f 11 6))
  (check-equal? (call-with-values (lambda () (line+column "ab\ncd" 4)) list) '(2 2))
  (check-equal? (call-with-values (lambda () (definition-body text 10)) list) '(21 23)))
