#lang racket/base

;;; elm-corpus-test.rkt — 32 real CQL libraries from published FHIR packages (FHIRHelpers/FHIRCommon,
;;; the CQF Measures eCQM examples, the CQL IG's examples), each with the reference translator's ELM
;;; (see elm-corpus-harness.rkt and tests/vendor/elm-corpus/SOURCE.txt).
;;;
;;; RATCHETED BASELINE — floors only rise. Every library parses and compiles (includes resolve to the
;;; libraries in the same package; FHIR types come from the vendored ModelInfo); the ELM-equal count
;;; measures agreement with the reference translator, definition by definition.

(require rackunit racket/runtime-path racket/list racket/string
         "elm-corpus-harness.rkt")

(define-runtime-path corpus "vendor/elm-corpus")
(define rs (check-elm-corpus corpus))
(define (sum f) (apply + (map f rs)))
(printf "  ELM corpus: ~a libraries; ~a parse, ~a compile; ~a/~a reference definitions present, ~a ELM-equal\n"
        (length rs) (count lib-result-parse-ok? rs) (count lib-result-compile-ok? rs)
        (sum lib-result-defs-present) (sum lib-result-defs-total) (sum lib-result-defs-equal))
;; the work list: every mismatch classified by its first difference (kind + path, list indices
;; dropped), largest class first, with examples (library/definition: reference | ours)
(define (class-of d)
  (cons (car d) (string-join (for/list ([seg (in-list (cadr d))] #:unless (number? seg)) (format "~a" seg)) ".")))
(define (short v) (let ([s (format "~s" v)]) (if (> (string-length s) 70) (string-append (substring s 0 70) "…") s)))
(define classes
  (sort (for*/fold ([h (hash)] #:result (hash->list h))
                   ([r (in-list rs)] [m (in-list (lib-result-mismatches r))])
          (hash-update h (class-of (cadr m)) (lambda (l) (cons (list (lib-result-name r) (car m) (cadr m)) l)) '()))
        > #:key (lambda (c) (length (cdr c)))))
(printf "  difference classes (first difference, reference vs ours), largest first:\n")
(for ([c (in-list classes)])
  (printf "    ~a  ~a ~a\n" (length (cdr c)) (car (car c)) (cdr (car c)))
  (for ([ex (in-list (take (reverse (cdr c)) (min 3 (length (cdr c)))))])
    (define d (caddr ex))
    (printf "        ~a/~a: ~a\n" (car ex) (cadr ex)
            (if (eq? (car d) 'differs) (format "~a  |  ~a" (short (caddr d)) (short (cadddr d))) (string-join (map short (cddr d)) " ")))))
(check-equal? (length rs) 32)
(check-true (>= (count lib-result-parse-ok? rs) 32) "fewer corpus libraries parse than the floor")
(check-true (>= (count lib-result-compile-ok? rs) 32) "fewer corpus libraries compile than the floor")
(check-true (>= (sum lib-result-defs-equal) 742) "fewer ELM-equal definitions than the floor")
