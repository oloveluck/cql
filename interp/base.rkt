#lang racket/base

;;; interp/base.rkt — what every part of the ELM interpreter rests on: the interpreter's error, the
;;; numeric tower (Integer / Long / Decimal as the FHIRPath carriers), 32-bit Integer overflow to null,
;;; three-valued negation, and the reading of an interval's bounds.
;;;
;;; Data definitions (the values are value.rkt's CqlValues)
;;;   A Num is an exact-integer (System.Integer, 32-bit) | fp-long (System.Long) | fp-decimal
;;;   An interval bound is a CqlValue | #f, #f meaning unbounded/unknown on that side; a POINT reads
;;;   as the degenerate closed interval [p, p] (hi-pt / lo-pt / hic / loc).

(require (only-in racket/math nan? infinite?)
         (only-in fhirpath/decimal fp-decimal? parse-decimal integer->decimal decimal->exact decimal-round decimal-negate)
         (only-in fhirpath/value fp-long fp-long? fp-long-v)
         (only-in "../error.rkt" cql-error)
         "../value.rkt")

(provide fail num? int-like? ->int ->dec any-long? INT-MIN INT-MAX int-guard wrap-int dec-abs num->real float->dec ceil-int floor-int truncate-int ->int-val rat->dec dec8 round-to-int sgn singular-unit three-not three-xor safe-float->dec iv-low iv-high iv-loc iv-hic hi-pt lo-pt hic loc nn->f)

(define (fail msg) (raise (cql-error 'interpret msg)))
(define (nn->f v) (if (cql-null? v) #f v))
(define (iv-low x) (cql-interval-low x))
(define (iv-high x) (cql-interval-high x))
(define (iv-loc x) (cql-interval-low-closed? x))
(define (iv-hic x) (cql-interval-high-closed? x))

;; treat a point as a degenerate [p,p] closed interval for the relational operators
(define (hi-pt v) (if (cql-interval? v) (iv-high v) v))
(define (lo-pt v) (if (cql-interval? v) (iv-low v) v))
(define (hic v) (if (cql-interval? v) (iv-hic v) #t))
(define (loc v) (if (cql-interval? v) (iv-loc v) #t))
(define (num? x) (or (exact-integer? x) (fp-long? x) (fp-decimal? x)))
(define (int-like? x) (or (exact-integer? x) (fp-long? x)))
(define (->int x) (if (fp-long? x) (fp-long-v x) x))
(define (->dec x) (cond [(fp-decimal? x) x] [(fp-long? x) (integer->decimal (fp-long-v x))] [(exact-integer? x) (integer->decimal x)] [else (fail "not numeric")]))
(define (any-long? a b) (or (fp-long? a) (fp-long? b)))

;; Integer results that overflow the 32-bit range become null (CQL Integer is 32-bit); Longs are unbounded here
(define INT-MIN -2147483648)
(define INT-MAX 2147483647)
(define (int-guard n) (if (and (exact-integer? n) (or (< n INT-MIN) (> n INT-MAX))) cql-null n))
(define (wrap-int n long?) (if long? (fp-long n) (int-guard n)))
(define (dec-abs d) (if (< (decimal->exact d) 0) (decimal-negate d) d))
(define (singular-unit u) (if (and (> (string-length u) 0) (char=? (string-ref u (sub1 (string-length u))) #\s)) (substring u 0 (sub1 (string-length u))) u))
(define (round-to-int d) (inexact->exact (round (if (fp-decimal? d) (decimal->exact d) d))))
(define (num->real x) (cond [(exact-integer? x) x] [(fp-long? x) (fp-long-v x)] [(fp-decimal? x) (decimal->exact x)] [else (fail "numeric expected")]))
(define (float->dec f) (or (parse-decimal (number->string (exact->inexact f))) (integer->decimal 0)))
(define (ceil-int x) (inexact->exact (ceiling (num->real x))))
(define (floor-int x) (inexact->exact (floor (num->real x))))
(define (truncate-int x) (inexact->exact (truncate (num->real x))))

;; a NaN (out-of-domain) is null; an INFINITE result is an overflow error (Exp(1000), Ln(0))
(define (safe-float->dec f)
  (cond [(nan? f) cql-null] [(infinite? f) (fail "arithmetic overflow")] [else (float->dec f)]))
(define (sgn n) (cond [(< n 0) -1] [(> n 0) 1] [else 0]))

;; three-not : CqlValue -> Boolean|Null
(define (three-not x) (cond [(cql-null? x) cql-null] [(boolean? x) (not x)] [else (fail "Not on non-boolean")]))

;; three-xor : (Listof CqlValue) -> Boolean|Null   (null if either operand is null)
(define (three-xor vals)
  (define a (car vals)) (define b (cadr vals))
  (if (or (cql-null? a) (cql-null? b)) cql-null (not (eq? a b))))

;; dec8 : real -> fp-decimal   at CQL's max decimal scale (8 fractional digits)
(define (dec8 r) (decimal-round (rat->dec r) 8))
(define (rat->dec r) (or (parse-decimal (number->string (exact->inexact r))) (integer->decimal 0)))
(define (->int-val x) (cond [(exact-integer? x) x] [(fp-long? x) (fp-long-v x)] [(fp-decimal? x) (truncate-int x)] [else (fail "not integral")]))


;; ---- living examples ----------------------------------------------------------------------------
(module+ test
  (require (except-in rackunit fail) (only-in fhirpath/decimal parse-decimal decimal->string) (only-in "../error.rkt" cql-error? cql-error-message))
  ;; fails? : regexp -> (Any -> Boolean)   a cql-error whose message matches (the interpreter raises a value, not an exn)
  (define ((fails? rx) e) (and (cql-error? e) (regexp-match? rx (cql-error-message e))))
  (check-true (num? 1)) (check-true (num? (fp-long 1))) (check-true (num? (parse-decimal "1.5"))) (check-false (num? "1"))
  (check-equal? (decimal->string (->dec 3)) "3")
  (check-equal? (->int (fp-long 7)) 7)
  (check-equal? (->int-val (parse-decimal "2.9")) 2)              ; toward zero
  (check-exn (fails? #rx"not integral") (lambda () (->int-val "x")))
  ;; Integer is 32-bit: past the range the result is null; a Long is unbounded
  (check-equal? (int-guard 2147483647) 2147483647)
  (check-true (cql-null? (int-guard 2147483648)))
  (check-equal? (wrap-int 2147483648 #t) (fp-long 2147483648))
  (check-equal? (decimal->string (dec-abs (parse-decimal "-1.50"))) "1.50")
  (check-equal? (sgn -3) -1) (check-equal? (sgn 0) 0)
  (check-equal? (singular-unit "days") "day") (check-equal? (singular-unit "day") "day")
  (check-equal? (decimal->string (rat->dec 1/4)) "0.25")
  (check-equal? (decimal->string (dec8 (sqrt 2.0))) "1.41421356")
  ;; a NaN is null, an infinity an overflow error
  (check-true (cql-null? (safe-float->dec +nan.0)))
  (check-exn (fails? #rx"overflow") (lambda () (safe-float->dec +inf.0)))
  (check-exn (fails? #rx"x$") (lambda () (fail "x")))
  ;; three-valued negation and xor
  (check-equal? (three-not #t) #f) (check-true (cql-null? (three-not cql-null)))
  (check-exn (fails? #rx"Not") (lambda () (three-not 1)))
  (check-equal? (three-xor (list #t #f)) #t) (check-true (cql-null? (three-xor (list #t cql-null))))
  ;; a point reads as the closed interval [p, p]
  (define iv (cql-interval 1 5 #t #f))
  (check-equal? (list (lo-pt iv) (hi-pt iv) (loc iv) (hic iv)) '(1 5 #t #f))
  (check-equal? (list (lo-pt 3) (hi-pt 3) (loc 3) (hic 3)) '(3 3 #t #t))
  (check-false (nn->f cql-null)) (check-equal? (nn->f 4) 4))
