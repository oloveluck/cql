#lang racket/base

;;; interp/arith.rkt — CQL arithmetic: Add/Subtract/Multiply/Divide and the truncated forms over the
;;; numeric tower and Quantities (units kept, multiplied or divided per UCUM), Integer results
;;; overflowing to null, uncertainty Intervals combined by their corners, and date arithmetic when
;;; the left operand is a moment (temporal.rkt's moment-duration).

(require (only-in racket/list last)
         (only-in fhirpath/decimal parse-decimal decimal->string
                  decimal-add decimal-sub decimal-mul decimal-div decimal-round)
         (only-in fhirpath/value fp-long fp-quantity fp-quantity? fp-quantity-value fp-quantity-unit)
         (only-in fhirpath/quantity quantity-add quantity-sub)
         (only-in fhirpath/temporal-ops moment?)
         "../value.rkt" "base.rkt" "temporal.rkt")

(provide arith num-interval? interval-arith qty-arith add-values sub-values mul-values unit-product divide power trunc-div modulo-op round-op bin)

;; arithmetic dispatch shared by Add/Subtract/Multiply — including uncertainty ranges (numeric
;; Intervals from partial-precision durations): [a,b]+[c,d]=[a+c,b+d]; -: [a-d,b-c]; *: extremes.
(define (arith int-op dec-op a b)
  (cond
    [(or (cql-null? a) (cql-null? b)) cql-null]
    [(or (num-interval? a) (num-interval? b)) (interval-arith int-op dec-op a b)]
    [(and (fp-quantity? a) (fp-quantity? b)) (or (qty-arith int-op a b) cql-null)]
    [(and (int-like? a) (int-like? b)) (wrap-int (int-op (->int a) (->int b)) (any-long? a b))]
    [(and (num? a) (num? b)) (dec-op (->dec a) (->dec b))]
    [else (fail "arithmetic on non-numeric")]))
(define (num-interval? v) (and (cql-interval? v) (num? (iv-low v)) (num? (iv-high v))))
(define (interval-arith int-op dec-op a b)
  (define-values (al ah) (if (num-interval? a) (values (iv-low a) (iv-high a)) (values a a)))
  (define-values (bl bh) (if (num-interval? b) (values (iv-low b) (iv-high b)) (values b b)))
  (define (op2 x y) (arith int-op dec-op x y))
  (define corners (list (op2 al bl) (op2 al bh) (op2 ah bl) (op2 ah bh)))
  (define sorted (sort corners (lambda (x y) (< (num->real x) (num->real y)))))
  (cql-interval (car sorted) (last sorted) #t #t))
(define (qty-arith int-op a b)
  (cond [(eq? int-op +) (quantity-add a b)] [(eq? int-op -) (quantity-sub a b)] [else #f]))

;; add-values / sub-values : CqlValue CqlValue -> CqlValue   numeric+quantity arithmetic, and temporal
;; duration arithmetic (moment ± Quantity of a calendar/time unit) via moment-plus.
(define (add-values a b) (if (moment? a) (moment-duration a b 1) (arith + decimal-add a b)))
(define (sub-values a b) (if (moment? a) (moment-duration a b -1) (arith - decimal-sub a b)))
(define (bin vals f)
  (define a (car vals)) (define b (cadr vals))
  (if (or (cql-null? a) (cql-null? b)) cql-null (f a b)))

;; div : truncated quotient ; mod : remainder. Integer/Decimal, and same-unit Quantities (div of two
;; same-unit quantities is a plain number; mod keeps the unit).
(define (trunc-div a b)
  (cond
    [(and (fp-quantity? a) (fp-quantity? b))
     ;; same-unit quantity div keeps the unit (per the reference tests: 10.0 'g' div 5.0 'g' = 2.0 'g')
     (if (string=? (fp-quantity-unit a) (fp-quantity-unit b))
         (let ([r (trunc-div (fp-quantity-value a) (fp-quantity-value b))])
           (if (cql-null? r) cql-null (fp-quantity (->dec r) (fp-quantity-unit a) #f)))
         cql-null)]
    [else (define bb (num->real b))
          (cond [(zero? bb) cql-null]
                [(and (int-like? a) (int-like? b)) (int-guard (quotient (->int a) (->int b)))]
                [else (rat->dec (truncate (/ (num->real a) bb)))])]))
(define (modulo-op a b)
  (cond
    [(and (fp-quantity? a) (fp-quantity? b))
     (if (string=? (fp-quantity-unit a) (fp-quantity-unit b))
         (let ([r (modulo-op (fp-quantity-value a) (fp-quantity-value b))]) (if (cql-null? r) cql-null (fp-quantity (->dec r) (fp-quantity-unit a) #f)))
         cql-null)]
    [else (define bb (num->real b))
          (cond [(zero? bb) cql-null]
                [(and (int-like? a) (int-like? b)) (remainder (->int a) (->int b))]
                [else (rat->dec (- (num->real a) (* (truncate (/ (num->real a) bb)) bb)))])]))

;; mul-values : CqlValue CqlValue -> CqlValue   numeric multiply; Quantity×scalar keeps the unit;
;; Quantity×Quantity multiplies values and units (same unit u -> u2, else u1.u2 per UCUM product)
(define (mul-values a b)
  (cond [(and (fp-quantity? a) (fp-quantity? b))
         (fp-quantity (decimal-mul (->dec (fp-quantity-value a)) (->dec (fp-quantity-value b)))
                      (unit-product (fp-quantity-unit a) (fp-quantity-unit b)) #f)]
        [(and (fp-quantity? a) (num? b)) (fp-quantity (decimal-mul (->dec (fp-quantity-value a)) (->dec b)) (fp-quantity-unit a) #f)]
        [(and (num? a) (fp-quantity? b)) (fp-quantity (decimal-mul (->dec a) (->dec (fp-quantity-value b))) (fp-quantity-unit b) #f)]
        [else (arith * decimal-mul a b)]))
(define (unit-product u1 u2)
  (cond [(string=? u1 "1") u2] [(string=? u2 "1") u1]
        [(string=? u1 u2) (string-append u1 "2")]
        [else (string-append u1 "." u2)]))

;; divide : CqlValue CqlValue -> CqlValue   same-unit Quantity/Quantity -> Quantity of unit '1'
(define (divide a b)
  (cond
    [(and (fp-quantity? a) (fp-quantity? b))
     (define q (decimal-div (->dec (fp-quantity-value a)) (->dec (fp-quantity-value b))))
     (cond [(not q) cql-null]
           [(string=? (fp-quantity-unit a) (fp-quantity-unit b)) (fp-quantity q "1" #f)]
           [else (fp-quantity q (string-append (fp-quantity-unit a) "/" (fp-quantity-unit b)) #f)])]
    [(fp-quantity? a) (let ([q (decimal-div (->dec (fp-quantity-value a)) (->dec b))]) (if q (fp-quantity q (fp-quantity-unit a) #f) cql-null))]
    [else (or (decimal-div (->dec a) (->dec b)) cql-null)]))
(define (power a b)
  (cond [(and (int-like? a) (int-like? b) (>= (->int b) 0)) (wrap-int (expt (->int a) (->int b)) (any-long? a b))]
        [else (float->dec (expt (exact->inexact (num->real a)) (exact->inexact (num->real b))))]))
(define (round-op vals)
  (define x (car vals))
  (define prec (if (and (pair? (cdr vals)) (not (cql-null? (cadr vals)))) (->int (cadr vals)) 0))
  (if (cql-null? x) cql-null (decimal-round (->dec x) prec)))


;; ---- living examples ----------------------------------------------------------------------------
(module+ test
  (require rackunit (only-in fhirpath/quantity make-quantity) (only-in fhirpath/temporal-ops parse-fp-date moment->string)
           (only-in "../error.rkt" cql-error? cql-error-message))
  ;; fails? : regexp -> (Any -> Boolean)   a cql-error whose message matches (the interpreter raises a value, not an exn)
  (define ((fails? rx) e) (and (cql-error? e) (regexp-match? rx (cql-error-message e))))
  (define (dec s) (parse-decimal s)) (define (q v u) (make-quantity (dec v) u))
  (check-equal? (add-values 1 2) 3)
  (check-true (cql-null? (add-values 2147483647 1)))                                  ; Integer overflow
  (check-equal? (add-values (fp-long 2147483647) 1) (fp-long 2147483648))
  (check-equal? (decimal->string (add-values (dec "1.10") 2)) "3.10")
  (check-equal? (decimal->string (fp-quantity-value (add-values (q "1.5" "mg") (q "0.5" "mg")))) "2")
  (check-equal? (fp-quantity-unit (add-values (q "1" "kg") (q "1" "g"))) "kg")          ; UCUM converts
  (check-equal? (moment->string (add-values (parse-fp-date "2024-01-31") (q "1" "month"))) "@2024-02-29")
  (check-equal? (moment->string (sub-values (parse-fp-date "2024-03-01") (q "1" "day"))) "@2024-02-29")
  (check-exn (fails? #rx"non-numeric") (lambda () (add-values "a" 1)))
  ;; uncertainty intervals combine by their corners
  (check-equal? (add-values (cql-interval 1 3 #t #t) 10) (cql-interval 11 13 #t #t))
  (check-equal? (sub-values 10 (cql-interval 1 3 #t #t)) (cql-interval 7 9 #t #t))
  (check-equal? (mul-values (cql-interval -2 3 #t #t) (cql-interval -1 2 #t #t)) (cql-interval -4 6 #t #t))
  ;; quantities: units multiply and divide per UCUM; same-unit division is dimensionless
  (check-equal? (fp-quantity-unit (mul-values (q "2" "m") (q "3" "m"))) "m2")
  (check-equal? (fp-quantity-unit (mul-values (q "2" "m") (q "3" "s"))) "m.s")
  (check-equal? (fp-quantity-unit (mul-values (q "2" "m") 3)) "m")
  (check-equal? (fp-quantity-unit (divide (q "6" "m") (q "3" "s"))) "m/s")
  (check-equal? (fp-quantity-unit (divide (q "6" "m") (q "3" "m"))) "1")
  (check-equal? (decimal->string (divide 1 4)) "0.25")
  (check-true (cql-null? (divide 1 0)))
  (check-equal? (trunc-div 7 2) 3) (check-equal? (trunc-div -7 2) -3)
  (check-equal? (decimal->string (trunc-div (dec "7.5") 2)) "3.0")
  (check-equal? (modulo-op 7 3) 1) (check-equal? (decimal->string (modulo-op (dec "7.5") 2)) "1.5")
  (check-equal? (decimal->string (fp-quantity-value (modulo-op (q "7" "g") (q "3" "g")))) "1.0")
  (check-equal? (power 2 10) 1024)
  (check-equal? (decimal->string (power 2 -1)) "0.5")
  (check-equal? (decimal->string (round-op (list (dec "2.345") 2))) "2.35")
  (check-equal? (round-op (list (dec "2.5"))) (dec "3"))
  (check-true (cql-null? (bin (list cql-null 1) +))))
