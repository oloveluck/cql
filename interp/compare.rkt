#lang racket/base

;;; interp/compare.rkt — CQL comparison and equality, three-valued: `cql-compare` orders two values of
;;; one kind (-1|0|1, or 'empty when undecidable — a second-precision moment against a millisecond
;;; one), `cql-equal` is structural equality ('true|'false|'empty) over lists, intervals, tuples,
;;; ratios, codes and concepts, `equivalent` is CQL's ~ (never null; decimals to their least
;;; precision, calendar durations pairwise), and the ordered operators read an Interval operand as
;;; an uncertainty band. The null-aware list helpers (membership, dedup) rest on cql-eq?.

(require (only-in racket/list remove-duplicates)
         (only-in fhirpath/decimal fp-decimal? decimal->exact decimal->string decimal-cmp decimal-round)
         (only-in fhirpath/value fp-long? fp-long-v fp-quantity? fp-quantity-value fp-quantity-unit)
         (only-in fhirpath/quantity quantity-compare)
         (only-in fhirpath/temporal-ops moment? moment-compare moment-frac)
         (only-in fhirpath/equality item-equal? item-equiv?)
         "../value.rkt" "base.rkt" "temporal.rkt")

(provide equal-op compare-op compare-op* pred->sym uncertain-rel range-of cql-compare cql-moment-compare frac?? cql-equal ratio-equal tri->val cql-eq? test-equal? list-equal interval-equal bound-equal tuple-equal equivalent decimal-equiv effective-precision ucum-duration-alias duration-unit fixed-unit-ms quantity-equiv duration-ms ratio-equiv equivalent2 tuple-equiv total-compare drop-nulls dedup cql-member? member*)

;; Equal, with uncertainty (Interval range) awareness: two intervals -> interval equality; an interval
;; against a scalar (a partial-precision duration result) -> band equality.
(define (equal-op vs)
  (define a (car vs)) (define b (cadr vs))
  (cond [(or (cql-null? a) (cql-null? b)) cql-null]
        [(and (cql-interval? a) (cql-interval? b)) (tri->val (cql-equal a b))]
        [(or (cql-interval? a) (cql-interval? b)) (uncertain-rel 'eq a b)]
        [else (tri->val (cql-equal a b))]))
(define (compare-op vals pred) (compare-op* vals pred #f))

;; ordered comparison; an Interval operand is treated as an uncertainty range (band comparison)
(define (compare-op* vals pred sym)
  (define a (car vals)) (define b (cadr vals))
  (cond [(or (cql-null? a) (cql-null? b)) cql-null]
        [(or (cql-interval? a) (cql-interval? b)) (uncertain-rel (or sym (pred->sym pred)) a b)]
        [else (let ([c (cql-compare a b)]) (if (eq? c 'empty) cql-null (pred c)))]))
(define (pred->sym pred) (cond [(pred -1) (if (pred 0) 'le 'lt)] [(pred 1) (if (pred 0) 'ge 'gt)] [else 'eq]))

;; uncertain-rel : Symbol CqlValue CqlValue -> Boolean|Null   band comparison over [lo,hi] ranges
(define (uncertain-rel sym a b)
  (define-values (alo ahi) (range-of a)) (define-values (blo bhi) (range-of b))
  (define (c< x y) (< (cql-compare x y) 0)) (define (c<= x y) (<= (cql-compare x y) 0))
  (case sym
    [(lt) (cond [(c< ahi blo) #t] [(c<= bhi alo) #f] [else cql-null])]
    [(le) (cond [(c<= ahi blo) #t] [(c< bhi alo) #f] [else cql-null])]
    [(gt) (cond [(c< bhi alo) #t] [(c<= ahi blo) #f] [else cql-null])]
    [(ge) (cond [(c<= bhi alo) #t] [(c< ahi blo) #f] [else cql-null])]
    [(eq) (cond [(and (= (cql-compare alo ahi) 0) (= (cql-compare blo bhi) 0) (= (cql-compare alo blo) 0)) #t]
                [(or (c< ahi blo) (c< bhi alo)) #f] [else cql-null])]
    [else cql-null]))
(define (range-of v) (if (cql-interval? v) (values (iv-low v) (iv-high v)) (values v v)))

;; -1|0|1|'empty
(define (cql-compare a b)
  (cond
    [(and (num? a) (num? b)) (if (and (int-like? a) (int-like? b)) (sgn (- (->int a) (->int b))) (decimal-cmp (->dec a) (->dec b)))]
    [(and (string? a) (string? b)) (cond [(string<? a b) -1] [(string>? a b) 1] [else 0])]
    [(and (moment? a) (moment? b)) (cql-moment-compare a b)]
    [(and (fp-quantity? a) (fp-quantity? b)) (quantity-compare a b)]
    [else 'empty]))    ; incomparable/unknown (e.g. a #f interval bound) -> undecidable, fail open to null

;; CQL treats milliseconds as their own precision: comparing a second-precision moment against a
;; millisecond-precision one is UNCERTAIN whenever they agree through seconds — the missing ms
;; component could fall either side (unlike FHIRPath, which reads a missing fraction as .000)
(define (cql-moment-compare a b)
  (cond [(eq? (frac?? a) (frac?? b)) (moment-compare a b)]
        [else (define cs (moment-compare (truncate-moment a 'second) (truncate-moment b 'second)))
              (if (eqv? cs 0) 'empty cs)]))
(define (frac?? m) (and (moment-frac m) #t))

;; CQL Equal: 'true|'false|'empty (null if either null, or a null-bearing structural compare)
(define (cql-equal a b)
  (cond
    [(or (cql-null? a) (cql-null? b)) 'empty]
    [(and (list? a) (list? b)) (list-equal a b)]
    [(and (cql-interval? a) (cql-interval? b)) (interval-equal a b)]
    [(and (cql-tuple? a) (cql-tuple? b)) (tuple-equal a b)]
    [(and (cql-ratio? a) (cql-ratio? b)) (ratio-equal a b)]
    ;; Code / Concept: every element equal, an absent element equal only to an absent one (the
    ;; reference engine's structural equality); a Concept's codes compared in order
    [(and (cql-code? a) (cql-code? b)) (if (equal? a b) 'true 'false)]
    [(and (cql-concept? a) (cql-concept? b))
     (if (and (equal? (cql-concept-display a) (cql-concept-display b))
              (= (length (cql-concept-codes a)) (length (cql-concept-codes b)))
              (andmap equal? (cql-concept-codes a) (cql-concept-codes b)))
         'true 'false)]
    [(or (cql-code? a) (cql-code? b) (cql-concept? a) (cql-concept? b)) 'false]
    ;; instances of a model's classes (a record and the same record reached another way, a QDM
    ;; encounter in two lists): equal when of one class with equal data
    [(and (model-node? a) (model-node? b))
     (if (and (equal? (model-node-type a) (model-node-type b)) (equal? (model-node-data a) (model-node-data b))) 'true 'false)]
    [else (item-equal? a b)]))       ; reuse FHIRPath scalar equality (System values)

;; ratio = : numerator and denominator must each be equal (as quantities)
(define (ratio-equal a b)
  (define n (cql-equal (cql-ratio-numerator a) (cql-ratio-numerator b)))
  (define d (cql-equal (cql-ratio-denominator a) (cql-ratio-denominator b)))
  (cond [(or (eq? n 'false) (eq? d 'false)) 'false]
        [(and (eq? n 'true) (eq? d 'true)) 'true]
        [else 'empty]))
(define (tri->val t) (case t [(true) #t] [(false) #f] [else cql-null]))
(define (cql-eq? a b) (eq? (cql-equal a b) 'true))

;; conformance comparison: expected null matches actual null; CQL equality when decidable; a
;; STRUCTURAL match settles the undecidable cases (two identical null-bounded intervals are the
;; "same result" for a test even though CQL `=` on them is null)
(define (test-equal? a b)
  (cond [(and (or (cql-null? a) (cql-null-list? a)) (or (cql-null? b) (cql-null-list? b))) #t]
        [(or (cql-null? a) (cql-null? b)) #f]
        [(with-handlers ([exn:fail? (lambda (_) #f)]) (eq? (cql-equal a b) 'true)) #t]
        [else (equal? a b)]))
(define (list-equal a b)
  (cond [(not (= (length a) (length b))) 'false]
        [else (let loop ([xs a] [ys b] [acc 'true])
                (cond [(null? xs) acc]
                      ;; in list equality, aligned null elements are considered equal ({null} = {null} is true)
                      [else (define r (if (and (cql-null? (car xs)) (cql-null? (car ys))) 'true (cql-equal (car xs) (car ys))))
                            (cond [(eq? r 'false) 'false] [(eq? r 'empty) (loop (cdr xs) (cdr ys) 'empty)]
                                  [else (loop (cdr xs) (cdr ys) acc)])]))]))
(define (interval-equal a b)
  (define lo (bound-equal (iv-low a) (iv-low b)))    ; #f bound (unbounded/unknown) -> 'empty
  (define hi (bound-equal (iv-high a) (iv-high b)))
  (cond [(or (eq? lo 'false) (eq? hi 'false)) 'false]
        [(and (eq? lo 'true) (eq? hi 'true)
              (eq? (iv-loc a) (iv-loc b)) (eq? (iv-hic a) (iv-hic b))) 'true]
        [else 'empty]))
(define (bound-equal a b) (cond [(and (not a) (not b)) 'empty] [(or (not a) (not b)) 'empty] [else (cql-equal a b)]))
(define (tuple-equal a b)
  (define ka (sort (hash-keys (cql-tuple-fields a)) symbol<?))
  (define kb (sort (hash-keys (cql-tuple-fields b)) symbol<?))
  (cond [(not (equal? ka kb)) 'false]
        ;; per reference behavior: the FIRST non-true component (in key order) decides — false OR null
        [else (let loop ([ks ka])
                (cond [(null? ks) 'true]
                      [else (define va (hash-ref (cql-tuple-fields a) (car ks))) (define vb (hash-ref (cql-tuple-fields b) (car ks)))
                            (define r (if (and (cql-null? va) (cql-null? vb)) 'true (cql-equal va vb)))
                            (if (eq? r 'true) (loop (cdr ks)) r)]))]))
(define (equivalent vals)
  ;; Equivalent never returns null: null ~ null is true; null ~ x is false
  (define a (car vals)) (define b (cadr vals))
  (cond [(and (cql-null? a) (cql-null? b)) #t]
        [(or (cql-null? a) (cql-null? b)) #f]
        [(and (list? a) (list? b)) (and (= (length a) (length b)) (andmap equivalent2 a b))]
        [(and (cql-tuple? a) (cql-tuple? b)) (tuple-equiv a b)]
        [(and (cql-interval? a) (cql-interval? b)) (eq? (interval-equal a b) 'true)]
        [(and (cql-ratio? a) (cql-ratio? b)) (ratio-equiv a b)]
        [(and (fp-decimal? a) (fp-decimal? b)) (decimal-equiv a b)]
        [(and (fp-quantity? a) (fp-quantity? b)) (quantity-equiv a b)]
        ;; Code ~ Code: code and system only ; Concept ~ Concept: a shared code ; a Code is the
        ;; Concept of itself (cql.hl7.org Equivalent)
        [(or (cql-code? a) (cql-concept? a) (cql-code? b) (cql-concept? b))
         (define (codes x) (cond [(cql-code? x) (list x)] [(cql-concept? x) (cql-concept-codes x)] [else #f]))
         (define ca (codes a)) (define cb (codes b))
         (and ca cb (for*/or ([x (in-list ca)] [y (in-list cb)])
                      (and (equal? (cql-code-code x) (cql-code-code y)) (equal? (cql-code-system x) (cql-code-system y))))
              #t)]
        [else (item-equiv? a b)]))

;; decimal ~ : same value to the LEAST effective precision (trailing zeros don't add precision):
;; 1.001 ~ 1.000 -> compare at precision 0 -> true.
(define (decimal-equiv a b)
  (define p (min (effective-precision a) (effective-precision b)))
  (= (decimal->exact (decimal-round a p)) (decimal->exact (decimal-round b p))))

;; the scale after stripping trailing fractional zeros
(define (effective-precision d)
  (define s (decimal->string d))
  (define m (regexp-match #px"\\.([0-9]*?)0*$" s))
  (if m (string-length (cadr m)) 0))

;; quantity ~ : calendar-duration equivalence uses the spec's PAIRWISE factors — 1 year = 12 months
;; AND 1 year = 365 days (deliberately non-transitive), 1 month = 30 days, 1 week = 7 days; UCUM
;; 'a'/'mo'/'wk'/'d'/'h'/'min'/'s'/'ms' alias the calendar units. Non-durations fall back to the
;; UCUM-aware comparison.
(define ucum-duration-alias
  (hash "a" "year" "mo" "month" "wk" "week" "d" "day" "h" "hour" "min" "minute" "s" "second" "ms" "millisecond"))
(define (duration-unit u)
  (define s (singular-unit u))
  (cond [(hash-ref ucum-duration-alias s #f)]
        [(member s '("year" "month" "week" "day" "hour" "minute" "second" "millisecond")) s]
        [else #f]))
(define fixed-unit-ms (hash "week" 604800000 "day" 86400000 "hour" 3600000
                            "minute" 60000 "second" 1000 "millisecond" 1))
(define (quantity-equiv a b)
  (define ua (duration-unit (fp-quantity-unit a)))
  (define ub (duration-unit (fp-quantity-unit b)))
  (define va (and ua (decimal->exact (->dec (fp-quantity-value a)))))
  (define vb (and ub (decimal->exact (->dec (fp-quantity-value b)))))
  (cond
    [(not (and ua ub)) (item-equiv? a b)]
    [(string=? ua ub) (= va vb)]
    ;; pairwise: year<->month by 12; year->365d and month->30d when compared against fixed units
    [(and (string=? ua "year") (string=? ub "month")) (= (* va 12) vb)]
    [(and (string=? ua "month") (string=? ub "year")) (= va (* vb 12))]
    [else (define ma (duration-ms ua va)) (define mb (duration-ms ub vb))
          (and ma mb (= ma mb))]))
(define (duration-ms u v)
  (cond [(hash-ref fixed-unit-ms u #f) => (lambda (f) (* v f))]
        [(string=? u "year") (* v 365 86400000)]
        [(string=? u "month") (* v 30 86400000)]
        [else #f]))

;; ratio ~ : equivalent by VALUE — a/b ~ c/d iff a·d = c·b (per spec; units must agree)
(define (ratio-equiv a b)
  (define (qv q) (if (fp-quantity? q) (decimal->exact (->dec (fp-quantity-value q))) (num->real q)))
  (define (qu q) (if (fp-quantity? q) (fp-quantity-unit q) "1"))
  (and (string=? (qu (cql-ratio-numerator a)) (qu (cql-ratio-numerator b)))
       (string=? (qu (cql-ratio-denominator a)) (qu (cql-ratio-denominator b)))
       (= (* (qv (cql-ratio-numerator a)) (qv (cql-ratio-denominator b)))
          (* (qv (cql-ratio-numerator b)) (qv (cql-ratio-denominator a))))))
(define (equivalent2 a b) (equivalent (list a b)))
(define (tuple-equiv a b)
  (define ka (sort (hash-keys (cql-tuple-fields a)) symbol<?))
  (define kb (sort (hash-keys (cql-tuple-fields b)) symbol<?))
  ;; comparing structurally different tuple types is a type error, not false
  (unless (equal? ka kb) (fail "cannot compare tuples of different types"))
  (andmap (lambda (k) (equivalent2 (hash-ref (cql-tuple-fields a) k) (hash-ref (cql-tuple-fields b) k))) ka))

;; drop-nulls : (Listof CqlValue) -> (Listof CqlValue)   nulls removed (aggregates ignore null)
(define (drop-nulls xs) (filter (lambda (e) (not (cql-null? e))) xs))

;; dedup : (Listof CqlValue) -> (Listof CqlValue)   CQL-equality de-duplication, order-preserving.
;; Fast O(n) path via an equal?-hash when every element is a plainly-hashable scalar of one kind
;; (Integer/String/Boolean); otherwise the general O(n²) cql-eq? path (matches prior behavior,
;; including null handling and value-based decimal equality).
(define (dedup xs)
  (cond
    [(null? xs) xs]
    [(or (andmap exact-integer? xs) (andmap string? xs) (andmap boolean? xs))
     (define seen (make-hash))
     (for/list ([x (in-list xs)] #:unless (hash-ref seen x #f)) (hash-set! seen x #t) x)]
    [else                                              ; general O(n²) path, null-aware (null = null)
     (let loop ([xs xs] [seen '()] [out '()])
       (cond [(null? xs) (reverse out)]
             [(cql-member? (car xs) seen) (loop (cdr xs) seen out)]
             [else (loop (cdr xs) (cons (car xs) seen) (cons (car xs) out))]))]))

;; cql-member? : CqlValue (Listof CqlValue) -> Boolean   null-aware membership
(define (cql-member? x lst)
  (and (ormap (lambda (e) (or (and (cql-null? x) (cql-null? e)) (cql-eq? x e))) lst) #t))
(define (member* e lst) (ormap (lambda (x) (cql-eq? e x)) lst))

;; total-compare : CqlValue CqlValue -> -1|0|1   cql-compare, totalized
(define (total-compare a b)
  (define c (cql-compare a b))
  (cond [(number? c) c]
        [(and (moment? a) (moment? b)) (moment-total-compare a b)]
        [else 0]))


;; ---- living examples ----------------------------------------------------------------------------
(module+ test
  (require (except-in rackunit test-equal?) (only-in fhirpath/decimal parse-decimal) (only-in fhirpath/quantity make-quantity)
           (only-in fhirpath/temporal-ops parse-fp-date parse-fp-datetime) (only-in "../error.rkt" cql-error?))
  (define (dec s) (parse-decimal s)) (define (q v u) (make-quantity (dec v) u))
  (define (dt s) (parse-fp-datetime s))
  ;; ordering: one kind at a time, 'empty across kinds or when undecidable
  (check-equal? (cql-compare 1 2) -1) (check-equal? (cql-compare (dec "2.0") 2) 0)
  (check-equal? (cql-compare "b" "a") 1)
  (check-equal? (cql-compare (q "1" "m") (q "100" "cm")) 0)
  (check-equal? (cql-compare 1 "1") 'empty)
  (check-equal? (cql-compare (dt "2024-03-05T10:30:15") (dt "2024-03-05T10:30:15.000")) 'empty)   ; ms is its own precision
  (check-equal? (cql-compare (dt "2024-03-05T10:30:15") (dt "2024-03-05T10:30:16.000")) -1)
  (check-equal? (total-compare (parse-fp-date "2012-10-05") (dt "2012-10-05T10")) -1)
  ;; equality is structural and three-valued
  (check-equal? (cql-equal (list 1 2) (list 1 2)) 'true)
  (check-equal? (cql-equal (list 1 cql-null) (list 1 cql-null)) 'true)
  (check-equal? (cql-equal (list 1 cql-null) (list 1 2)) 'empty)
  (check-equal? (cql-equal (cql-interval 1 5 #t #t) (cql-interval 1 5 #t #t)) 'true)
  (check-equal? (cql-equal (cql-interval 1 5 #t #t) (cql-interval 1 4 #t #t)) 'false)      ; [1,5) is [1,4] once built
  (check-equal? (cql-equal (cql-interval #f 5 #t #t) (cql-interval #f 5 #t #t)) 'empty)
  (check-equal? (cql-equal (cql-tuple (hash 'a 1 'b 2)) (cql-tuple (hash 'b 2 'a 1))) 'true)
  (check-equal? (cql-equal (cql-tuple (hash 'a 1)) (cql-tuple (hash 'b 1))) 'false)
  (check-equal? (cql-equal (cql-code "a" "s" #f #f) (cql-code "a" "s" #f #f)) 'true)
  (check-equal? (cql-equal (cql-code "a" "s" #f #f) (cql-code "a" "s" "1" #f)) 'false)
  (check-equal? (cql-equal (cql-ratio (q "1" "m") (q "2" "s")) (cql-ratio (q "1" "m") (q "2" "s"))) 'true)
  (check-true (cql-eq? "x" "x")) (check-equal? (tri->val 'empty) cql-null)
  (check-equal? (equal-op (list (cql-interval 1 3 #t #t) 2)) cql-null)          ; an uncertainty band
  (check-equal? (equal-op (list (cql-interval 2 2 #t #t) 2)) #t)
  (check-equal? (compare-op (list (cql-interval 1 3 #t #t) 5) (lambda (c) (< c 0))) #t)
  (check-equal? (compare-op (list (cql-interval 1 3 #t #t) 2) (lambda (c) (< c 0))) cql-null)
  (check-equal? (compare-op (list 1 cql-null) (lambda (c) (< c 0))) cql-null)
  ;; equivalence never answers null
  (check-true (equivalent (list cql-null cql-null))) (check-false (equivalent (list cql-null 1)))
  (check-true (equivalent (list (dec "1.001") (dec "1.000"))))                      ; least precision
  (check-false (equivalent (list (dec "1.001") (dec "1.002"))))
  (check-true (equivalent (list (q "1" "year") (q "12" "months"))))
  (check-true (equivalent (list (q "1" "year") (q "365" "days"))))
  (check-false (equivalent (list (q "12" "months") (q "365" "days"))))             ; pairwise, not transitive
  (check-true (equivalent (list (cql-code "a" "s" #f "x") (cql-code "a" "s" "2" "y"))))
  (check-true (equivalent (list (cql-code "a" "s" #f #f) (cql-concept (list (cql-code "b" "s" #f #f) (cql-code "a" "s" #f #f)) #f))))
  (check-true (equivalent (list (cql-ratio (q "1" "m") (q "2" "m")) (cql-ratio (q "2" "m") (q "4" "m")))))
  (check-exn (lambda (e) (cql-error? e)) (lambda () (equivalent (list (cql-tuple (hash 'a 1)) (cql-tuple (hash 'b 1))))))
  ;; the harness's comparison settles what = leaves open, structurally
  (check-true (test-equal? (cql-interval #f 5 #t #t) (cql-interval #f 5 #t #t)))
  (check-true (test-equal? cql-null cql-null-list)) (check-false (test-equal? cql-null 1))
  ;; null-aware list helpers
  (check-equal? (drop-nulls (list 1 cql-null 2)) '(1 2))
  (check-equal? (dedup (list 1 2 1 3 2)) '(1 2 3))
  (check-equal? (dedup (list (dec "1.0") 1 cql-null cql-null)) (list (dec "1.0") cql-null))
  (check-true (cql-member? cql-null (list 1 cql-null)))
  (check-false (member* 2 (list 1 3))))
