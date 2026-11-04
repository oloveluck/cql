#lang racket/base

;;; interp/interval.rkt — CQL intervals: construction (open bounds of a discrete point type close to
;;; the successor/predecessor; a null bound is unbounded), start/end/width/point, the relational
;;; operators (before/after/meets/overlaps/starts/ends/includes, precision-qualified over moments),
;;; membership of a point, the set operations, collapse and expand, and the timing dispatch that
;;; sends point, interval and list operands to their own comparison.

(require (only-in racket/list append-map)
         (only-in fhirpath/decimal fp-decimal? parse-decimal decimal->exact)
         (only-in fhirpath/value fp-long fp-long? fp-long-v fp-quantity fp-quantity? fp-quantity-value fp-quantity-unit)
         (only-in fhirpath/temporal-ops moment? moment-compare moment-kind moment-year moment-prec moment-frac moment-plus moment-add-ms)
         "../value.rkt" "base.rkt" "temporal.rkt" "arith.rkt" "compare.rkt")

(provide make-interval-value point-succ point-pred point-step int-guard-overflow? iv-start iv-end iv-width iv-point collapse-intervals merge-iv >=? cmp-low cmp-high expand-interval expand-step expand-points point-le? step-scale expand-one number-point? ->exactnum num->cql interval-timing or-null and3 starts-strictly-before ends-strictly-after trunc-iv trunc-pt cmp-endpoints le-or-null ge-or-null rel-before rel-after rel-overlaps unknown-interval? rel-meets start-contains end-contains rel-includes rel-starts rel-ends timing-op-eval list-timing in-op point-in-interval set-op interval-set-op merge-iv2 step-or-fail)

;; timing-op-eval : Symbol (Symbol|#f) CqlValue CqlValue -> Boolean|Null
(define (timing-op-eval op* prec a b)
  ;; the point-vs-interval ELM forms (In/Contains/ProperIn/ProperContains) evaluate as their interval
  ;; counterparts — the value shapes decide
  (define op (case op* [(in) 'included-in] [(contains) 'includes]
               [(proper-in) 'properly-included-in] [(proper-contains) 'properly-includes] [else op*]))
  (cond
    ;; list inclusion FIRST — a null operand against a list is decidable (in/includes are false-based)
    [(or (list? a) (list? b) (cql-null-list? a) (cql-null-list? b)) (list-timing op a b)]
    [(or (cql-null? a) (cql-null? b)) cql-null]
    [(or (cql-interval? a) (cql-interval? b)) (interval-timing op prec a b)]
    [(and (moment? a) (moment? b)) (point-timing op prec a b)]
    [else cql-null]))

;; list inclusion: includes / included in (+ properly) and during over lists. `A properly includes B`
;; = every element of B is in A AND A has MORE elements than B (duplicates count; a scalar counts 1).
(define (list-timing op a b)
  (define (mem e y) (eq? (in-op e y) #t))
  (define (subset? x y) (andmap (lambda (e) (mem e y)) x))
  (define (size v) (cond [(list? v) (length v)] [else 1]))   ; scalar (incl. null) counts as one element
  (define (as-list v) (if (list? v) v (list v)))
  ;; a list-typed null against a LIST is undecidable (null); against a scalar the answer is false
  (define (nl? v) (cql-null-list? v))
  (case op
    [(includes) (cond [(or (nl? a) (nl? b)) cql-null]
                      [(list? a) (subset? (as-list b) a)] [(cql-null? a) #f] [else cql-null])]
    [(included-in during) (cond [(or (nl? a) (nl? b)) cql-null]
                                [(list? b) (subset? (as-list a) b)] [(cql-null? b) #f] [else cql-null])]
    [(properly-includes)
     (cond [(and (nl? a) (not (list? b)) (not (nl? b))) #f]   ; null-list vs scalar -> false
           [(or (nl? a) (nl? b)) cql-null]
           [(list? a) (and (subset? (as-list b) a) (> (length a) (size b)))]
           [(cql-null? a) #f] [else cql-null])]
    [(properly-included-in)
     (cond [(and (nl? b) (not (list? a)) (not (nl? a))) #f]
           [(or (nl? a) (nl? b)) cql-null]
           [(list? b) (and (subset? (as-list a) b) (> (length b) (size a)))]
           [(cql-null? b) #f] [else cql-null])]
    [else cql-null]))

;; CQL normalizes intervals over discrete point types to closed bounds (open bound -> successor/
;; predecessor). A null bound stays null (infinite). Bounds that cross after normalization are invalid.
(define (make-interval-value low high lc hc)
  (cond
    [(or (cql-null? low) (cql-null? high))
     (cql-interval (nn->f low) (nn->f high) lc hc)]     ; a null bound -> #f (infinite/unknown)
    [else
     (define-values (l l-c) (if (and (not lc) (point-succ low)) (values (point-succ low) #t) (values low lc)))
     (define-values (h h-c) (if (and (not hc) (point-pred high)) (values (point-pred high) #t) (values high hc)))
     (when (and l h (let ([c (cql-compare l h)]) (and (number? c) (> c 0)))) (fail "invalid interval: low > high"))
     (cql-interval l h l-c h-c)]))

;; point-succ / point-pred : the next/previous value of an ordered point type. Decimal step is 1e-8
;; (CQL decimal precision); a moment steps by its own precision (day-precision date -> +1 day, etc.).
(define (point-succ p) (point-step p 1))
(define (point-pred p) (point-step p -1))
(define (point-step p dir)
  (cond [(and (exact-integer? p) (not (int-guard-overflow? (+ p dir)))) (+ p dir)]
        [(exact-integer? p) cql-null]
        [(fp-long? p) (fp-long (+ (fp-long-v p) dir))]
        [(fp-decimal? p) (rat->dec (+ (decimal->exact p) (* dir 1/100000000)))]
        [(fp-quantity? p) (fp-quantity (rat->dec (+ (decimal->exact (->dec (fp-quantity-value p))) (* dir 1/100000000)))
                                       (fp-quantity-unit p) #f)]
        [(moment? p) (moment-step p dir)]
        [else #f]))
(define (int-guard-overflow? n) (or (< n INT-MIN) (> n INT-MAX)))

;; start of / end of : the effective first/last point (open bound -> successor/predecessor)
(define (iv-start iv)
  (cond [(not (cql-interval? iv)) cql-null] [(not (iv-low iv)) cql-null]
        [(iv-loc iv) (iv-low iv)] [else (or (point-succ (iv-low iv)) (iv-low iv))]))
(define (iv-end iv)
  (cond [(not (cql-interval? iv)) cql-null] [(not (iv-high iv)) cql-null]
        [(iv-hic iv) (iv-high iv)] [else (or (point-pred (iv-high iv)) (iv-high iv))]))
(define (iv-width iv)
  (cond [(not (cql-interval? iv)) cql-null] [(or (not (iv-low iv)) (not (iv-high iv))) cql-null]
        [else (sub-values (iv-high iv) (iv-low iv))]))
(define (iv-point iv)
  (cond [(not (cql-interval? iv)) cql-null]
        [(let ([s (iv-start iv)] [e (iv-end iv)]) (and (not (cql-null? s)) (equal? (cql-compare s e) 0))) (iv-start iv)]
        [else cql-null]))          ; point from a non-unit interval is undefined -> null

;; collapse : merge a list of overlapping/adjacent intervals
(define (collapse-intervals x)
  (cond [(cql-null? x) cql-null] [(not (list? x)) cql-null]
        [else (define (fully-null? iv) (and (not (iv-low iv)) (not (iv-high iv))))   ; (null,null) drops out
              (define ivs (sort (filter (lambda (iv) (and (cql-interval? iv) (not (fully-null? iv)))) (drop-nulls x))
                                (lambda (p q) (< (cmp-low (iv-low p) (iv-low q)) 0))))
              (let loop ([xs ivs] [acc '()])
                (cond [(null? xs) (reverse acc)]
                      [(null? acc) (loop (cdr xs) (list (car xs)))]
                      [else (define prev (car acc)) (define cur (car xs))
                            (if (or (eq? (rel-overlaps prev cur) #t) (eq? (rel-meets prev cur) #t))
                                (loop (cdr xs) (cons (merge-iv prev cur) (cdr acc)))
                                (loop (cdr xs) (cons cur acc)))]))]))
(define (merge-iv a b) (merge-iv2 a b))
(define (>=? a b) (let ([c (cql-compare a b)]) (and (number? c) (>= c 0))))

;; null-safe low/high boundary comparison: #f low = -inf, #f high = +inf ; -> -1|0|1
(define (cmp-low a b) (cond [(and (not a) (not b)) 0] [(not a) -1] [(not b) 1] [else (let ([c (cql-compare a b)]) (if (number? c) c 0))]))
(define (cmp-high a b) (cond [(and (not a) (not b)) 0] [(not a) 1] [(not b) -1] [else (let ([c (cql-compare a b)]) (if (number? c) c 0))]))

;; expand — two forms, per the CQL spec:
;;   expand <interval>       per s  -> the list of POINTS s apart (start, start+s, … <= end)
;;   expand <list-of-ivs>    per s  -> step-wide SUB-INTERVALS covering each interval ([p, p+s-ulp])
;; The step is a number (numeric intervals) or a calendar quantity (temporal intervals); a fractional
;; numeric step yields Decimal points. Undecidable inputs -> null (empty list stays an empty list).
(define (expand-interval x per)
  (cond
    [(cql-null? x) cql-null]
    [(list? x) (append-map (lambda (iv) (let ([r (expand-one iv per)]) (if (list? r) r '()))) (drop-nulls x))]
    [(cql-interval? x) (expand-points x per)]
    [else cql-null]))

;; expand-step : per -> (values kind step) ; kind 'num (step: exact number) | 'cal (step: (n . unit)) | 'bad
(define (expand-step per)
  (cond [(or (not per) (cql-null? per)) (values 'num 1)]
        [(fp-quantity? per)
         (define u (fp-quantity-unit per))
         (define n (decimal->exact (->dec (fp-quantity-value per))))
         (if (calendar-unit-name? u) (values 'cal (cons (inexact->exact (round n)) u)) (values 'num n))]
        [(num? per) (values 'num (num->real per))]
        [else (values 'bad #f)]))

;; The expand algorithm (mirrors the reference implementation):
;;  * numeric: convert integer bounds to the step's decimal scale via low/high boundary (10 -> [10.0,
;;    10.9] at scale 1); a point p is included while its whole step-window [p, p+step-ulp] fits.
;;  * temporal: a bound COARSER than the step's unit makes the expansion empty; finer bounds are
;;    truncated to the unit; the window is [p, p+(n-1) units].
;; expand-points : interval per -> (Listof point) | null | '()
(define (expand-points iv per)
  (define s (iv-start iv)) (define e (iv-end iv))
  (define-values (kind step) (expand-step per))
  (cond
    [(or (cql-null? s) (cql-null? e) (eq? kind 'bad)) cql-null]
    [(and (eq? kind 'num) (number-point? s) (positive? step))
     (define scale (step-scale step))
     (define dec? (> scale 0))
     (define ulp (if dec? (expt 10 (- scale)) 1))
     ;; integer bounds under a fractional step widen to their decimal range (10 -> 10.0 .. 10.9)
     (define lo (->exactnum s))
     (define hi0 (->exactnum e))
     (define hi (if (and dec? (exact-integer? hi0) (exact-integer? lo)) (+ hi0 1 (- ulp)) hi0))
     (let loop ([p lo] [acc '()])
       (if (<= (+ p step (- ulp)) hi) (loop (+ p step) (cons (num->cql p dec?) acc)) (reverse acc)))]
    [(and (eq? kind 'cal) (moment? s) (positive? (car step)))
     (define lvl (hash-ref duration-level (singular-unit (cdr step)) 6))
     (cond
       [(< (moment-prec s) lvl) '()]                 ; bound coarser than the step unit -> empty
       [else (define sym (level->prec-symbol lvl))
             (define s* (truncate-moment s sym))
             (define e* (truncate-moment e sym))
             ;; moment-plus can raise the precision (hour math goes through ms) — re-truncate each step
             (define (step+ p n) (truncate-moment (moment-plus p n (cdr step)) sym))
             (let loop ([p s*] [acc '()])
               (define window-end (if (> (car step) 1) (step+ p (sub1 (car step))) p))
               (if (eq? (point-le? window-end e*) #t)
                   (loop (step+ p (car step)) (cons p acc))
                   (reverse acc)))])]
    [else cql-null]))
(define (point-le? a b) (let ([c (cql-compare a b)]) (if (eq? c 'empty) cql-null (<= c 0))))

;; the decimal scale of a step (0.1 -> 1, 2 -> 0)
(define (step-scale st) (if (integer? st) 0 (let loop ([x st] [n 0]) (if (integer? x) n (loop (* x 10) (add1 n))))))

;; expand-one : interval per -> (Listof interval) | null   step-window sub-intervals [p, p+step-ulp]
(define (expand-one iv per)
  (define pts (expand-points iv per))
  (define-values (kind step) (expand-step per))
  (cond
    [(not (list? pts)) cql-null]
    [else
     (define (upper p)
       (cond [(eq? kind 'cal) (if (> (car step) 1) (moment-plus p (sub1 (car step)) (cdr step)) p)]
             [(= 0 (step-scale step)) (if (exact-integer? p) (+ p (inexact->exact step) -1) p)]  ; [p, p+step-1]
             [else p]))                                            ; a fractional step's window is [p, p]
     ;; decimal-mode points arrive as rendered CqlValues; recompute uppers from the raw walk instead
     (map (lambda (p) (cql-interval p (if (fp-decimal? p) p (upper p)) #t #t)) pts)]))
(define (number-point? p) (or (exact-integer? p) (fp-long? p) (fp-decimal? p)))
(define (->exactnum p) (cond [(fp-decimal? p) (decimal->exact p)] [(fp-long? p) (fp-long-v p)] [else p]))
(define (num->cql p dec?)
  (if (and (not dec?) (integer? p)) (if (exact? p) p (inexact->exact p))
      (or (parse-decimal (number->string (exact->inexact p))) p)))

;; interval relational operators (op symbols from parse-timing-op)
(define (interval-timing op prec a b)
  (case op
    [(before) (rel-before (trunc-iv a prec) (trunc-iv b prec))]
    [(after) (rel-after (trunc-iv a prec) (trunc-iv b prec))]
    [(same-or-before) (let ([c (cmp-endpoints (hi-pt (trunc-iv a prec)) (lo-pt (trunc-iv b prec)))]) (le-or-null c))]
    [(same-or-after) (let ([c (cmp-endpoints (lo-pt (trunc-iv a prec)) (hi-pt (trunc-iv b prec)))]) (ge-or-null c))]
    [(meets) (or-null (rel-meets a b) (rel-meets b a))]
    [(meets-before) (rel-meets a b)]
    [(meets-after) (rel-meets b a)]
    [(overlaps) (rel-overlaps a b)]
    ;; A overlaps before B: A overlaps B AND A starts before B starts
    [(overlaps-before) (and3 (rel-overlaps a b) (starts-strictly-before a b))]
    ;; A overlaps after B: A overlaps B AND A ends after B ends
    [(overlaps-after) (and3 (rel-overlaps a b) (ends-strictly-after a b))]
    [(starts) (rel-starts a b)]
    [(ends) (rel-ends a b)]
    ;; precision-qualified inclusion truncates both operands to the precision first
    [(includes) (rel-includes (trunc-iv a prec) (trunc-iv b prec) #f)]
    [(properly-includes) (rel-includes (trunc-iv a prec) (trunc-iv b prec) #t)]
    [(included-in) (rel-includes (trunc-iv b prec) (trunc-iv a prec) #f)]
    [(properly-included-in) (rel-includes (trunc-iv b prec) (trunc-iv a prec) #t)]
    [(same-as) (tri->val (cql-equal a b))]
    [else cql-null]))
(define (or-null x y) (cond [(eq? x #t) #t] [(eq? y #t) #t] [(or (cql-null? x) (cql-null? y)) cql-null] [else #f]))

;; and3 : (Boolean|Null) (Boolean|Null) -> Boolean|Null   three-valued conjunction of relation results
(define (and3 x y) (cond [(or (eq? x #f) (eq? y #f)) #f] [(and (eq? x #t) (eq? y #t)) #t] [else cql-null]))

;; A's effective start strictly before B's ; A's effective end strictly after B's — null when unknowable
(define (starts-strictly-before a b)
  (define sa (and (cql-interval? a) (iv-low a))) (define sb (and (cql-interval? b) (iv-low b)))
  (cond [(and (not sa) sb) #t] [(not sb) (if sa #f cql-null)]
        [else (define c (cql-compare (iv-start a) (iv-start b)))
              (if (eq? c 'empty) cql-null (< c 0))]))
(define (ends-strictly-after a b)
  (define ea (and (cql-interval? a) (iv-high a))) (define eb (and (cql-interval? b) (iv-high b)))
  (cond [(and (not ea) eb) #t] [(not eb) (if ea #f cql-null)]
        [else (define c (cql-compare (iv-end a) (iv-end b)))
              (if (eq? c 'empty) cql-null (> c 0))]))

;; truncate a value or interval's moment bounds to a precision (for precision-qualified timing)
(define (trunc-iv v prec)
  (cond [(not prec) v]
        [(cql-interval? v) (cql-interval (trunc-pt (iv-low v) prec) (trunc-pt (iv-high v) prec) (iv-loc v) (iv-hic v))]
        [else (trunc-pt v prec)]))
(define (trunc-pt v prec) (if (moment? v) (truncate-moment v prec) v))
(define (cmp-endpoints a b) (cond [(or (not a) (not b)) 'empty] [else (cql-compare a b)]))
(define (le-or-null c) (cond [(eq? c 'empty) cql-null] [(<= c 0) #t] [else #f]))
(define (ge-or-null c) (cond [(eq? c 'empty) cql-null] [(>= c 0) #t] [else #f]))
(define (rel-before a b)
  (define ha (hi-pt a)) (define lb (lo-pt b))
  (cond [(or (not ha) (not lb)) #f]
        [else (define c (cql-compare ha lb))
              (cond [(eq? c 'empty) cql-null] [(< c 0) #t]
                    [(and (= c 0) (not (and (hic a) (loc b)))) #t] [else #f])]))
(define (rel-after a b)
  (define la (lo-pt a)) (define hb (hi-pt b))
  (cond [(or (not la) (not hb)) #f]
        [else (define c (cql-compare la hb))
              (cond [(eq? c 'empty) cql-null] [(> c 0) #t]
                    [(and (= c 0) (not (and (loc a) (hic b)))) #t] [else #f])]))
(define (rel-overlaps a b)
  (cond [(or (unknown-interval? a) (unknown-interval? b)) cql-null]   ; (null,null) is unknowable
        [else (define bf (rel-before a b)) (define af (rel-after a b))
              (cond [(or (cql-null? bf) (cql-null? af)) cql-null] [else (and (not bf) (not af))])]))

;; an interval with BOTH bounds null carries no information at all
(define (unknown-interval? v) (and (cql-interval? v) (not (iv-low v)) (not (iv-high v))))
(define (rel-meets a b)   ; a ends immediately before b starts (adjacent, no gap/overlap)
  (define ha (hi-pt a)) (define lb (lo-pt b))
  (cond [(and (not ha) (not lb)) #f]                      ; both endpoints unknown -> definitely not adjacent
        [(or (not ha) (not lb)) cql-null]                 ; one unknown endpoint is unknowable
        [else (define s (point-succ ha))
              (cond [(and s (not (cql-null? s)) (equal? (cql-compare s lb) 0) (hic a) (loc b)) #t]
                    [(and (equal? (cql-compare ha lb) 0) (not (and (hic a) (loc b)))) #t]
                    [else #f])]))

;; start-contains: a starts at or before the start of b ; end-contains: a ends at or after end of b
(define (start-contains a b)
  (define al (iv-low a)) (define bl (lo-pt b))
  (cond [(not al) #t] [(not bl) #f]
        [else (define c (cql-compare al bl))
              (cond [(eq? c 'empty) 'empty] [(< c 0) #t] [(= c 0) (or (iv-loc a) (not (loc b)))] [else #f])]))
(define (end-contains a b)
  (define ah (iv-high a)) (define bh (hi-pt b))
  (cond [(not ah) #t] [(not bh) #f]
        [else (define c (cql-compare ah bh))
              (cond [(eq? c 'empty) 'empty] [(> c 0) #t] [(= c 0) (or (iv-hic a) (not (hic b)))] [else #f])]))
(define (rel-includes a b proper?)
  (cond
    [(not (cql-interval? a)) cql-null]
    ;; a POINT is properly included only strictly inside (not at either boundary)
    [(and proper? (not (cql-interval? b)))
     (define in (point-in-interval b a))
     (cond [(not (eq? in #t)) in]
           [else (define at-start (cql-equal b (iv-start a)))
                 (define at-end (cql-equal b (iv-end a)))
                 (cond [(or (eq? at-start 'empty) (eq? at-end 'empty)) cql-null]
                       [(or (eq? at-start 'true) (eq? at-end 'true)) #f]
                       [else #t])])]
    [else (define s (start-contains a b)) (define e (end-contains a b))
          (cond [(or (eq? s 'empty) (eq? e 'empty)) cql-null]
                [(and s e) (if proper? (not (eq? (cql-equal a b) 'true)) #t)] [else #f])]))
(define (rel-starts a b)   ; a.low = b.low and a.high <= b.high ; a null bound is unknowable
  (cond [(not (and (cql-interval? a) (cql-interval? b))) cql-null]
        [(or (unknown-interval? a) (unknown-interval? b) (not (iv-low a)) (not (iv-low b))) cql-null]
        [else (define lo-eq (and (equal? (cql-compare (iv-low a) (iv-low b)) 0) (eq? (iv-loc a) (iv-loc b))))
              (define hi-ok (end-contains b a))
              (if (eq? hi-ok 'empty) cql-null (and lo-eq (eq? hi-ok #t)))]))
(define (rel-ends a b)     ; a.high = b.high and a.low >= b.low ; a null bound is unknowable
  (cond [(not (and (cql-interval? a) (cql-interval? b))) cql-null]
        [(or (unknown-interval? a) (unknown-interval? b) (not (iv-high a)) (not (iv-high b))) cql-null]
        [else (define hi-eq (and (equal? (cql-compare (iv-high a) (iv-high b)) 0) (eq? (iv-hic a) (iv-hic b))))
              (define lo-ok (start-contains b a))
              (if (eq? lo-ok 'empty) cql-null (and hi-eq (eq? lo-ok #t)))]))

;; the operator form errors at the type boundary (interval internals use point-step and degrade)
(define (step-or-fail x dir)
  (cond [(cql-null? x) cql-null]
        [else (define r (point-step x dir))
              (cond [(eq? r #f) (fail "predecessor/successor out of range")]
                    [else r])]))
(define (in-op x coll)
  (cond
    [(cql-interval? coll) (point-in-interval x coll)]
    [(or (cql-null? coll) (cql-null-list? coll)) #f]   ; membership in a null list is false (per CQL tests)
    [(list? coll) (if (cql-member? x coll) #t #f)]     ; found (null matches null) -> true, else false
    [else (fail "In: not a list or interval")]))

;; point-in-interval : CqlValue Interval -> Boolean|Null   three-valued: an undecidable bound
;; comparison (mixed second/ms precision) makes the membership null, not false
(define (point-in-interval x iv)
  (cond
    [(cql-null? x) cql-null]
    [(unknown-interval? iv) #f]                    ; membership in a fully-unknown interval is false
    [else (let ([lo (iv-low iv)] [hi (iv-high iv)])   ; a single #f bound is unbounded on that side
            (define ge-lo (if (not lo) #t (let ([c (cql-compare x lo)])
                                            (if (eq? c 'empty) cql-null (if (iv-loc iv) (>= c 0) (> c 0))))))
            (define le-hi (if (not hi) #t (let ([c (cql-compare x hi)])
                                            (if (eq? c 'empty) cql-null (if (iv-hic iv) (<= c 0) (< c 0))))))
            (and3 ge-lo le-hi))]))
(define (set-op vals kind)
  ;; a list-typed null behaves as the empty list in set algebra (union/intersect/except)
  (define a (let ([v (car vals)]) (if (cql-null-list? v) '() v)))
  (define b (let ([v (cadr vals)]) (if (cql-null-list? v) '() v)))
  (cond [(and (eq? kind 'except) (list? a) (cql-null? b)) a]   ; list except null -> the list
        [(or (cql-null? a) (cql-null? b)) cql-null]
        [(or (cql-interval? a) (cql-interval? b)) (interval-set-op kind a b)]
        [else (case kind
                [(union) (dedup (append a b))]
                [(intersect) (dedup (filter (lambda (e) (member* e b)) a))]
                [(except) (dedup (filter (lambda (e) (not (member* e b))) a))])]))

;; interval set operations: union (only if overlapping/meeting), intersect, except -> an interval or null
(define (interval-set-op kind a b)
  (cond
    [(not (and (cql-interval? a) (cql-interval? b))) cql-null]
    [(eq? kind 'union) (if (or (eq? (rel-overlaps a b) #t) (eq? (rel-meets a b) #t) (eq? (rel-meets b a) #t)) (merge-iv2 a b) cql-null)]
    [(eq? kind 'intersect)
     ;; low = max of lows, high = min of highs — but a NULL (unknown) bound propagates as null
     (if (eq? (rel-overlaps a b) #t)
         (let ([lo (cond [(not (iv-low a)) b] [(not (iv-low b)) a]
                         [(>= (cmp-low (iv-low a) (iv-low b)) 0) a] [else b])]
               [hi (cond [(not (iv-high a)) a] [(not (iv-high b)) b]   ; unknown high stays unknown
                         [(<= (cmp-high (iv-high a) (iv-high b)) 0) a] [else b])])
           (cql-interval (iv-low lo) (iv-high hi) (iv-loc lo) (iv-hic hi)))
         cql-null)]
    [(eq? kind 'except)
     (cond [(cql-null? (rel-overlaps a b)) cql-null]      ; undecidable overlap -> null
           [(not (eq? (rel-overlaps a b) #t)) a]
           [(and (eq? (start-contains b a) #t) (eq? (end-contains b a) #t)) cql-null] ; b covers a
           [(eq? (start-contains b a) #t) (make-interval-value (point-succ (iv-high b)) (iv-high a) #t (iv-hic a))]
           [(eq? (end-contains b a) #t) (make-interval-value (iv-low a) (point-pred (iv-low b)) (iv-loc a) #t)]
           [else cql-null])]
    [else cql-null]))
(define (merge-iv2 a b)
  (define lo (if (<= (cmp-low (iv-low a) (iv-low b)) 0) a b))
  (define hi (if (>= (cmp-high (iv-high a) (iv-high b)) 0) a b))
  (cql-interval (iv-low lo) (iv-high hi) (iv-loc lo) (iv-hic hi)))


;; ---- living examples ----------------------------------------------------------------------------
(module+ test
  (require rackunit (only-in fhirpath/quantity make-quantity) (only-in fhirpath/temporal-ops parse-fp-date moment->string)
           (only-in "../error.rkt" cql-error? cql-error-message))
  ;; fails? : regexp -> (Any -> Boolean)   a cql-error whose message matches (the interpreter raises a value, not an exn)
  (define ((fails? rx) e) (and (cql-error? e) (regexp-match? rx (cql-error-message e))))
  (define (iv l h [lc #t] [hc #t]) (cql-interval l h lc hc))
  (define (d s) (parse-fp-date s))
  ;; construction closes open bounds of a discrete type; crossing bounds are an error
  (check-equal? (make-interval-value 1 5 #f #f) (iv 2 4))
  (check-equal? (make-interval-value (parse-decimal "1.0") 2 #f #t) (iv (parse-decimal "1.00000001") 2))
  (check-equal? (make-interval-value cql-null 5 #t #t) (iv #f 5))
  (check-exn (fails? #rx"low > high") (lambda () (make-interval-value 5 1 #t #t)))
  (check-equal? (point-succ 2147483647) cql-null)
  (check-equal? (moment->string (point-pred (d "2024-03"))) "@2024-02")
  (check-exn (fails? #rx"out of range") (lambda () (step-or-fail (d "9999") 1)))
  ;; start / end / width / point
  (check-equal? (iv-start (iv 1 5 #f #t)) 2) (check-equal? (iv-end (iv 1 5)) 5)
  (check-equal? (iv-width (iv 1 5)) 4) (check-true (cql-null? (iv-width (iv #f 5))))
  (check-equal? (iv-point (iv 3 3)) 3) (check-true (cql-null? (iv-point (iv 1 3))))
  ;; the relations, three-valued on unknown bounds
  (check-equal? (rel-before (iv 1 2) (iv 3 4)) #t) (check-equal? (rel-before (iv 1 3) (iv 3 4)) #f)
  (check-equal? (rel-before (iv 1 3 #t #f) (iv 3 4)) #t)                           ; [1,3) before [3,4]
  (check-equal? (rel-meets (iv 1 2) (iv 3 4)) #t) (check-equal? (rel-meets (iv 1 2) (iv 4 5)) #f)
  (check-equal? (rel-overlaps (iv 1 3) (iv 2 5)) #t) (check-equal? (rel-overlaps (iv 1 2) (iv 3 5)) #f)
  (check-true (cql-null? (rel-overlaps (iv #f #f) (iv 1 2))))
  (check-equal? (rel-includes (iv 1 5) (iv 2 3) #f) #t) (check-equal? (rel-includes (iv 1 5) (iv 1 5) #t) #f)
  (check-equal? (rel-includes (iv 1 5) 1 #t) #f) (check-equal? (rel-includes (iv 1 5) 3 #t) #t)
  (check-equal? (rel-starts (iv 1 2) (iv 1 5)) #t) (check-equal? (rel-ends (iv 3 5) (iv 1 5)) #t)
  (check-equal? (interval-timing 'overlaps-before #f (iv 1 3) (iv 2 5)) #t)
  (check-equal? (interval-timing 'before 'month (iv (d "2024-01-10") (d "2024-01-20")) (iv (d "2024-01-25") (d "2024-02-01"))) #f)
  (check-equal? (interval-timing 'before #f (iv (d "2024-01-10") (d "2024-01-20")) (iv (d "2024-01-25") (d "2024-02-01"))) #t)
  ;; membership
  (check-equal? (point-in-interval 3 (iv 1 5)) #t) (check-equal? (point-in-interval 5 (iv 1 5 #t #f)) #f)
  (check-equal? (point-in-interval 9 (iv 1 #f)) #t) (check-equal? (point-in-interval 1 (iv #f #f)) #f)
  (check-equal? (in-op 2 (list 1 2)) #t) (check-equal? (in-op 2 cql-null-list) #f)
  (check-exn (fails? #rx"In") (lambda () (in-op 2 "x")))
  ;; set operations over lists and intervals
  (check-equal? (set-op (list (list 1 2) (list 2 3)) 'union) '(1 2 3))
  (check-equal? (set-op (list (list 1 2 3) (list 2)) 'except) '(1 3))
  (check-equal? (set-op (list (list 1 2) cql-null) 'except) '(1 2))
  (check-equal? (set-op (list (iv 1 3) (iv 2 5)) 'union) (iv 1 5))
  (check-true (cql-null? (set-op (list (iv 1 2) (iv 4 5)) 'union)))
  (check-equal? (set-op (list (iv 1 3) (iv 2 5)) 'intersect) (iv 2 3))
  (check-equal? (set-op (list (iv 1 5) (iv 3 9)) 'except) (iv 1 2))
  (check-equal? (collapse-intervals (list (iv 4 5) (iv 1 2) (iv 3 3))) (list (iv 1 5)))
  ;; expand: points of an interval, sub-intervals of a list
  (check-equal? (expand-points (iv 1 4) #f) '(1 2 3 4))
  (check-equal? (expand-points (iv 1 5) 2) '(1 3))                                   ; [5,6] does not fit
  (check-equal? (map moment->string (expand-points (iv (d "2024-01") (d "2024-03")) (make-quantity (parse-decimal "1") "month")))
                '("@2024-01" "@2024-02" "@2024-03"))
  (check-equal? (expand-points (iv (d "2024") (d "2025")) (make-quantity (parse-decimal "1") "month")) '())  ; coarser than the step
  (check-equal? (expand-interval (list (iv 1 4)) 2) (list (iv 1 2) (iv 3 4)))
  ;; the timing dispatch: lists first, then null, intervals, points
  (check-equal? (timing-op-eval 'includes #f (list 1 2 3) (list 1 2)) #t)
  (check-equal? (timing-op-eval 'properly-includes #f (list 1 2) (list 1 2)) #f)
  (check-equal? (timing-op-eval 'in #f 2 (iv 1 3)) #t)
  (check-equal? (timing-op-eval 'before #f (d "2024-01") (d "2024-02")) #t)
  (check-true (cql-null? (timing-op-eval 'before #f cql-null (d "2024-02")))))
