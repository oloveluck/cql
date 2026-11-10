#lang racket/base

;;; interp/temporal.rkt — CQL's Date / DateTime / Time semantics over FHIRPath's `moment`:
;;; the constructors (with their domain checks), components, durations and differences (definite when
;;; both operands carry the precision, else an uncertainty Interval[min, max]), precision and the
;;; low/high boundaries, truncation to a precision, point timing, date arithmetic with a calendar
;;; quantity, the clock, and the string conversions.
;;;
;;; A Precision is one of 'year 'month 'day 'hour 'minute 'second 'millisecond (prec-level: 1..6).
;;; A moment's `prec` field is that level (1 = year … 6 = second; a fraction present means ms).

(require racket/date
         (only-in racket/list last)
         (only-in fhirpath/decimal fp-decimal? parse-decimal integer->decimal decimal->exact decimal->string decimal-precision)
         (only-in fhirpath/value fp-long? fp-quantity? fp-quantity-value fp-quantity-unit)
         (only-in fhirpath/temporal-ops
                  moment moment? moment-compare moment->string parse-fp-date parse-fp-datetime parse-fp-time calendar-problem
                  moment-kind moment-year moment-month moment-day moment-hour moment-minute moment-second
                  moment-frac moment-tzmin moment-prec moment-plus moment-add-ms)
         "../value.rkt" "base.rkt")

(provide pad3 ms->frac frac->ms check-range check-date-parts check-time-parts build-date build-time build-datetime component-from jdn abs-ms month-index in-year-key in-month-key abs-ms-time list<? between-op required-level has-precision? between-range unit-ms day-index difference-in duration-in precision-of moment-precision-digits boundary boundary-decimal digit->level digit->ms? boundary-moment days-in-month prec-level truncate-moment point-timing moment-duration coarsen-duration duration-level moment-prec-symbol moment-step to-temporal strip-time-decorations moment->plain-string current-moment moment-total-compare moment-components calendar-unit-name? level->prec-symbol)

(define (current-moment)
  (or (current-evaluation-time) (clock-moment)))
(define (clock-moment)
  (define d (seconds->date (current-seconds) #f))
  (moment 'dateTime (date-year d) (date-month d) (date-day d) (date-hour d) (date-minute d) (date-second d) "000" 0 6))
(define (pad3 n) (let ([s (number->string n)]) (string-append (make-string (max 0 (- 3 (string-length s))) #\0) s)))
(define (ms->frac ms) (and ms (pad3 ms)))
(define (frac->ms f) (inexact->exact (round (* (or (string->number (string-append "0." f)) 0) 1000))))

;; build-date : (or int #f)... -> moment | cql-null   (precision follows the last present component).
;; Components outside their domain (year 1..9999, month 1..12, day 1..31, hour 0..23, minute/second
;; 0..59, ms 0..999) are constructor errors — @T24:00 or DateTime(10000,…) must not build.
(define (check-range what v lo hi) (when (and v (or (< v lo) (> v hi))) (fail (format "~a ~a is out of range" what v))))
(define (check-date-parts y mo d)
  (check-range "year" y 1 9999) (check-range "month" mo 1 12) (check-range "day" d 1 31)
  ;; the calendar, not only 1..31: February 30th names no day (leap years by 4/100/400)
  (when (and y mo d (> d (days-in-month y mo)))
    (fail (format "day ~a is out of range (~a-~a has ~a days)" d y (if (< mo 10) (format "0~a" mo) mo)
                  (days-in-month y mo)))))
(define (check-time-parts h mi s ms)
  (check-range "hour" h 0 23) (check-range "minute" mi 0 59) (check-range "second" s 0 59) (check-range "millisecond" ms 0 999))
(define (build-date y mo d)
  (cond [(not y) cql-null]
        [else (check-date-parts y mo d)
              (moment 'date y mo d #f #f #f #f #f (cond [d 3] [mo 2] [else 1]))]))
(define (build-time h mi s ms)
  (cond [(not h) cql-null]
        [else (check-time-parts h mi s ms)
              (moment 'time #f #f #f h mi s (ms->frac ms) #f (cond [s 6] [mi 5] [else 4]))]))
(define (build-datetime y mo d h mi s ms tz-hours)
  (cond [(not y) cql-null]
        [else (check-date-parts y mo d) (check-time-parts h mi s ms)
              (moment 'dateTime y mo d h mi s (ms->frac ms)
                      (and tz-hours (inexact->exact (round (* tz-hours 60))))
                      (cond [s 6] [mi 5] [h 4] [d 3] [mo 2] [else 1]))]))

;; component-from : Symbol CqlValue -> CqlValue   extract a datetime component / timezone / date / time
(define (component-from prec m)
  (cond
    [(not (moment? m)) cql-null]
    [else (case prec
            [(year) (or (moment-year m) cql-null)] [(month) (or (moment-month m) cql-null)]
            [(day) (or (moment-day m) cql-null)] [(hour) (or (moment-hour m) cql-null)]
            [(minute) (or (moment-minute m) cql-null)] [(second) (or (moment-second m) cql-null)]
            [(millisecond) (let ([f (moment-frac m)]) (if f (frac->ms f) cql-null))]
            [(timezoneoffset timezone) (let ([tz (moment-tzmin m)])
                                         (if tz (or (parse-decimal (number->string (exact->inexact (/ tz 60)))) cql-null) cql-null))]
            [(date) (if (eq? (moment-kind m) 'time) cql-null
                        (moment 'date (moment-year m) (moment-month m) (moment-day m) #f #f #f #f #f (min 3 (moment-prec m))))]
            [(time) (if (moment-hour m)
                        (moment 'time #f #f #f (moment-hour m) (moment-minute m) (moment-second m) (moment-frac m) #f
                                (cond [(moment-second m) 6] [(moment-minute m) 5] [else 4]))
                        cql-null)]
            [else cql-null])]))

;; proleptic Gregorian day number
(define (jdn y m d)
  (let* ([a (quotient (- 14 m) 12)] [yy (- (+ y 4800) a)] [mm (+ m (* 12 a) -3)])
    (+ d (quotient (+ (* 153 mm) 2) 5) (* 365 yy) (quotient yy 4)
       (- (quotient yy 100)) (quotient yy 400) -32045)))

;; abs-ms : moment -> exact-integer   absolute UTC milliseconds (absent components default low)
(define (abs-ms m)
  (define base (+ (* 86400000 (jdn (or (moment-year m) 0) (or (moment-month m) 1) (or (moment-day m) 1)))
                  (* 3600000 (or (moment-hour m) 0)) (* 60000 (or (moment-minute m) 0))
                  (* 1000 (or (moment-second m) 0)) (if (moment-frac m) (frac->ms (moment-frac m)) 0)))
  (if (moment-tzmin m) (- base (* 60000 (moment-tzmin m))) base))
(define (month-index m) (+ (* (or (moment-year m) 0) 12) (- (or (moment-month m) 1) 1)))

;; within-year/within-month keys, for the "complete period" adjustment of duration year/month
(define (in-year-key m) (list (or (moment-month m) 1) (or (moment-day m) 1) (abs-ms-time m)))
(define (in-month-key m) (list (or (moment-day m) 1) (abs-ms-time m)))
(define (abs-ms-time m) (+ (* 3600000 (or (moment-hour m) 0)) (* 60000 (or (moment-minute m) 0))
                           (* 1000 (or (moment-second m) 0)) (if (moment-frac m) (frac->ms (moment-frac m)) 0)))
(define (list<? a b) (cond [(or (null? a) (null? b)) #f] [(< (car a) (car b)) #t] [(> (car a) (car b)) #f] [else (list<? (cdr a) (cdr b))]))

;; between-op : ('duration|'difference) Symbol CqlValue CqlValue -> CqlValue
;; When both operands carry the precision the unit needs, the result is an exact integer; otherwise the
;; missing components make it indeterminate and we return a cql-uncertainty [min, max].
(define (between-op kind prec a b)
  (cond
    [(or (cql-null? a) (cql-null? b)) cql-null]
    [(not (and (moment? a) (moment? b))) cql-null]
    [(and (has-precision? a prec) (has-precision? b prec))
     (if (eq? kind 'difference) (difference-in prec a b) (duration-in prec a b))]
    [else (between-range kind prec a b)]))

;; a duration/difference is definite only when both operands carry enough precision: DAY for
;; year/month/week/day (the sub-day position can flip the count), and the unit itself for time units.
(define (required-level unit) (case unit [(hour) 4] [(minute) 5] [(second millisecond) 6] [else 3]))
(define (has-precision? m unit) (>= (moment-prec m) (required-level unit)))

;; the range of a duration/difference when operands are partial: fill each to its low/high boundary and
;; take the extremes over the combinations (diff grows as B rises and A falls)
(define (between-range kind prec a b)
  (define f (if (eq? kind 'difference) difference-in duration-in))
  ;; fill BOTH operands to their full millisecond low/high boundaries and take the extremes over the
  ;; four corners — the reference's uncertainty model (a partial date's true instant may lie anywhere
  ;; inside it, so the minimal duration pairs a's latest instant with b's earliest)
  (define al (boundary-moment a #f #t)) (define ah (boundary-moment a #f #f))
  (define bl (boundary-moment b #f #t)) (define bh (boundary-moment b #f #f))
  (define vs (list (f prec al bl) (f prec ah bh) (f prec al bh) (f prec ah bl)))
  (define lo (apply min vs)) (define hi (apply max vs))
  (if (= lo hi) lo (cql-interval lo hi #t #t)))    ; an uncertain result is an Interval[min, max]
(define (unit-ms u) (case u [(day) 86400000] [(hour) 3600000] [(minute) 60000] [(second) 1000] [(millisecond) 1] [else #f]))
(define (day-index m) (floor (/ (abs-ms m) 86400000)))     ; UTC-normalized calendar day number

;; difference: count of unit boundaries crossed
(define (difference-in prec a b)
  (case prec
    [(year) (- (or (moment-year b) 0) (or (moment-year a) 0))]
    [(month) (- (month-index b) (month-index a))]
    [(week) (truncate (/ (- (day-index b) (day-index a)) 7))]
    [(day) (- (day-index b) (day-index a))]
    [else (let ([u (unit-ms prec)])
            (if u (- (floor (/ (abs-ms b) u)) (floor (/ (abs-ms a) u))) cql-null))]))

;; duration: number of complete periods from a to b (year/month adjusted for the sub-unit position;
;; day/week/time as complete elapsed periods, truncated toward zero)
(define (duration-in prec a b)
  (case prec
    [(year) (let ([d (- (or (moment-year b) 0) (or (moment-year a) 0))])
              (if (list<? (in-year-key b) (in-year-key a)) (sub1 d) d))]
    [(month) (let ([d (- (month-index b) (month-index a))])
               (if (list<? (in-month-key b) (in-month-key a)) (sub1 d) d))]
    [else (let ([u (or (unit-ms prec) (and (eq? prec 'week) 604800000))])
            (if u (truncate (/ (- (abs-ms b) (abs-ms a)) u)) cql-null))]))

;; Precision : Decimal -> its scale ; temporal -> its component digit-count (year=4 … ms=17; time hh=2…ms=9)
(define (precision-of x)
  (cond [(cql-null? x) cql-null]
        [(fp-decimal? x) (decimal-precision x)]
        [(moment? x) (moment-precision-digits x)]
        [else cql-null]))
(define (moment-precision-digits m)
  (if (eq? (moment-kind m) 'time)
      (case (moment-prec m) [(4) 2] [(5) 4] [(6) (if (moment-frac m) 9 6)] [else 2])
      (case (moment-prec m) [(1) 4] [(2) 6] [(3) 8] [(4) 10] [(5) 12] [(6) (if (moment-frac m) 17 14)] [else 4])))

;; LowBoundary/HighBoundary : fill a value out to a precision, taking the min (low) or max (high) of the
;; unspecified places. Decimals pad the fraction with 0/9 (default precision 8); temporals fill components.
(define (boundary x p low?)
  (cond [(cql-null? x) cql-null]
        [(fp-decimal? x) (boundary-decimal x (if (cql-null? p) 8 (->int-val p)) low?)]
        [(and (num? x)) (boundary-decimal (->dec x) (if (cql-null? p) 8 (->int-val p)) low?)]
        [(moment? x) (boundary-moment x (if (cql-null? p) #f (->int-val p)) low?)]
        [else cql-null]))
(define (boundary-decimal d p low?)
  (define s (decimal->string d))
  (define parts (regexp-match #px"^(-?[0-9]+)(?:\\.([0-9]*))?$" s))
  (cond [(not parts) cql-null]
        [else (define ip (cadr parts)) (define fp (or (caddr parts) "")) (define cur (string-length fp))
              (define fp* (if (>= cur p) (substring fp 0 p) (string-append fp (make-string (- p cur) (if low? #\0 #\9)))))
              (or (parse-decimal (if (<= p 0) ip (string-append ip "." fp*))) cql-null)]))
(define (digit->level digits time?)
  (if time? (case digits [(2) 4] [(4) 5] [(6 9) 6] [else 6]) (case digits [(4) 1] [(6) 2] [(8) 3] [(10) 4] [(12) 5] [(14 17) 6] [else 6])))
(define (digit->ms? digits time?) (if time? (= digits 9) (= digits 17)))
(define (boundary-moment m digits low?)
  (define time? (eq? (moment-kind m) 'time))
  (define L (if digits (digit->level digits time?) 6))
  (define ms? (if digits (digit->ms? digits time?) #t))
  (define (fill present lo hi) (or present (if low? lo hi)))
  (define y (moment-year m))
  (define mo (and (>= L 2) (fill (moment-month m) 1 12)))
  (define d (and (>= L 3) (fill (moment-day m) 1 (if low? 1 (days-in-month (or y 2000) (or mo 12))))))
  (define h (and (>= L 4) (fill (moment-hour m) 0 23)))
  (define mi (and (>= L 5) (fill (moment-minute m) 0 59)))
  (define s (and (>= L 6) (fill (moment-second m) 0 59)))
  (define fr (and ms? (or (moment-frac m) (if low? "000" "999"))))
  (moment (moment-kind m) y (if time? (moment-month m) mo) (if time? (moment-day m) d) h mi s fr (moment-tzmin m) L))
(define (days-in-month y m)
  (define ny (if (= m 12) (add1 y) y)) (define nm (if (= m 12) 1 (add1 m)))
  (- (jdn ny nm 1) (jdn y m 1)))
(define prec-level (hash 'year 1 'month 2 'day 3 'hour 4 'minute 5 'second 6 'millisecond 6))

;; truncate-moment : moment Symbol -> moment   drop components finer than the precision
(define (truncate-moment m prec)
  (define lvl (hash-ref prec-level prec 6))
  (moment (moment-kind m) (moment-year m) (and (>= lvl 2) (moment-month m)) (and (>= lvl 3) (moment-day m))
          (and (>= lvl 4) (moment-hour m)) (and (>= lvl 5) (moment-minute m)) (and (>= lvl 6) (moment-second m))
          (if (eq? prec 'millisecond) (moment-frac m) #f) (moment-tzmin m) lvl))  ; frac kept only at ms precision

;; point vs point timing (before/after/same, precision-qualified)
(define (point-timing op prec a b)
  (define c (if prec (moment-compare (truncate-moment a prec) (truncate-moment b prec)) (moment-compare a b)))
  (cond [(eq? c 'empty) cql-null]
        [else (case op
                [(before) (< c 0)] [(after) (> c 0)] [(same-as) (= c 0)]
                [(same-or-before) (<= c 0)] [(same-or-after) (>= c 0)] [else cql-null])]))

;; stepping a moment past its type's range (year 1..9999; a Time wrap past midnight) -> #f, so
;; interval internals degrade gracefully; the Predecessor/Successor OPERATORS turn #f into an error.
(define (moment-step m dir)
  (define r (if (moment-frac m) (moment-add-ms m dir)
                (moment-plus m dir (case (moment-prec m) [(1) "year"] [(2) "month"] [(3) "day"] [(4) "hour"] [(5) "minute"] [else "second"]))))
  (cond [(and (eq? (moment-kind m) 'time)
              (let ([c (moment-compare r m)]) (and (number? c) (not (= (sgn c) dir)))))
         #f]                                                  ; wrapped around midnight
        [(let ([y (moment-year r)]) (and y (or (< y 1) (> y 9999)))) #f]
        [else r]))
(define (calendar-unit-name? u)
  (member u '("year" "years" "month" "months" "week" "weeks" "day" "days"
              "hour" "hours" "minute" "minutes" "second" "seconds" "millisecond" "milliseconds")))
(define (level->prec-symbol lvl) (case lvl [(1) 'year] [(2) 'month] [(3) 'day] [(4) 'hour] [(5) 'minute] [else 'second]))

;; add/subtract a duration to a moment. Time units carry through milliseconds (moment-plus quantizes
;; ms to seconds, losing sub-second), so use moment-add-ms and truncate back to the operand's precision.
;; year over/underflow (outside 0001..9999) is invalid.
(define (moment-duration m q sign)
  (unless (fp-quantity? q) (fail "date/time arithmetic requires a quantity"))
  (define n0 (* sign (round-to-int (fp-quantity-value q))))
  (define u0 (singular-unit (fp-quantity-unit q)))
  ;; a duration finer than the moment's precision is converted (truncating) to the moment's precision,
  ;; per spec: DateTime(2014) + 730 days = @2016 ; Date(2014,6) - 33 days = @2014-05
  (define-values (n u) (coarsen-duration n0 u0 (moment-prec m)))
  (define orig (moment-prec-symbol m))
  (define r (case u
              [("hour") (truncate-moment (moment-add-ms m (* n 3600000)) orig)]
              [("minute") (truncate-moment (moment-add-ms m (* n 60000)) orig)]
              [("second") (truncate-moment (moment-add-ms m (* n 1000)) orig)]
              [("millisecond") (truncate-moment (moment-add-ms m n) orig)]
              [else (moment-plus m n u)]))
  (define y (moment-year r))
  (if (and y (or (< y 1) (> y 9999))) (fail "date/time arithmetic out of range") r))

;; coarsen-duration : n unit moment-prec -> (values n' unit')   convert a finer-than-precision
;; duration up to the moment's precision, truncating toward zero (12 months->1 year, 730 days->2 years)
(define duration-level (hash "year" 1 "month" 2 "week" 3 "day" 3 "hour" 4 "minute" 5 "second" 6 "millisecond" 6))
(define (coarsen-duration n u prec)
  (define lvl (hash-ref duration-level u 6))
  (cond
    [(<= lvl prec) (values n u)]
    [else
     ;; go through milliseconds for time-chain, days for the date-chain
     (define ms (case u [("week") (* n 604800000)] [("day") (* n 86400000)] [("hour") (* n 3600000)]
                  [("minute") (* n 60000)] [("second") (* n 1000)] [("millisecond") n] [("month") #f] [else #f]))
     (case prec
       [(1) (values (cond [(string=? u "month") (truncate (/ n 12))] [ms (truncate (/ ms 31536000000))] [else n])
                    "year")]
       [(2) (values (cond [ms (truncate (/ ms 2592000000))] [else n]) "month")]
       [(3) (values (if ms (truncate (/ ms 86400000)) n) "day")]
       [(4) (values (if ms (truncate (/ ms 3600000)) n) "hour")]
       [(5) (values (if ms (truncate (/ ms 60000)) n) "minute")]
       [else (values n u)])]))
(define (moment-prec-symbol m)
  (cond [(moment-frac m) 'millisecond]
        [else (case (moment-prec m) [(1) 'year] [(2) 'month] [(3) 'day] [(4) 'hour] [(5) 'minute] [else 'second])]))

;; the ToString rendering drops the CQL literal prefixes (@ / @T): 09:30:01.003, 2000-01-01T…
(define (moment->plain-string m)
  (regexp-replace #px"^@T?" (moment->string m) ""))
(define (moment-total-compare a b)
  (let loop ([xs (moment-components a)] [ys (moment-components b)])
    (cond [(null? xs) 0]
          [else (define x (car xs)) (define y (car ys))
                (cond [(and (not x) (not y)) (loop (cdr xs) (cdr ys))]
                      [(not x) -1] [(not y) 1]                 ; absent sorts first
                      [(< x y) -1] [(> x y) 1]
                      [else (loop (cdr xs) (cdr ys))])])))
(define (moment-components m)
  (list (moment-year m) (moment-month m) (moment-day m) (moment-hour m) (moment-minute m) (moment-second m)))

;; to-temporal : CqlValue ('date|'datetime|'time) -> moment|Null   parse a string / pass a moment
;; through. Time strings may carry a leading T and a timezone suffix (ignored for a Time value).
(define (to-temporal x kind)
  (cond [(cql-null? x) cql-null]
        [(moment? x) x]
        [(string? x) (case kind
                       [(date) (on-calendar (parse-fp-date x))]
                       [(datetime) (on-calendar (parse-fp-datetime x))]
                       [(time) (or (parse-fp-time (strip-time-decorations x)) cql-null)])]
        [else cql-null]))
;; on-calendar : moment|#f -> moment | cql-null   a string naming a day its month lacks converts to
;; null, as any string that is not a date does ('1940-02-30')
(define (on-calendar m) (if (and m (not (calendar-problem m))) m cql-null))
(define (strip-time-decorations s)
  (regexp-replace #px"(Z|[+-][0-9]{2}:[0-9]{2})$" (regexp-replace #px"^T" s "") ""))


;; ---- living examples ----------------------------------------------------------------------------
(module+ test
  (require rackunit (only-in fhirpath/decimal parse-decimal decimal->string)
           (only-in fhirpath/quantity make-quantity) (only-in "../error.rkt" cql-error? cql-error-message))
  ;; fails? : regexp -> (Any -> Boolean)   a cql-error whose message matches (the interpreter raises a value, not an exn)
  (define ((fails? rx) e) (and (cql-error? e) (regexp-match? rx (cql-error-message e))))
  (define (d s) (parse-fp-date s)) (define (dt s) (parse-fp-datetime s)) (define (t s) (parse-fp-time s))
  (define (str m) (moment->string m))
  ;; constructors: precision follows the last component; domains are checked
  (check-equal? (str (build-date 2024 3 #f)) "@2024-03")
  (check-equal? (str (build-datetime 2024 3 5 10 30 #f #f #f)) "@2024-03-05T10:30")
  (check-equal? (str (build-datetime 2024 3 5 10 30 15 250 -5)) "@2024-03-05T10:30:15.250-05:00")
  (check-equal? (str (build-time 10 30 #f #f)) "@T10:30")
  (check-true (cql-null? (build-date #f #f #f)))
  ;; the calendar: February 30th is no day — a constructor error, a conversion to null
  (check-exn (fails? #rx"1940-02 has 29 days") (lambda () (build-date 1940 2 30)))
  (check-equal? (str (build-date 2000 2 29)) "@2000-02-29")
  (check-exn (fails? #rx"1900-02 has 28 days") (lambda () (build-datetime 1900 2 29 #f #f #f #f #f)))
  (check-true (cql-null? (to-temporal "1940-02-30" 'date)))
  (check-true (cql-null? (to-temporal "1940-04-31T10:00:00" 'datetime)))
  (check-exn (fails? #rx"month 13 is out of range") (lambda () (build-date 2024 13 1)))
  (check-exn (fails? #rx"hour 24") (lambda () (build-time 24 0 #f #f)))
  ;; components
  (check-equal? (component-from 'month (d "2024-03-05")) 3)
  (check-true (cql-null? (component-from 'day (d "2024-03"))))
  (check-equal? (component-from 'millisecond (dt "2024-03-05T10:30:15.250")) 250)
  (check-equal? (str (component-from 'date (dt "2024-03-05T10:30"))) "@2024-03-05")
  (check-equal? (str (component-from 'time (dt "2024-03-05T10:30"))) "@T10:30")
  ;; duration and difference: definite with enough precision, an Interval[min, max] without
  (check-equal? (between-op 'duration 'year (d "2000-06-15") (d "2024-06-14")) 23)
  (check-equal? (between-op 'difference 'year (d "2000-06-15") (d "2024-06-14")) 24)
  (check-equal? (between-op 'duration 'month (d "2024-01-31") (d "2024-03-01")) 1)
  (check-equal? (between-op 'duration 'day (d "2024-02-27") (d "2024-03-02")) 4)        ; leap year
  (check-equal? (between-op 'difference 'hour (dt "2024-03-05T10:59") (dt "2024-03-05T11:01")) 1)
  (check-equal? (between-op 'duration 'hour (dt "2024-03-05T10:59") (dt "2024-03-05T11:01")) 0)
  (check-equal? (between-op 'duration 'day (d "2024-01") (d "2024-03")) (cql-interval 29 90 #t #t))
  (check-true (cql-null? (between-op 'duration 'day cql-null (d "2024-03"))))
  (check-equal? (jdn 2000 1 1) 2451545)                                               ; J2000
  (check-equal? (days-in-month 2024 2) 29)
  ;; precision, boundaries, truncation
  (check-equal? (precision-of (parse-decimal "1.250")) 3)
  (check-equal? (precision-of (d "2024-03")) 6)
  (check-equal? (precision-of (dt "2024-03-05T10:30:15.250")) 17)
  (check-equal? (decimal->string (boundary (parse-decimal "1.5") 2 #t)) "1.50")
  (check-equal? (decimal->string (boundary (parse-decimal "1.5") 2 #f)) "1.59")
  (check-equal? (str (boundary (d "2024-02") 8 #t)) "@2024-02-01")
  (check-equal? (str (boundary (d "2024-02") 8 #f)) "@2024-02-29")
  (check-equal? (str (boundary (d "2024-02") cql-null #f)) "@2024-02-29T23:59:59.999")
  (check-equal? (str (truncate-moment (dt "2024-03-05T10:30:15") 'minute)) "@2024-03-05T10:30")
  (check-equal? (point-timing 'before 'hour (dt "2024-03-05T10:30") (dt "2024-03-05T10:45")) #f)
  (check-equal? (point-timing 'before #f (dt "2024-03-05T10:30") (dt "2024-03-05T10:45")) #t)
  ;; date arithmetic: a finer duration is coarsened to the moment's precision; out of range errors
  (check-equal? (str (moment-duration (d "2024-01-31") (make-quantity (parse-decimal "1") "month") 1)) "@2024-02-29")
  (check-equal? (str (moment-duration (d "2014") (make-quantity (parse-decimal "730") "days") 1)) "@2016")
  (check-equal? (str (moment-duration (dt "2024-03-05T10:30") (make-quantity (parse-decimal "90") "minutes") -1)) "@2024-03-05T09:00")
  (check-exn (fails? #rx"out of range") (lambda () (moment-duration (d "9999") (make-quantity (parse-decimal "1") "year") 1)))
  (let-values ([(n u) (coarsen-duration 14 "month" 1)]) (check-equal? (list n u) '(1 "year")))
  ;; stepping by the moment's own precision; a Time never wraps midnight
  (check-equal? (str (moment-step (d "2024-02") 1)) "@2024-03")
  (check-false (moment-step (t "23:59:59") 1))
  ;; conversions
  (check-equal? (str (to-temporal "2024-03-05" 'date)) "@2024-03-05")
  (check-equal? (str (to-temporal "T10:30:00+01:00" 'time)) "@T10:30:00")
  (check-true (cql-null? (to-temporal "nope" 'date)))
  (check-equal? (moment->plain-string (t "10:30")) "10:30")
  (check-equal? (moment-total-compare (d "2012-10-05") (dt "2012-10-05T10")) -1)        ; absent sorts first
  (check-true (moment? (current-moment))))
