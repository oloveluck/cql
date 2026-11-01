#lang racket/base

;;; interp.rkt — the ELM interpreter (ELM node -> CQL value).
;;;
;;; CQL is compiled to ELM and then RUN by walking the ELM tree — never evaluated from CQL directly.
;;; This is the ELM analog of fhirpath/eval.rkt's fp-eval: a `match` over the prefab ELM structs,
;;; dispatching binary/n-ary/unary operators on their ELM type string. It reuses the FHIRPath System
;;; primitives (decimal, quantity, temporal, equality) and adds CQL's three-valued null handling.
;;; This interpreter is what drives the conformance harness (compile a CQL expression -> ELM -> run ->
;;; compare to the expected value).

(require racket/match
         racket/contract
         (only-in racket/list index-of remove-duplicates take drop last append-map argmax)
         (only-in racket/string string-split)
         (only-in racket/math nan? infinite?)
         "elm/ast.rkt"
         "value.rkt"
         (only-in "error.rkt" cql-error cql-error?)
         (only-in fhir/result/result ok err)
         (only-in fhir/fhirpath/decimal
                  fp-decimal? parse-decimal integer->decimal decimal->exact decimal->string
                  decimal-add decimal-sub decimal-mul decimal-div decimal-cmp decimal-round decimal-negate decimal-precision)
         (only-in fhir/fhirpath/value fp-long fp-long? fp-long-v fp-quantity fp-quantity? fp-quantity-value fp-quantity-unit)
         (only-in fhir/fhirpath/quantity make-quantity quantity-add quantity-sub quantity-compare)
         (only-in fhir/fhirpath/temporal-ops
                  moment moment? moment-compare moment->string parse-fp-date parse-fp-datetime parse-fp-time
                  moment-kind moment-year moment-month moment-day moment-hour moment-minute moment-second
                  moment-frac moment-tzmin moment-prec moment-plus moment-add-ms)
         (only-in fhir/fhirpath/equality item-equal? item-equiv?))

(provide (contract-out
          [eval-elm    (-> elm-expr? ienv? cql-value?)]  ; run one ELM expression node -> a CQL value
          [run-library (-> elm-library? hash?)]     ; def-name(string) -> interpreted CqlValue
          [make-ienv   (->* () (#:defs hash? #:operands hash?) ienv?)]
          [ienv?       (-> any/c boolean?)]
          [test-equal? (-> any/c any/c boolean?)]))   ; value comparison for the conformance harness

;; ---- interpreter environment ----------------------------------------------
;; defs: name(string) -> promise-of-value ; operands: name(string) -> value ; funcs: name -> elm-function-def ;
;; now: a single evaluation-time moment shared by Now()/Today()/TimeOfDay() so they're self-consistent
(struct ienv (defs operands funcs now) #:transparent)
(define (make-ienv #:defs [defs (hash)] #:operands [operands (hash)]) (ienv defs operands (hash) (current-moment)))
(define (current-moment)
  (define d (seconds->date (current-seconds) #f))
  (moment 'dateTime (date-year d) (date-month d) (date-day d) (date-hour d) (date-minute d) (date-second d) "000" 0 6))

;; ---- library driver --------------------------------------------------------
(define (run-library lib)
  (define funcs (for/hash ([s (in-list (elm-library-statements lib))] #:when (elm-function-def? s))
                  (values (elm-function-def-name s) s)))
  ;; expression defs are memoized promises so cross-references evaluate once and can forward-refer
  (define def-boxes (make-hash))
  (define now (current-moment))
  (define base (ienv (make-hash) (hash) funcs now))
  (define (env) (ienv (ienv-defs base) (ienv-operands base) funcs now))
  (for ([s (in-list (elm-library-statements lib))] #:when (elm-def? s))
    (hash-set! (ienv-defs base) (elm-def-name s)
               (delay-value (lambda () (eval-elm (elm-def-expression s) (env))))))
  (for/hash ([s (in-list (elm-library-statements lib))] #:when (elm-def? s))
    (values (elm-def-name s) (force-value (hash-ref (ienv-defs base) (elm-def-name s))))))

;; a tiny memoizing thunk (avoids racket/promise's contract friction with our value domain)
(define (delay-value th) (let ([done #f] [v #f]) (lambda () (unless done (set! v (th)) (set! done #t)) v)))
(define (force-value p) (if (procedure? p) (p) p))

;; ---- the evaluator ---------------------------------------------------------
;; An ELM-Expr is one of (see cql/elm/ast.rkt): literal | null | quantity | ratio | nary | unary |
;; interval | list | tuple | if | case | property | expression-ref | operand-ref | function-ref |
;; is | as | to. `eval-elm` is the structural-recursion template over that data definition: one match
;; clause per variant, recurring on sub-expressions; operator variants (nary/unary) delegate to the
;; operator registry via apply-op.
;; eval-elm : ELM-Expr Env -> CqlValue
(define (eval-elm node env)
  (match node
    [(elm-literal vt v) (eval-literal vt v)]
    [(elm-null _) cql-null]
    [(elm-quantity v unit) (make-quantity (or (parse-decimal v) (integer->decimal 0)) (or unit "1"))]
    [(elm-ratio n d) (cql-ratio (eval-elm n env) (eval-elm d env))]
    [(elm-date y mo d) (build-date (ci y env) (ci mo env) (ci d env))]
    [(elm-time h mi s ms) (build-time (ci h env) (ci mi env) (ci s env) (ci ms env))]
    [(elm-datetime y mo d h mi s ms tz)
     (build-datetime (ci y env) (ci mo env) (ci d env) (ci h env) (ci mi env) (ci s env) (ci ms env) (cnum tz env))]
    [(elm-component prec operand) (component-from prec (eval-elm operand env))]
    [(elm-between kind prec a b) (between-op kind prec (eval-elm a env) (eval-elm b env))]
    [(elm-timing op prec a b) (timing-op-eval op prec (eval-elm a env) (eval-elm b env))]
    [(elm-query sources lets where ret ret-distinct sort agg) (run-query sources lets where ret ret-distinct sort agg env)]
    [(elm-instance class-type elems) (build-instance class-type (for/list ([te (in-list elems)])
                                                                  (cons (elm-tuple-elem-name te)
                                                                        (eval-elm (elm-tuple-elem-value te) env))))]
    [(elm-nary op operands _rt) (apply-op op operands env)]
    [(elm-unary op operand _rt) (apply-op op (list operand) env)]
    [(elm-interval low high lc hc)
     (make-interval-value (and low (eval-elm low env)) (and high (eval-elm high env)) lc hc)]
    [(elm-list elems) (map (lambda (e) (eval-elm e env)) elems)]
    [(elm-tuple elems)
     (cql-tuple (for/hash ([te (in-list elems)])
                  (values (elm-tuple-elem-name te) (eval-elm (elm-tuple-elem-value te) env))))]
    [(elm-if c t e) (eval-if c t e env)]
    [(elm-case comparand items els) (eval-case comparand items els env)]
    [(elm-property src path) (eval-property (eval-elm src env) path)]
    [(elm-expression-ref name _lib) (force-value (hash-ref (ienv-defs env) name (lambda () (fail (format "unknown reference ~a" name)))))]
    [(elm-operand-ref name) (hash-ref (ienv-operands env) name (lambda () (fail (format "unbound operand ~a" name))))]
    [(elm-function-ref name _lib operands) (eval-function name (map (lambda (o) (eval-elm o env)) operands) env)]
    [(elm-is operand type) (eval-is (eval-elm operand env) type)]
    [(elm-as operand type _strict)
     ;; As is a pass-through, EXCEPT `null as List<T>` which becomes the LIST-TYPED null: Length sees
     ;; it as empty (0) and scalar membership is false, while list-list inclusion stays null
     (define v (eval-elm operand env))
     (if (and (cql-null? v) (elm-list-type? type)) cql-null-list v)]
    [(elm-to operand to-type) (convert-to (eval-elm operand env) to-type)]
    [_ (fail (format "cannot evaluate ELM node: ~s" node))]))

(define (fail msg) (raise (cql-error 'interpret msg)))

;; ---- literals --------------------------------------------------------------
(define (eval-literal vt v)
  (case vt
    [("Integer") (string->number v)]
    [("Long") (fp-long (string->number v))]
    [("Decimal") (or (parse-decimal v) (fail (format "bad decimal ~a" v)))]
    [("String") v]
    [("Boolean") (string=? v "true")]
    [("Date") (or (parse-fp-date v) (fail (format "bad date ~a" v)))]
    [("DateTime") (or (parse-fp-datetime v) (fail (format "bad dateTime ~a" v)))]
    [("Time") (or (parse-fp-time v) (fail (format "bad time ~a" v)))]
    [else (fail (format "unsupported literal type ~a" vt))]))

;; ---- temporal constructors / components ------------------------------------
;; ci : (or ELM #f) Env -> (or exact-integer #f)   evaluate a component operand to an integer, or #f
(define (ci x env) (and x (let ([v (eval-elm x env)]) (if (cql-null? v) #f (->int-val v)))))
;; cnum : (or ELM #f) Env -> (or real #f)   evaluate a numeric operand (e.g. a tz-offset in hours)
(define (cnum x env) (and x (let ([v (eval-elm x env)]) (if (cql-null? v) #f (num->real v)))))
(define (pad3 n) (let ([s (number->string n)]) (string-append (make-string (max 0 (- 3 (string-length s))) #\0) s)))
(define (ms->frac ms) (and ms (pad3 ms)))
(define (frac->ms f) (inexact->exact (round (* (or (string->number (string-append "0." f)) 0) 1000))))
;; build-date : (or int #f)... -> moment | cql-null   (precision follows the last present component).
;; Components outside their domain (year 1..9999, month 1..12, day 1..31, hour 0..23, minute/second
;; 0..59, ms 0..999) are constructor errors — @T24:00 or DateTime(10000,…) must not build.
(define (check-range what v lo hi) (when (and v (or (< v lo) (> v hi))) (fail (format "~a ~a is out of range" what v))))
(define (check-date-parts y mo d) (check-range "year" y 1 9999) (check-range "month" mo 1 12) (check-range "day" d 1 31))
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
            [(time) (if (moment-hour m) (moment 'time #f #f #f (moment-hour m) (moment-minute m) (moment-second m) (moment-frac m) #f 6) cql-null)]
            [else cql-null])]))

;; ---- duration / difference between temporals --------------------------------
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

;; ---- precision / boundary --------------------------------------------------
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

;; ---- timing comparison (point vs point; precision-qualified) ----------------
(define prec-level (hash 'year 1 'month 2 'day 3 'hour 4 'minute 5 'second 6 'millisecond 6))
;; truncate-moment : moment Symbol -> moment   drop components finer than the precision
(define (truncate-moment m prec)
  (define lvl (hash-ref prec-level prec 6))
  (moment (moment-kind m) (moment-year m) (and (>= lvl 2) (moment-month m)) (and (>= lvl 3) (moment-day m))
          (and (>= lvl 4) (moment-hour m)) (and (>= lvl 5) (moment-minute m)) (and (>= lvl 6) (moment-second m))
          (if (eq? prec 'millisecond) (moment-frac m) #f) (moment-tzmin m) lvl))  ; frac kept only at ms precision
;; timing-op-eval : Symbol (Symbol|#f) CqlValue CqlValue -> Boolean|Null
(define (timing-op-eval op prec a b)
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

;; point vs point timing (before/after/same, precision-qualified)
(define (point-timing op prec a b)
  (define c (if prec (moment-compare (truncate-moment a prec) (truncate-moment b prec)) (moment-compare a b)))
  (cond [(eq? c 'empty) cql-null]
        [else (case op
                [(before) (< c 0)] [(after) (> c 0)] [(same-as) (= c 0)]
                [(same-or-before) (<= c 0)] [(same-or-after) (>= c 0)] [else cql-null])]))

;; ---- intervals -------------------------------------------------------------
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

;; build-instance : String (Listof (cons Symbol CqlValue)) -> CqlValue
;; the known System class selectors build their carrier; anything else is a structured tuple
(define (build-instance class-type fields)
  (define (f k) (cond [(assq k fields) => cdr] [else cql-null]))
  (define (fs k) (let ([v (f k)]) (if (string? v) v #f)))
  (case class-type
    [("Code" "System.Code") (cql-code (fs 'code) (fs 'system) (fs 'version) (fs 'display))]
    [("Concept" "System.Concept")
     (cql-concept (let ([c (f 'codes)]) (cond [(list? c) c] [(cql-code? c) (list c)] [else '()])) (fs 'display))]
    [("Quantity" "System.Quantity")
     (let ([v (f 'value)]) (if (num? v) (fp-quantity (->dec v) (or (fs 'unit) "1") #f) cql-null))]
    [else (cql-obj class-type (for/hash ([kv (in-list fields)]) (values (car kv) (cdr kv))))]))

;; singleton from : the sole element of a list (empty -> null, >1 -> error)
(define (singleton-from x)
  (cond [(cql-null? x) cql-null] [(not (list? x)) cql-null]
        [(null? x) cql-null] [(null? (cdr x)) (car x)] [else (fail "List contains more than one element")]))

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
(define (calendar-unit-name? u)
  (member u '("year" "years" "month" "months" "week" "weeks" "day" "days"
              "hour" "hours" "minute" "minutes" "second" "seconds" "millisecond" "milliseconds")))

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
(define (level->prec-symbol lvl) (case lvl [(1) 'year] [(2) 'month] [(3) 'day] [(4) 'hour] [(5) 'minute] [else 'second]))

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

;; ---- numeric helpers -------------------------------------------------------
(define (num? x) (or (exact-integer? x) (fp-long? x) (fp-decimal? x)))
(define (int-like? x) (or (exact-integer? x) (fp-long? x)))
(define (->int x) (if (fp-long? x) (fp-long-v x) x))
(define (->dec x) (cond [(fp-decimal? x) x] [(fp-long? x) (integer->decimal (fp-long-v x))] [(exact-integer? x) (integer->decimal x)] [else (fail "not numeric")]))
(define (any-long? a b) (or (fp-long? a) (fp-long? b)))
;; Integer results that overflow the 32-bit range become null (CQL Integer is 32-bit); Longs are unbounded here
(define INT-MIN -2147483648) (define INT-MAX 2147483647)
(define (int-guard n) (if (and (exact-integer? n) (or (< n INT-MIN) (> n INT-MAX))) cql-null n))
(define (wrap-int n long?) (if long? (fp-long n) (int-guard n)))
(define (dec-abs d) (if (< (decimal->exact d) 0) (decimal-negate d) d))

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
(define (singular-unit u) (if (and (> (string-length u) 0) (char=? (string-ref u (sub1 (string-length u))) #\s)) (substring u 0 (sub1 (string-length u))) u))
(define (moment-prec-symbol m)
  (cond [(moment-frac m) 'millisecond]
        [else (case (moment-prec m) [(1) 'year] [(2) 'month] [(3) 'day] [(4) 'hour] [(5) 'minute] [else 'second])]))
(define (round-to-int d) (inexact->exact (round (if (fp-decimal? d) (decimal->exact d) d))))

;; ---- operator registry -----------------------------------------------------
;; The operator catalogue is DATA (the HTDP move): the interpreter core stays a small structural
;; recursion over ELM nodes, and operator application is one O(1) hash lookup — the CQL analog of
;; fhirpath/functions.rkt's fn registry.
;;
;; An Op is (op Boolean Impl):
;;   - eager (lazy? = #f): Impl is (Listof CqlValue) Env -> CqlValue  (operands pre-evaluated)
;;   - lazy  (lazy? = #t): Impl is (Listof ELM-node) Env -> CqlValue  (Impl evaluates what it needs,
;;                          so And/Or/Implies can short-circuit on a decisive operand)
(struct op (lazy? impl) #:transparent)
(define OPS (make-hash))                                  ; op-name(string) -> op
(define (def-op! name lazy? impl) (hash-set! OPS name (op lazy? impl)))
(define (op-count) (hash-count OPS))                      ; coverage introspection

;; apply-op : String (Listof ELM-node) Env -> CqlValue
(define (apply-op name raw-operands env)
  (define o (hash-ref OPS name (lambda () (fail (format "unsupported operator ~a" name)))))
  (if (op-lazy? o)
      ((op-impl o) raw-operands env)
      ((op-impl o) (map (lambda (x) (eval-elm x env)) raw-operands) env)))

;; registration shorthands: unary (one value), nary (the value list), lazy (raw ELM operands + env)
(define (def-unary! name f) (def-op! name #f (lambda (vals _e) (f (car vals)))))
(define (def-nary!  name f) (def-op! name #f (lambda (vals _e) (f vals))))
(define (def-lazy!  name f) (def-op! name #t f))

;; ---- unary operators -------------------------------------------------------
(def-unary! "Negate"
  (lambda (x) (cond [(cql-null? x) cql-null]
                    [(exact-integer? x) (- x)] [(fp-long? x) (fp-long (- (fp-long-v x)))]
                    [(fp-decimal? x) (decimal-negate x)]
                    [(fp-quantity? x) (fp-quantity (decimal-negate (->dec (fp-quantity-value x))) (fp-quantity-unit x) #f)]
                    [else (fail "cannot negate")])))
(def-unary! "Not"     (lambda (x) (three-not x)))         ; three-valued negation
(def-unary! "Exists"  (lambda (x) (and (list? x) (pair? (drop-nulls x)))))  ; true iff a non-null element exists
(def-unary! "IsNull"  cql-null?)
(def-unary! "IsTrue"  (lambda (x) (eq? x #t)))
(def-unary! "IsFalse" (lambda (x) (eq? x #f)))
(def-unary! "Abs"     (lambda (x) (cond [(cql-null? x) cql-null]
                                        [(fp-quantity? x) (fp-quantity (dec-abs (->dec (fp-quantity-value x))) (fp-quantity-unit x) #f)]
                                        [else (num-map x abs (lambda (d) (dec-abs d)))])))
(def-unary! "Ceiling" (lambda (x) (nn x (lambda () (int-guard (ceil-int x))))))
(def-unary! "Floor"   (lambda (x) (nn x (lambda () (int-guard (floor-int x))))))
(def-unary! "Truncate" (lambda (x) (nn x (lambda () (int-guard (truncate-int x))))))
(def-unary! "Predecessor" (lambda (x) (step-or-fail x -1)))
(def-unary! "Successor"   (lambda (x) (step-or-fail x 1)))
;; the operator form errors at the type boundary (interval internals use point-step and degrade)
(define (step-or-fail x dir)
  (cond [(cql-null? x) cql-null]
        [else (define r (point-step x dir))
              (cond [(eq? r #f) (fail "predecessor/successor out of range")]
                    [else r])]))
(def-unary! "ToDate"      (lambda (x) (to-temporal x 'date)))
(def-unary! "ToDateTime"  (lambda (x) (to-temporal x 'datetime)))
(def-unary! "ToTime"      (lambda (x) (to-temporal x 'time)))
;; nullary Now/Today/TimeOfDay read the single evaluation-time moment from the env
(def-unary! "Precision" (lambda (x) (precision-of x)))
(def-nary!  "LowBoundary"  (lambda (vs) (boundary (car vs) (cadr vs) #t)))
(def-nary!  "HighBoundary" (lambda (vs) (boundary (car vs) (cadr vs) #f)))
(def-op! "Now"       #f (lambda (_vs env) (or (ienv-now env) cql-null)))
(def-op! "Today"     #f (lambda (_vs env) (component-from 'date (ienv-now env))))
(def-op! "TimeOfDay" #f (lambda (_vs env) (component-from 'time (ienv-now env))))
(def-unary! "Ln"      (lambda (x) (nn x (lambda () (let ([v (num->real x)])
                                                     (cond [(zero? v) (fail "Ln(0) overflows")]   ; -inf
                                                           [(< v 0) cql-null]                     ; NaN
                                                           [else (safe-float->dec (log (exact->inexact v)))]))))))
(def-unary! "Exp"     (lambda (x) (nn x (lambda () (safe-float->dec (exp (exact->inexact (num->real x))))))))
(def-unary! "ToString" (lambda (x) (cond [(cql-null? x) cql-null]
                                         [(moment? x) (moment->plain-string x)]
                                         [else (render x)])))
;; the ToString rendering drops the CQL literal prefixes (@ / @T): 09:30:01.003, 2000-01-01T…
(define (moment->plain-string m)
  (regexp-replace #px"^@T?" (moment->string m) ""))
(def-unary! "ToInteger" (lambda (x) (to-integer x)))
(def-unary! "ToLong"  (lambda (x) (if (cql-null? x) cql-null (fp-long (->int-val x)))))
(def-unary! "ToDecimal" (lambda (x) (cond [(cql-null? x) cql-null] [(string? x) (or (parse-decimal x) cql-null)]
                                          [(num? x) (->dec x)] [else cql-null])))
(def-unary! "ToBoolean" (lambda (x) (to-boolean x)))
(def-unary! "ToQuantity" (lambda (x) (to-quantity x)))
(def-unary! "Upper"   (lambda (x) (str-map x string-upcase)))
(def-unary! "Lower"   (lambda (x) (str-map x string-downcase)))
(def-unary! "Count"   (lambda (x) (if (list? x) (length (drop-nulls x)) cql-null)))
(def-unary! "Length"  (lambda (x) (cond [(string? x) (string-length x)] [(list? x) (length x)]
                                        [(cql-null-list? x) 0]           ; a list-typed null is empty
                                        [(cql-null? x) cql-null] [else (fail "Length")])))
(def-unary! "First"   (lambda (x) (if (and (list? x) (pair? x)) (car x) cql-null)))
(def-unary! "Last"    (lambda (x) (if (and (list? x) (pair? x)) (last x) cql-null)))
(def-unary! "Distinct" (lambda (x) (if (list? x) (dedup x) cql-null)))
(def-unary! "Sum"     (lambda (x) (aggregate x (lambda (a b) (arith + decimal-add a b)) 0)))
(def-unary! "Min"     (lambda (x) (aggregate-cmp x -1)))
(def-unary! "Max"     (lambda (x) (aggregate-cmp x 1)))
(def-unary! "Avg"     (lambda (x) (avg x)))
;; interval accessors / operators
(def-unary! "Start"   (lambda (x) (iv-start x)))
(def-unary! "End"     (lambda (x) (iv-end x)))
(def-unary! "Width"   (lambda (x) (iv-width x)))
(def-unary! "PointFrom" (lambda (x) (iv-point x)))
(def-unary! "SingletonFrom" (lambda (x) (singleton-from x)))
(def-unary! "Collapse" (lambda (x) (collapse-intervals x)))
(def-nary!  "Expand"  (lambda (vs) (expand-interval (car vs) (and (pair? (cdr vs)) (cadr vs)))))
;; list operators
(def-nary!  "Take"    (lambda (vs) (lst-take (car vs) (cadr vs))))
(def-nary!  "Skip"    (lambda (vs) (lst-skip (car vs) (cadr vs))))
(def-unary! "Tail"    (lambda (x) (lst-tail x)))
(def-nary!  "Slice"   (lambda (vs) (lst-slice (car vs)
                                              (if (>= (length vs) 2) (cadr vs) cql-null)
                                              (if (>= (length vs) 3) (caddr vs) cql-null))))
(def-unary! "descendents" (lambda (x) cql-null))    ; structural traversal: null/scalars have none
(def-unary! "ToConcept" (lambda (x) (cond [(cql-null? x) cql-null]
                                          [(cql-code? x) (cql-concept (list x) (cql-code-display x))]
                                          [(cql-concept? x) x] [else cql-null])))
(def-unary! "ToCode"    (lambda (x) (if (cql-code? x) x cql-null)))
(def-nary!  "IndexOf" (lambda (vs) (lst-index-of (car vs) (cadr vs))))
(def-unary! "Flatten" (lambda (x) (flatten-list x)))
;; aggregate functions
(def-unary! "AllTrue" (lambda (x) (all-true x)))
(def-unary! "AnyTrue" (lambda (x) (any-true x)))
(def-unary! "Product" (lambda (x) (aggregate x (lambda (a b) (arith * decimal-mul a b)) 1)))
(def-unary! "Mode"    (lambda (x) (mode x)))
(def-unary! "Median"  (lambda (x) (median x)))
(def-unary! "Variance" (lambda (x) (variance x #f)))
(def-unary! "StdDev"  (lambda (x) (stddev x #f)))
(def-unary! "PopulationVariance" (lambda (x) (variance x #t)))
(def-unary! "PopulationStdDev" (lambda (x) (stddev x #t)))
(def-unary! "GeometricMean" (lambda (x) (geometric-mean x)))
;; string functions
(def-nary!  "Split"   (lambda (vs) (str-split (car vs) (cadr vs))))
(def-nary!  "LastPositionOf" (lambda (vs) (last-position-of (car vs) (cadr vs))))
(def-nary!  "Matches" (lambda (vs) (str2 vs (lambda (s p) (regexp-match? (pregexp (string-append "^(?:" p ")$")) s)))))
(def-nary!  "ReplaceMatches"
  (lambda (vs) (if (ormap cql-null? vs) cql-null
                   ;; the CQL replacement string uses \x escapes for literals; unescape, then quote for
                   ;; Racket's replacement syntax (where & and \ are special)
                   (regexp-replace* (pregexp (cadr vs)) (car vs)
                                    (regexp-replace-quote (regexp-replace* #px"\\\\(.)" (caddr vs) "\\1"))))))
;; Message(source, condition, code, severity, message): returns source; an 'Error' severity with a
;; true condition raises (per spec, Error messages halt evaluation).
(def-nary!  "Message"
  (lambda (vs)
    (define cond? (and (>= (length vs) 2) (eq? (cadr vs) #t)))
    (define severity (and (>= (length vs) 4) (list-ref vs 3)))
    (if (and cond? (equal? severity "Error"))
        (fail (format "Message error: ~a" (if (>= (length vs) 5) (list-ref vs 4) "")))
        (car vs))))

(define (nn x th) (if (cql-null? x) cql-null (th)))
(define (num-map x int-f dec-f)
  (cond [(cql-null? x) cql-null] [(int-like? x) (wrap-int (int-f (->int x)) (fp-long? x))]
        [(fp-decimal? x) (dec-f x)] [else (fail "numeric expected")]))
(define (str-map x f) (if (string? x) (f x) cql-null))

(define (num->real x) (cond [(exact-integer? x) x] [(fp-long? x) (fp-long-v x)] [(fp-decimal? x) (decimal->exact x)] [else (fail "numeric expected")]))
(define (float->dec f) (or (parse-decimal (number->string (exact->inexact f))) (integer->decimal 0)))
(define (ceil-int x) (inexact->exact (ceiling (num->real x))))
(define (floor-int x) (inexact->exact (floor (num->real x))))
(define (truncate-int x) (inexact->exact (truncate (num->real x))))

;; ---- n-ary / binary operators ----------------------------------------------
;; arithmetic (Add/Subtract also do temporal duration arithmetic when the left operand is a moment)
(def-nary! "Add"      (lambda (vs) (bin vs add-values)))
(def-nary! "Subtract" (lambda (vs) (bin vs sub-values)))
(def-nary! "Multiply" (lambda (vs) (bin vs mul-values)))
(def-nary! "Divide"   (lambda (vs) (bin vs divide)))
(def-nary! "TruncatedDivide" (lambda (vs) (bin vs trunc-div)))
(def-nary! "Modulo"   (lambda (vs) (bin vs modulo-op)))
(def-nary! "Power"    (lambda (vs) (bin vs power)))
(def-nary! "Log"      (lambda (vs) (bin vs (lambda (a b) (let ([av (num->real a)] [bv (num->real b)])
                                                           (if (or (<= av 0) (<= bv 0) (= bv 1)) cql-null
                                                               (safe-float->dec (/ (log (exact->inexact av)) (log (exact->inexact bv))))))))))
(def-nary! "Round"    (lambda (vs) (round-op vs)))
;; string
(def-nary! "Concatenate" (lambda (vs) (concat vs)))
(def-nary! "Combine"  (lambda (vs) (combine vs)))
(def-nary! "Substring" (lambda (vs) (substring-op vs)))
(def-nary! "StartsWith" (lambda (vs) (str2 vs (lambda (s p) (and (>= (string-length s) (string-length p)) (string=? (substring s 0 (string-length p)) p))))))
(def-nary! "EndsWith"   (lambda (vs) (str2 vs (lambda (s p) (and (>= (string-length s) (string-length p)) (string=? (substring s (- (string-length s) (string-length p))) p))))))
(def-nary! "PositionOf" (lambda (vs) (position-of vs)))
(def-nary! "Indexer"  (lambda (vs) (indexer vs)))
;; comparison / equality
(def-nary! "Equal"    (lambda (vs) (equal-op vs)))
(def-nary! "Equivalent" (lambda (vs) (equivalent vs)))
(def-nary! "Less"     (lambda (vs) (compare-op vs (lambda (c) (< c 0)))))
(def-nary! "Greater"  (lambda (vs) (compare-op vs (lambda (c) (> c 0)))))
(def-nary! "LessOrEqual" (lambda (vs) (compare-op vs (lambda (c) (<= c 0)))))
(def-nary! "GreaterOrEqual" (lambda (vs) (compare-op vs (lambda (c) (>= c 0)))))
;; logical (three-valued, short-circuit on a decisive operand — also avoids errors in unreached branches)
(def-lazy! "And"      (lambda (ops env) (and-lazy (car ops) (cadr ops) env)))
(def-lazy! "Or"       (lambda (ops env) (or-lazy (car ops) (cadr ops) env)))
(def-nary! "Xor"      (lambda (vs) (three-xor vs)))
(def-lazy! "Implies"  (lambda (ops env) (implies-lazy (car ops) (cadr ops) env)))
;; membership / set
(def-nary! "In"       (lambda (vs) (in-op (car vs) (cadr vs))))
(def-nary! "Contains" (lambda (vs) (in-op (cadr vs) (car vs))))
(def-nary! "Union"    (lambda (vs) (set-op vs 'union)))
(def-nary! "Intersect" (lambda (vs) (set-op vs 'intersect)))
(def-nary! "Except"   (lambda (vs) (set-op vs 'except)))
;; null
(def-nary! "Coalesce" (lambda (vs) (coalesce vs)))

(define (bin vals f)
  (define a (car vals)) (define b (cadr vals))
  (if (or (cql-null? a) (cql-null? b)) cql-null (f a b)))

(define (int-bin a b f) (if (or (cql-null? a) (cql-null? b)) cql-null (let ([r (f (num->real a) (num->real b))]) (if r (if (or (fp-decimal? a) (fp-decimal? b)) (float->dec r) r) cql-null))))
;; a NaN (out-of-domain) is null; an INFINITE result is an overflow error (Exp(1000), Ln(0))
(define (safe-float->dec f)
  (cond [(nan? f) cql-null] [(infinite? f) (fail "arithmetic overflow")] [else (float->dec f)]))

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

;; ---- strings ---------------------------------------------------------------
(define (concat vals)
  (if (ormap cql-null? vals) cql-null (apply string-append (map ->str vals))))
(define (combine vals)
  ;; Combine(list) or Combine(list, sep) ; an empty (or all-null) list -> null
  (define lst (car vals))
  (define sep (if (and (pair? (cdr vals)) (string? (cadr vals))) (cadr vals) ""))
  (cond [(not (list? lst)) cql-null]
        [(null? (drop-nulls lst)) cql-null]
        [else (join (map ->str (drop-nulls lst)) sep)]))
(define (->str x) (if (string? x) x (render x)))
(define (join xs sep) (cond [(null? xs) ""] [(null? (cdr xs)) (car xs)] [else (string-append (car xs) sep (join (cdr xs) sep))]))
(define (str2 vals f) (bin vals (lambda (a b) (and (string? a) (string? b) (f a b)))))
(define (substring-op vals)
  (define s (car vals))
  (cond [(cql-null? s) cql-null]
        [(cql-null? (cadr vals)) cql-null]
        [else (define start (->int-val (cadr vals)))
              (define len (and (>= (length vals) 3) (not (cql-null? (caddr vals))) (->int-val (caddr vals))))
              (define n (string-length s))
              (cond [(and (= start 0) (= n 0)) ""]
                    [(or (< start 0) (>= start n)) cql-null]
                    [len (substring s start (min n (+ start (max 0 len))))]
                    [else (substring s start)])]))
(define (position-of vals)
  ;; PositionOf(pattern, string) -> 0-based index or -1
  (define pat (car vals)) (define s (cadr vals))
  (if (or (cql-null? pat) (cql-null? s)) cql-null
      (let ([i (find-substring s pat)]) (or i -1))))
(define (find-substring s pat)
  (define n (string-length s)) (define m (string-length pat))
  (let loop ([i 0]) (cond [(> (+ i m) n) #f] [(string=? (substring s i (+ i m)) pat) i] [else (loop (add1 i))])))
(define (indexer vals)
  (define x (car vals)) (define i (cadr vals))
  (cond [(or (cql-null? x) (cql-null-list? x) (cql-null? i)) cql-null]
        [(string? x) (if (and (>= (->int i) 0) (< (->int i) (string-length x))) (string (string-ref x (->int i))) cql-null)]
        [(list? x) (if (and (>= (->int i) 0) (< (->int i) (length x))) (list-ref x (->int i)) cql-null)]
        [else (fail "Indexer")]))

;; ---- comparison / equality -------------------------------------------------
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
(define (sgn n) (cond [(< n 0) -1] [(> n 0) 1] [else 0]))

;; CQL Equal: 'true|'false|'empty (null if either null, or a null-bearing structural compare)
(define (cql-equal a b)
  (cond
    [(or (cql-null? a) (cql-null? b)) 'empty]
    [(and (list? a) (list? b)) (list-equal a b)]
    [(and (cql-interval? a) (cql-interval? b)) (interval-equal a b)]
    [(and (cql-tuple? a) (cql-tuple? b)) (tuple-equal a b)]
    [(and (cql-ratio? a) (cql-ratio? b)) (ratio-equal a b)]
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

;; ---- three-valued logic ----------------------------------------------------
;; three-not : CqlValue -> Boolean|Null
(define (three-not x) (cond [(cql-null? x) cql-null] [(boolean? x) (not x)] [else (fail "Not on non-boolean")]))
;; three-xor : (Listof CqlValue) -> Boolean|Null   (null if either operand is null)
(define (three-xor vals)
  (define a (car vals)) (define b (cadr vals))
  (if (or (cql-null? a) (cql-null? b)) cql-null (not (eq? a b))))
;; and/or/implies are LAZY: they take the raw ELM operands + env and short-circuit on a decisive
;; operand (false for And, true for Or, false-antecedent for Implies), so an error or expensive
;; computation in the unreached branch never fires. Result is CQL three-valued.
;; and-lazy : ELM ELM Env -> Boolean|Null
(define (and-lazy a b env)
  (define av (eval-elm a env))
  (cond [(eq? av #f) #f]
        [else (define bv (eval-elm b env))
              (cond [(eq? bv #f) #f] [(and (eq? av #t) (eq? bv #t)) #t] [else cql-null])]))
;; or-lazy : ELM ELM Env -> Boolean|Null
(define (or-lazy a b env)
  (define av (eval-elm a env))
  (cond [(eq? av #t) #t]
        [else (define bv (eval-elm b env))
              (cond [(eq? bv #t) #t] [(and (eq? av #f) (eq? bv #f)) #f] [else cql-null])]))
;; implies-lazy : ELM ELM Env -> Boolean|Null   (false antecedent => true, without evaluating b)
(define (implies-lazy a b env)
  (define av (eval-elm a env))
  (cond [(eq? av #f) #t]
        [else (define bv (eval-elm b env))
              (cond [(eq? bv #t) #t] [(and (eq? av #t) (eq? bv #f)) #f] [else cql-null])]))

;; ---- list helpers ----------------------------------------------------------
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

;; ---- membership / set ------------------------------------------------------
(define (in-op x coll)
  (cond
    [(cql-interval? coll) (point-in-interval x coll)]
    [(or (cql-null? coll) (cql-null-list? coll)) #f]   ; membership in a null list is false (per CQL tests)
    [(list? coll) (if (cql-member? x coll) #t #f)]     ; found (null matches null) -> true, else false
    [else (fail "In: not a list or interval")]))
;; cql-member? : CqlValue (Listof CqlValue) -> Boolean   null-aware membership
(define (cql-member? x lst)
  (and (ormap (lambda (e) (or (and (cql-null? x) (cql-null? e)) (cql-eq? x e))) lst) #t))

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
(define (member* e lst) (ormap (lambda (x) (cql-eq? e x)) lst))

;; coalesce : (Listof CqlValue) -> CqlValue   first non-null argument; the single-LIST form
;; coalesces the list's elements (Coalesce({null, 'a'}) = 'a')
(define (coalesce vals)
  (define vs (if (and (= 1 (length vals)) (list? (car vals))) (car vals) vals))
  (let loop ([vs vs]) (cond [(null? vs) cql-null] [(cql-null? (car vs)) (loop (cdr vs))] [else (car vs)])))

;; ---- aggregates ------------------------------------------------------------
;; aggregate : CqlValue (CqlValue CqlValue -> CqlValue) Any -> CqlValue   fold over non-null elements
(define (aggregate x f _seed)
  (cond [(not (list? x)) cql-null]
        [else (define nn (drop-nulls x))
              (if (null? nn) cql-null (foldl (lambda (e acc) (f acc e)) (car nn) (cdr nn)))]))
;; aggregate-cmp : CqlValue (or -1 1) -> CqlValue   min (dir=-1) / max (dir=1) over non-null elements
(define (aggregate-cmp x dir)
  (cond [(not (list? x)) cql-null]
        [else (define nn (drop-nulls x))
              (if (null? nn) cql-null
                  (foldl (lambda (e acc) (if (= (sgn (cql-compare e acc)) dir) e acc)) (car nn) (cdr nn)))]))
;; avg : CqlValue -> CqlValue   arithmetic mean (Decimal) over non-null elements
(define (avg x)
  (cond [(not (list? x)) cql-null]
        [else (define nn (drop-nulls x))
              (if (null? nn) cql-null
                  (decimal-div (foldl (lambda (e acc) (decimal-add acc (->dec e))) (integer->decimal 0) nn)
                               (integer->decimal (length nn))))]))

;; ---- list operators --------------------------------------------------------
(define (lst? x) (and (list? x) #t))
(define (lst-take x n)
  (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null] [(cql-null? n) '()]
        [else (let ([k (max 0 (->int-val n))]) (if (>= k (length x)) x (take x k)))]))
(define (lst-skip x n)
  (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null] [(cql-null? n) x]
        [else (let ([k (max 0 (->int-val n))]) (if (>= k (length x)) '() (drop x k)))]))
(define (lst-tail x) (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null] [(null? x) '()] [else (cdr x)]))
;; lst-slice : CqlValue CqlValue CqlValue -> CqlValue
;; Slice(list [, start [, end]]): null start/end default to the whole extent; a NEGATIVE index counts
;; from the end (-2 -> the last two). Out-of-range windows clamp to the empty list.
(define (lst-slice x s e)
  (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null]
        [else (define n (length x))
              (define (norm i dflt) (cond [(cql-null? i) dflt]
                                          [else (define v (->int-val i)) (if (< v 0) (max 0 (+ n v)) v)]))
              (define start (min n (norm s 0)))
              (define end (min n (norm e n)))
              (if (> start end) '() (take (drop x start) (- end start)))]))
(define (lst-index-of x el)
  (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null] [(cql-null? el) cql-null]   ; IndexOf(_, null) = null
        [else (let loop ([i 0] [xs x]) (cond [(null? xs) -1] [(cql-eq? (car xs) el) i] [else (loop (add1 i) (cdr xs))]))]))
(define (flatten-list x)
  (cond [(cql-null? x) cql-null] [(not (lst? x)) cql-null]
        [else (append-map (lambda (e) (if (list? e) e (list e))) x)]))

;; ---- more aggregates -------------------------------------------------------
;; all-true / any-true : CqlValue -> Boolean   null elements are ignored (empty/null list: #t / #f).
;; NOTE: must not use findf to look for #f — the found element IS #f, indistinguishable from not-found.
(define (all-true x) (cond [(cql-null? x) #t] [(not (lst? x)) #t] [else (not (for/or ([e (in-list x)]) (eq? e #f)))]))
(define (any-true x) (cond [(cql-null? x) #f] [(not (lst? x)) #f] [else (for/or ([e (in-list x)]) (eq? e #t))]))
(define (median x)
  (cond [(not (lst? x)) cql-null]
        [else (define nn (sort (drop-nulls x) (lambda (a b) (< (cql-compare a b) 0))))
              (define n (length nn))
              (cond [(zero? n) cql-null]
                    [(odd? n) (->dec (list-ref nn (quotient n 2)))]
                    [else (decimal-div (decimal-add (->dec (list-ref nn (sub1 (quotient n 2)))) (->dec (list-ref nn (quotient n 2))))
                                       (integer->decimal 2))])]))
(define (mode x)
  (cond [(not (lst? x)) cql-null]
        [else (define nn (drop-nulls x)) (define counts (make-hash))
              (cond [(null? nn) cql-null]
                    [else (for ([e (in-list nn)]) (hash-update! counts (render e) (lambda (c) (cons (add1 (car c)) e)) (cons 0 e)))
                          (cdr (argmax car (hash-values counts)))])]))
(define (variance x pop?)
  (cond [(not (lst? x)) cql-null]
        [else (define nn (map (lambda (e) (decimal->exact (->dec e))) (drop-nulls x)))
              (define n (length nn))
              (cond [(< n (if pop? 1 2)) cql-null]
                    [else (define mean (/ (apply + nn) n))
                          (define ss (apply + (map (lambda (v) (* (- v mean) (- v mean))) nn)))
                          (rat->dec (/ ss (if pop? n (sub1 n))))])]))
(define (stddev x pop?)
  (define v (variance x pop?))
  (if (cql-null? v) cql-null (dec8 (sqrt (exact->inexact (decimal->exact v))))))
;; dec8 : real -> fp-decimal   at CQL's max decimal scale (8 fractional digits)
(define (dec8 r) (decimal-round (rat->dec r) 8))
(define (geometric-mean x)
  (cond [(not (lst? x)) cql-null]
        [else (define nn (map (lambda (e) (exact->inexact (decimal->exact (->dec e)))) (drop-nulls x)))
              (if (null? nn) cql-null (rat->dec (expt (apply * nn) (/ 1.0 (length nn)))))]))
(define (rat->dec r) (or (parse-decimal (number->string (exact->inexact r))) (integer->decimal 0)))

;; ---- more string functions -------------------------------------------------
(define (str-split s sep)
  (cond [(cql-null? s) cql-null]
        [(not (string? s)) cql-null]
        [(cql-null? sep) (list s)]                      ; null separator -> the whole string, one element
        [(not (string? sep)) cql-null]
        [else (string-split s sep #:trim? #f)]))
(define (last-position-of pat s)
  (cond [(or (cql-null? pat) (cql-null? s)) cql-null]
        [else (define n (string-length s)) (define m (string-length pat))
              (let loop ([i (- n m)]) (cond [(< i 0) -1] [(string=? (substring s i (+ i m)) pat) i] [else (loop (sub1 i))]))]))

;; ---- query engine ----------------------------------------------------------
;; run-query : sources lets where ret ret-distinct sort agg Env -> CqlValue
;; sources/lets are (Listof (cons string elm)); agg is (list name distinct? starting-elm body-elm)|#f.
(define (run-query sources lets where ret ret-distinct sort agg env)
  (define srcs (map (lambda (s) (cons (car s) (eval-elm (cdr s) env))) sources))
  (cond
    [(ormap (lambda (s) (cql-null? (cdr s))) srcs) cql-null]
    [else
     (define aliases (map car srcs))
     (define scalar-single? (and (= 1 (length srcs)) (not (list? (cdr (car srcs))))))
     (define elem-lists (map (lambda (s) (if (list? (cdr s)) (cdr s) (list (cdr s)))) srcs))
     (define rows (query-cartesian aliases elem-lists))
     (define row-envs (map (lambda (row) (apply-lets (row-env env row) lets)) rows))
     (define kept (if where (filter (lambda (e) (eq? (eval-elm where e) #t)) row-envs) row-envs))
     (cond
       [agg (run-aggregate agg aliases kept env)]
       [else
        (define results (map (lambda (e) (row-result ret aliases e)) kept))
        (define r1 (if ret-distinct (dedup results) results))
        (define r2 (if sort (sort-values r1 sort) r1))
        (if scalar-single? (if (pair? r2) (car r2) cql-null) r2)])]))

(define (query-cartesian aliases lists)
  (if (null? aliases) (list (hash))
      (append-map (lambda (v) (map (lambda (r) (hash-set r (car aliases) v))
                                   (query-cartesian (cdr aliases) (cdr lists))))
                  (car lists))))
(define (row-env base row)
  (struct-copy ienv base [operands (for/fold ([h (ienv-operands base)]) ([(k v) (in-hash row)]) (hash-set h k v))]))
(define (apply-lets env lets)
  (for/fold ([e env]) ([l (in-list lets)])
    (struct-copy ienv e [operands (hash-set (ienv-operands e) (car l) (eval-elm (cdr l) e))])))
(define (row-result ret aliases e)
  (cond [ret (eval-elm ret e)]
        [(= 1 (length aliases)) (hash-ref (ienv-operands e) (car aliases))]
        [else (cql-tuple (for/hash ([a (in-list aliases)]) (values (string->symbol a) (hash-ref (ienv-operands e) a))))]))
;; sort-values : (Listof CqlValue) ('asc|'desc) -> (Listof CqlValue)
;; Sorting needs a TOTAL order, so undecidable pairs (mixed-precision moments) fall back to a
;; deterministic tiebreak: component-wise with an absent component sorting FIRST (so @2012-10-05
;; precedes @2012-10-05T10, per the reference implementation).
(define (sort-values xs dir)
  (sort xs (lambda (a b) (define c (total-compare a b)) (if (eq? dir 'desc) (> c 0) (< c 0)))))
;; total-compare : CqlValue CqlValue -> -1|0|1   cql-compare, totalized
(define (total-compare a b)
  (define c (cql-compare a b))
  (cond [(number? c) c]
        [(and (moment? a) (moment? b)) (moment-total-compare a b)]
        [else 0]))
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
(define (run-aggregate agg aliases kept env)
  (define name (car agg)) (define adist (cadr agg)) (define start-elm (caddr agg)) (define body (cadddr agg))
  (define rows (if adist (dedup-rows kept aliases) kept))
  (define start (if start-elm (eval-elm start-elm env) cql-null))
  (foldl (lambda (e acc) (eval-elm body (struct-copy ienv e [operands (hash-set (ienv-operands e) name acc)])))
         start rows))
;; dedup row-envs by their alias-value tuple (for `aggregate distinct`)
(define (dedup-rows envs aliases)
  (define seen (make-hash)) (define out '())
  (for ([e (in-list envs)])
    (define key (map (lambda (a) (render (hash-ref (ienv-operands e) a))) aliases))
    (unless (hash-ref seen key #f) (hash-set! seen key #t) (set! out (cons e out))))
  (reverse out))

;; ---- control flow ----------------------------------------------------------
(define (eval-if c t e env)
  (define cv (eval-elm c env))
  (if (eq? cv #t) (eval-elm t env) (eval-elm e env)))   ; false or null -> else

(define (eval-case comparand items els env)
  (define cmp (and comparand (eval-elm comparand env)))
  (let loop ([its items])
    (cond
      [(null? its) (if els (eval-elm els env) cql-null)]
      [else (define it (car its))
            (define w (eval-elm (elm-case-item-when it) env))
            (define hit (if comparand (cql-eq? cmp w) (eq? w #t)))
            (if hit (eval-elm (elm-case-item-then it) env) (loop (cdr its)))])))

;; ---- navigation / functions / types ---------------------------------------
(define (eval-property v path)
  (cond [(cql-null? v) cql-null]
        [(cql-tuple? v) (hash-ref (cql-tuple-fields v) (string->symbol path) cql-null)]
        [else cql-null]))          ; slice-1: property on non-tuples is undecidable -> null

(define (eval-function name args env)
  (define f (hash-ref (ienv-funcs env) name (lambda () (fail (format "unknown function ~a" name)))))
  (define ops (elm-function-def-operands f))
  (define env* (struct-copy ienv env
                            [operands (for/fold ([h (hash)]) ([o (in-list ops)] [a (in-list args)])
                                        (hash-set h (elm-operand-def-name o) a))]))
  (eval-elm (elm-function-def-expression f) env*))

(define (eval-is v type)
  (cond [(cql-null? v) #f]
        [(elm-named-type? type) (value-is-type? v (elm-named-type-name type))]
        [else #f]))
(define (simple-name qn) (let ([m (regexp-match #rx"}([^}]+)$" qn)]) (if m (cadr m) qn)))
(define (value-is-type? v qn)
  (define t (simple-name qn))
  (case t
    [("Integer") (exact-integer? v)] [("Long") (fp-long? v)] [("Decimal") (fp-decimal? v)]
    [("String") (string? v)] [("Boolean") (boolean? v)] [("Quantity") (fp-quantity? v)]
    [("Date" "DateTime" "Time") (moment? v)]
    [("Code") (cql-code? v)] [("Concept") (cql-concept? v)]
    ;; class hierarchy: ValueSet and CodeSystem are Vocabularies
    [("Vocabulary") (and (cql-obj? v) (member (obj-class-simple v) '("ValueSet" "CodeSystem" "Vocabulary")) #t)]
    [("ValueSet" "CodeSystem") (and (cql-obj? v) (string=? (obj-class-simple v) t))]
    [else #f]))
(define (obj-class-simple o) (simple-name (regexp-replace #px"^System\\." (cql-obj-class o) "")))

(define (convert-to v to-type)
  (if (cql-null? v) cql-null
      (case to-type
        [("String") (render v)] [("Integer") (to-integer v)] [("Long") (fp-long (->int-val v))]
        [("Decimal") (->dec v)] [("Boolean") (to-boolean v)] [("Quantity") (to-quantity v)]
        [("Date") (to-temporal (if (moment? v) v (render v)) 'date)]
        [("DateTime") (to-temporal (if (moment? v) v (render v)) 'datetime)]
        [("Time") (to-temporal (if (moment? v) v (render v)) 'time)]
        [else cql-null])))

(define (->int-val x) (cond [(exact-integer? x) x] [(fp-long? x) (fp-long-v x)] [(fp-decimal? x) (truncate-int x)] [else (fail "not integral")]))
(define (to-integer x)
  (cond [(cql-null? x) cql-null] [(exact-integer? x) x] [(fp-long? x) (fp-long-v x)]
        [(string? x) (or (string->number x) cql-null)] [(fp-decimal? x) (truncate-int x)] [else cql-null]))
(define (to-boolean x)
  (cond [(cql-null? x) cql-null] [(boolean? x) x]
        [(string? x) (cond [(member (string-downcase x) '("true" "t" "yes" "y" "1")) #t]
                           [(member (string-downcase x) '("false" "f" "no" "n" "0")) #f] [else cql-null])]
        [else cql-null]))
;; to-temporal : CqlValue ('date|'datetime|'time) -> moment|Null   parse a string / pass a moment
;; through. Time strings may carry a leading T and a timezone suffix (ignored for a Time value).
(define (to-temporal x kind)
  (cond [(cql-null? x) cql-null]
        [(moment? x) x]
        [(string? x) (case kind
                       [(date) (or (parse-fp-date x) cql-null)]
                       [(datetime) (or (parse-fp-datetime x) cql-null)]
                       [(time) (or (parse-fp-time (strip-time-decorations x)) cql-null)])]
        [else cql-null]))
(define (strip-time-decorations s)
  (regexp-replace #px"(Z|[+-][0-9]{2}:[0-9]{2})$" (regexp-replace #px"^T" s "") ""))
(define (to-quantity x)
  (cond [(cql-null? x) cql-null] [(fp-quantity? x) x] [(num? x) (make-quantity (->dec x) "1")]
        [(string? x) (let ([m (regexp-match #px"^([0-9.]+)\\s*'([^']*)'$" x)])
                       (if m (make-quantity (or (parse-decimal (cadr m)) (integer->decimal 0)) (caddr m)) cql-null))]
        [else cql-null]))

(module+ test
  (require rackunit
           (only-in "parser/parse.rkt" parse-cql-expr)
           (only-in "compile.rkt" compile-expr)
           (only-in fhir/result/result ok-value))
  ;; end-to-end: parse -> compile -> eval a bare expression
  (define (E s) (eval-elm (ok-value (compile-expr (ok-value (parse-cql-expr s)))) (make-ienv)))
  (check-equal? (E "1 + 2") 3)
  (check-equal? (E "2 * 3 + 1") 7)
  (check-equal? (E "10 - 4 * 2") 2)
  (check-equal? (decimal->string (E "3.0 + 0.14")) "3.14")
  (check-equal? (E "7 div 2") 3)
  (check-equal? (E "7 mod 3") 1)
  (check-equal? (E "2 ^ 10") 1024)
  (check-equal? (E "'a' + 'b'") "ab")
  (check-equal? (E "'a' & 'b'") "ab")
  (check-true (cql-null? (E "'a' + null")))     ; + is null-propagating
  (check-equal? (E "'x' & null") "x")            ; & treats null as ''
  (check-equal? (E "1 = 1") #t)
  (check-equal? (E "1 = 2") #f)
  (check-equal? (E "1 != 2") #t)
  (check-equal? (E "3 > 2") #t)
  (check-equal? (E "2 >= 2") #t)
  (check-equal? (E "'abc' < 'abd'") #t)
  (check-equal? (E "true and false") #f)
  (check-true (cql-null? (E "true and null")))
  (check-equal? (E "true or null") #t)
  (check-equal? (E "not true") #f)
  (check-equal? (E "-5") -5)
  (check-equal? (E "Abs(-3)") 3)
  (check-equal? (E "if 1 < 2 then 'yes' else 'no'") "yes")
  (check-equal? (E "case when false then 1 when true then 2 else 3 end") 2)
  (check-equal? (E "{1, 2, 3}") (list 1 2 3))
  (check-equal? (E "Count({1, 2, 3})") 3)
  (check-equal? (E "First({4, 5, 6})") 4)
  (check-equal? (E "5 in {1, 5, 9}") #t)
  (check-equal? (E "5 in Interval[1, 10]") #t)
  (check-equal? (E "10 in Interval[1, 10)") #f)
  (check-equal? (E "{1, 2} union {2, 3}") (list 1 2 3))
  (check-equal? (E "{1, 2, 3} intersect {2, 3, 4}") (list 2 3))
  (check-equal? (E "Coalesce(null, null, 7)") 7)
  (check-equal? (E "ToString(42)") "42")
  (check-equal? (E "ToInteger('15')") 15)
  (check-equal? (E "Upper('abc')") "ABC")
  (check-equal? (E "5 is Integer") #t)
  (check-equal? (E "convert 5 to String") "5")
  ;; library run with cross references
  (require (only-in "parser/parse.rkt" parse-cql) (only-in "compile.rkt" compile-library))
  (define results (run-library (ok-value (compile-library (ok-value (parse-cql "define A: 3 + 4\ndefine B: A * 2"))))))
  (check-equal? (hash-ref results "A") 7)
  (check-equal? (hash-ref results "B") 14))
