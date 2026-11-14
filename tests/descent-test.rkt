#lang racket/base

;;; descent-test.rkt — the recursive-descent reader held to the grammar.
;;;
;;; parse.rkt reads CQL with parser/descent.rkt and hands to brag (parser/grammar.rkt, the port of
;;; cql.g4) only what descent does not take. This test keeps brag as the reference:
;;;   - at the seam, chunk by chunk: for every definition chunk of every library the repository
;;;     holds (the HL7 suite's are expressions, below; the ELM, QI-Core, QDM and eCQM corpora,
;;;     libraries/, examples/, guide.md's ```cql blocks), descent's datum is brag's
;;;     `parse-to-datum` datum, exactly;
;;;   - whole: `parse-cql` and `parse-cql/brag` (brag alone) give the same library, or the same
;;;     error word for word at the same place; `parse-cql-expr` and `parse-cql-expr/brag` the same
;;;     for every <expression> and <output> of the HL7 suites;
;;;   - generated (seeded, so a failure repeats): expressions grown from the grammar's productions
;;;     (every operator, literal, selector, timing phrase, query clause and nesting), small
;;;     libraries of every kind of definition, and token soups that are mostly not CQL — where a
;;;     reader that accepts too much would show.
;;; It also counts what descent read itself (the rest went to brag): a reader that refused
;;; everything would pass the agreement and buy nothing, so its share has a floor.

(require rackunit racket/list racket/file racket/string xml
         (prefix-in g: "../parser/grammar.rkt")
         (only-in "../parser/parse.rkt" parse-cql parse-cql/brag parse-cql-expr parse-cql-expr/brag definition-chunks
                  current-brag-reader)
         (only-in "../parser/descent.rkt" descend-library)
         (only-in "../error.rkt" cql-error-string)
         (only-in fhir/result/result ok? ok-value err-error)
         (only-in brag/support srcloc-token? srcloc-token-token token-struct? token-struct-skip?
                  token-struct-type token-struct-val))

;; ---- outcomes -----------------------------------------------------------------------------------
;; An Outcome is (list 'ok Value) | (list 'error String)
;; result-outcome : (-> Result) -> Outcome
(define (result-outcome read)
  (with-handlers ([exn:fail? (lambda (e) (list 'error (exn-message e)))])
    (define r (read))
    (if (ok? r) (list 'ok (ok-value r)) (list 'error (cql-error-string (err-error r))))))

;; A Tally is (tally texts brag-ok descended disagreements)
(struct tally (texts brag-ok descended disagreements) #:transparent)
(define clock (current-inexact-milliseconds))
(define (report label t)
  (printf "  [~a ms] " (round (- (current-inexact-milliseconds) clock))) (set! clock (current-inexact-milliseconds))
  (printf "  ~a: ~a texts, ~a read by the grammar, ~a by descent itself, ~a disagreements\n"
          label (tally-texts t) (tally-brag-ok t) (tally-descended t) (length (tally-disagreements t)))
  (for ([d (in-list (take (tally-disagreements t) (min 5 (length (tally-disagreements t)))))])
    (printf "    ~s\n      production ~s\n      grammar    ~s\n" (car d) (cadr d) (caddr d)))
  (check-equal? (tally-disagreements t) '() label))

;; real-tokens : (listof token) -> (listof token-struct)   the tokens the grammar reads
(define (real-tokens toks)
  (for*/list ([t (in-list toks)]
              [k (in-value (if (srcloc-token? t) (srcloc-token-token t) t))]
              #:when (and (token-struct? k) (not (token-struct-skip? k))))
    k))
;; lexed : String -> (listof token) | #f   the text's tokens, or #f when it does not lex
(define (lexed text)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (append* (definition-chunks (open-input-string (string-append "define __expr__: " text))))))
;; descended-text? : String -> Boolean   descent read the expression itself
(define (descended-text? text)
  (define toks (lexed text))
  (and toks (descend-library (real-tokens toks)) #t))

;; compare-expressions : (listof String) -> Tally
(define (compare-expressions texts)
  (for/fold ([t (tally 0 0 0 '())]) ([s (in-list (remove-duplicates texts))])
    (define production (result-outcome (lambda () (parse-cql-expr s))))
    (define reference (result-outcome (lambda () (parse-cql-expr/brag s))))
    (tally (add1 (tally-texts t))
           (+ (tally-brag-ok t) (if (eq? (car reference) 'ok) 1 0))
           (+ (tally-descended t) (if (descended-text? s) 1 0))
           (if (equal? production reference) (tally-disagreements t)
               (cons (list s production reference) (tally-disagreements t))))))

;; compare-libraries : (listof String) -> Tally   whole texts through parse-cql
(define (compare-libraries texts)
  (for/fold ([t (tally 0 0 0 '())]) ([s (in-list (remove-duplicates texts))])
    (define production (result-outcome (lambda () (parse-cql s #:source "t.cql"))))
    (define reference (result-outcome (lambda () (parse-cql/brag s #:source "t.cql"))))
    (define chunks (with-handlers ([exn:fail? (lambda (_) #f)]) (definition-chunks (open-input-string s))))
    (tally (add1 (tally-texts t))
           (+ (tally-brag-ok t) (if (eq? (car reference) 'ok) 1 0))
           (+ (tally-descended t) (if (and chunks (andmap (lambda (ch) (descend-library (real-tokens ch))) chunks)) 1 0))
           (if (equal? production reference) (tally-disagreements t)
               (cons (list (substring s 0 (min 200 (string-length s))) production reference) (tally-disagreements t))))))

;; compare-chunks : (listof String) -> Tally   at the seam: every distinct definition chunk, descent's
;; datum against brag's. A chunk longer than `long-chunk` tokens is compared whole (above) only:
;; brag takes half a minute on the longest list.
(define long-chunk 400)
;; brag-datums : token-key -> Datum   what brag read at the seam, so that the whole libraries' brag
;; reading (below) does not read a chunk again: a datum depends on the tokens' types and values only
(define brag-datums (make-hash))
(define (token-key real) (map (lambda (k) (cons (token-struct-type k) (token-struct-val k))) real))
(define (remembering-brag-reader toks)
  (define key (token-key (real-tokens toks)))
  (hash-ref brag-datums key
            (lambda ()
              (g:parse-to-datum (let ([ts toks]) (lambda () (if (null? ts) (void) (begin0 (car ts) (set! ts (cdr ts))))))))))
(define (compare-chunks texts)
  (define seen (make-hash))
  (for*/fold ([t (tally 0 0 0 '())])
             ([s (in-list texts)]
              [ch (in-list (definition-chunks (open-input-string s)))]
              [real (in-value (real-tokens ch))]
              [key (in-value (token-key real))]
              #:unless (hash-ref seen key #f))
    (hash-set! seen key #t)
    (define d (descend-library real))
    (cond
      [(> (length real) long-chunk) (tally (add1 (tally-texts t)) (tally-brag-ok t) (+ (tally-descended t) (if d 1 0)) (tally-disagreements t))]
      [else
       (define b (with-handlers ([exn:fail? (lambda (e) (list 'error (exn-message e)))])
                   (define datum (g:parse-to-datum (let ([ts ch]) (lambda () (if (null? ts) (void) (begin0 (car ts) (set! ts (cdr ts))))))))
                   (hash-set! brag-datums key datum)
                   datum))
       (tally (add1 (tally-texts t))
              (+ (tally-brag-ok t) (if (eq? (car b) 'error) 0 1))
              (+ (tally-descended t) (if d 1 0))
              (if (or (not d) (equal? d b)) (tally-disagreements t)
                  (cons (list (string-join (map (lambda (k) (format "~a" (token-struct-val k))) real) " ") d b)
                        (tally-disagreements t))))])))

;; ---- the libraries ------------------------------------------------------------------------------
(define cql-root (collection-path "cql"))
;; every .cql file of the repository (the vendored corpora, libraries/, examples/), a `#lang cql`
;; line left out, and the guide's ```cql blocks
(define library-texts
  (append
   (for/list ([f (in-directory cql-root)]
              #:when (regexp-match? #rx"[.]cql$" (path->string f))
              #:unless (regexp-match? #rx"/compiled/" (path->string f)))
     (regexp-replace #rx"^#lang[^\n]*" (file->string f) ""))
   (regexp-match* #px"```cql\n(.*?)```" (file->string (build-path cql-root "guide.md")) #:match-select cadr)))

(define chunks (compare-chunks library-texts))
(report "definition chunks of the libraries" chunks)
(check-true (> (tally-texts chunks) 4000) "the corpora are there")
;; what descent leaves to brag in the corpora: a query whose clause an enclosing query could take
;; (`let r: X.e E where … return E`) and `starts`/`ends` before an unqualified offset
(check-true (>= (tally-descended chunks) (- (tally-texts chunks) 45)) "descent reads all but a few dozen chunks")

(define libraries (parameterize ([current-brag-reader remembering-brag-reader]) (compare-libraries library-texts)))
(report "libraries, whole" libraries)

;; ---- the HL7 suites -----------------------------------------------------------------------------
;; every <expression> and <output> text, as the harness reads them
(define (elem-text e)
  (string-trim (apply string-append (for/list ([c (element-content e)] #:when (pcdata? c)) (pcdata-string c)))))
(define (suite-texts doc)
  (let loop ([x (document-element doc)])
    (cond [(and (element? x) (memq (element-name x) '(expression output))) (list (elem-text x))]
          [(element? x) (append-map loop (element-content x))]
          [else '()])))
(define suite-dir (build-path cql-root "tests" "vendor" "cql-tests"))
(define hl7-texts
  (append* (for/list ([f (in-list (directory-list suite-dir #:build? #t))] #:when (regexp-match? #rx"[.]xml$" (path->string f)))
             (suite-texts (call-with-input-file f read-xml)))))
(define suites (compare-expressions hl7-texts))
(report "HL7 suite expressions and outputs" suites)
(check-true (> (tally-texts suites) 2000))
(check-equal? (tally-descended suites) (tally-brag-ok suites) "descent reads every suite text the grammar reads")

;; ---- generated ----------------------------------------------------------------------------------
;; grown from the grammar: a pseudo-random generator with a fixed seed, so a failure repeats
(define rng (make-pseudo-random-generator))
(parameterize ([current-pseudo-random-generator rng]) (random-seed 20261114))
(define (pick xs) (list-ref xs (random (length xs) rng)))
(define (chance n) (< (random 100 rng) n))
(define (some n gen) (string-join (for/list ([_ (in-range n)]) (gen)) ", "))

(define names '("X" "Y" "\"Quoted Name\"" "code" "concept" "Lib.\"Def\"" "P" "E" "$this"))
(define literals
  '("1" "0.50" "42L" "'text'" "true" "false" "null" "@2014-01-25" "@2014-01-25T14:30:00.0Z" "@T12:00"
    "5 'mg'" "3 days" "1 year" "1 'cm':2 'cm'" "2 years"))
(define types '("Integer" "System.Decimal" "FHIR.Quantity" "List<Integer>" "Interval<DateTime>"
                "Tuple { a Integer, b List<String> }" "Choice<Integer, String>" "Code" "Concept" "code"))
(define binary-ops '("implies" "or" "xor" "and" "in" "contains" "=" "!=" "~" "!~" "<=" ">=" "<" ">"
                     "union" "intersect" "except" "+" "-" "&" "*" "/" "div" "mod" "^"))
(define timing-phrases
  '("before" "after" "on or before" "on or after" "before or on" "after or on" "same as" "same day as"
    "same month or before" "same year or after" "during" "during day of" "properly during" "included in"
    "properly included in month of" "includes" "properly includes" "includes start" "meets" "meets before"
    "meets after day of" "overlaps" "overlaps before" "overlaps after year of" "starts" "ends" "starts day of"
    "in day of" "contains month of" "within 3 days of" "properly within 1 year of end" "3 days or less before"
    "1 year or more after" "less than 2 hours before" "more than 1 day after start" "starts before"
    "ends after or on" "starts same day as" "occurs during" "starts 3 days or less on or after day of"
    "ends 1 hour or less before start" "starts 1 day before" "ends 2 days after" "occurs 1 day before"
    "starts within 3 days of" "ends properly during" "before start" "after end" "starts before start of"))
(define retrieves
  '("[Encounter]" "[\"Encounter\": \"VS\"]" "[Condition: code in \"VS\"]" "[Observation: code ~ \"C\"]"
    "[Observation: value.coding = \"C\"]" "[P -> Encounter]" "[X.y -> Observation: \"VS\"]" "[Code]"))

(define (gen-source depth)
  (format "~a ~a" (pick (list (pick retrieves) (format "(~a)" (gen-expr (sub1 depth))) (pick names) "A.b.c"))
          (pick '("a" "b" "E" "code" "x" "in" "start"))))
(define (gen-tail depth)
  (define (sub) (gen-expr (sub1 depth)))
  (define (let-item) (format "~a: ~a" (pick '("l" "m")) (sub)))
  (define (sort-item) (format "~a~a" (gen-unary (sub1 depth)) (pick '("" " asc" " desc"))))
  (define clauses
    (list (and (chance 25) (format "let ~a" (some (add1 (random 2 rng)) let-item)))
          (and (chance 30) (format "~a ~a such that ~a" (pick '("with" "without")) (gen-source (sub1 depth)) (sub)))
          (and (chance 50) (format "where ~a" (sub)))
          (and (chance 40)
               (if (chance 70)
                   (format "return ~a~a" (pick '("" "all " "distinct ")) (sub))
                   (format "aggregate ~aR~a: ~a" (pick '("" "distinct " "all "))
                           (if (chance 50) (format " starting ~a" (sub)) "") (sub))))
          (and (chance 30)
               (if (chance 30)
                   (format "sort ~a" (pick '("asc" "desc" "ascending" "descending")))
                   (format "sort by ~a" (some (add1 (random 2 rng)) sort-item))))))
  (string-join (filter values clauses) " "))
(define (gen-unary depth)
  (cond
    [(or (<= depth 0) (chance 30)) (if (chance 50) (pick literals) (pick names))]
    [else
     (case (random 12 rng)
       [(0) (format "~a.~a" (gen-unary (sub1 depth)) (pick '("value" "start" "end" "code" "display" "contains" "where(true)" "f()" "g(1, 2)")))]
       [(1) (format "~a[~a]" (gen-unary (sub1 depth)) (gen-expr (sub1 depth)))]
       [(2) (format "(~a)" (gen-expr (sub1 depth)))]
       [(3) (format "~a(~a)" (pick '("F" "Count" "Lib.F" "code")) (some (random 3 rng) (lambda () (gen-expr (sub1 depth)))))]
       [(4) (format "~a ~a" (pick '("not" "exists" "-" "+" "distinct" "flatten" "collapse" "expand" "start of" "end of"
                                    "width of" "point from" "singleton from" "predecessor of" "successor of" "year from"
                                    "duration in days of" "date from"))
                    (gen-unary (sub1 depth)))]
       [(5) (pick retrieves)]
       [(6) (format "Interval~a~a, ~a~a" (pick '("[" "(")) (gen-expr (sub1 depth)) (gen-expr (sub1 depth)) (pick '("]" ")")))]
       [(7) (pick (list (format "{ ~a }" (some (random 3 rng) (lambda () (gen-expr (sub1 depth)))))
                        (format "List<Integer> { ~a }" (some (random 3 rng) (lambda () (gen-expr (sub1 depth)))))
                        (format "Tuple { a: ~a, start: ~a }" (gen-expr (sub1 depth)) (gen-expr (sub1 depth)))
                        (format "{ a: ~a }" (gen-expr (sub1 depth))) "{ : }" "{}"
                        (format "System.Quantity { value: ~a, unit: 'mg' }" (gen-expr (sub1 depth)))
                        (format "Code { code: ~a }" (gen-expr (sub1 depth))) "Concept { }"
                        "Code '1' from \"CS\" display 'd'" "Code '1' from \"CS\"" "Code '1' from L.\"CS\""
                        "Concept { Code '1' from \"CS\", Code '2' from L.\"CS\" display 'x' } display 'c'"
                        (format "~a ~a" (pick '("minimum" "maximum")) (pick types))))]
       [(8) (format "~a ~a and ~a" (pick '("duration in days between" "difference in months between" "days between" "X between"))
                    (gen-unary (sub1 depth)) (gen-unary (sub1 depth)))]
       [(9) (format "~a ~a ~a" (pick '("cast" "convert")) (gen-unary (sub1 depth)) (pick (list (format "as ~a" (pick types)) (format "to ~a" (pick types)) "to 'mg'" "to days")))]
       [(10) (format "case ~a~a else ~a end" (if (chance 40) (format "~a " (gen-expr (sub1 depth))) "")
                     (string-join (for/list ([_ (in-range (add1 (random 2 rng)))]) (format "when ~a then ~a" (gen-expr (sub1 depth)) (gen-expr (sub1 depth)))) " ")
                     (gen-expr (sub1 depth)))]
       [else (gen-source depth)])]))
(define (gen-expr depth)
  (cond
    [(or (<= depth 0) (chance 20)) (gen-unary depth)]
    [else
     (case (random 9 rng)
       [(0 1 2) (format "~a ~a ~a" (gen-expr (sub1 depth)) (pick binary-ops) (gen-expr (sub1 depth)))]
       [(3) (format "~a ~a ~a" (gen-unary (sub1 depth)) (pick timing-phrases) (gen-unary (sub1 depth)))]
       [(4) (format "~a ~a" (gen-unary (sub1 depth))
                    (pick (list (format "is ~a" (pick types)) (format "as ~a" (pick types)) "is null" "is not null" "is true" "is not false")))]
       [(5) (format "if ~a then ~a else ~a" (gen-expr (sub1 depth)) (gen-expr (sub1 depth)) (gen-expr (sub1 depth)))]
       [(6 7) (format "~a ~a" (gen-source depth) (gen-tail depth))]
       [else (format "from ~a ~a" (some (add1 (random 2 rng)) (lambda () (gen-source (sub1 depth)))) (gen-tail depth))])]))

(define generated (compare-expressions (append (for/list ([_ (in-range 2750)]) (gen-expr 3)) (for/list ([_ (in-range 250)]) (gen-expr 4)))))
(report "generated expressions" generated)
(check-true (> (tally-brag-ok generated) 1000) "the generator writes mostly CQL")
(check-true (> (tally-descended generated) (* 1/2 (tally-brag-ok generated))) "descent reads most of what the grammar reads")

;; libraries: a header and definitions of every kind, in any order
(define (gen-definition)
  (case (random 10 rng)
    [(0) (format "using ~a~a" (pick '("FHIR" "QDM" "Lib.Model")) (pick '("" " version '4.0.1'" " version '1' called M")))]
    [(1) (format "include ~a~a" (pick '("FHIRHelpers" "Lib.Common")) (pick '("" " version '4.0.1'" " called H" " version '1' called C")))]
    [(2) (format "~aparameter ~a~a~a" (pick '("" "private " "public ")) (pick '("P" "\"Measurement Period\""))
                 (pick '("" " Integer" " Interval<DateTime>" " List<Tuple { a Integer }>" " code"))
                 (if (chance 50) (format " default ~a" (gen-expr 2)) ""))]
    [(3) (format "~acodesystem \"CS\": 'http://cs'~a" (pick '("" "private ")) (pick '("" " version '1'")))]
    [(4) (format "valueset \"VS\": 'urn:oid:1'~a~a" (pick '("" " version '2'")) (pick '("" " codesystems { \"CS\", L.\"CS2\" }")))]
    [(5) (format "code \"C\": '123' from ~a~a" (pick '("\"CS\"" "L.\"CS\"")) (pick '("" " display 'c'")))]
    [(6) (format "concept \"K\": { \"C\", L.\"D\" }~a" (pick '("" " display 'k'")))]
    [(7) (format "context ~a" (pick '("Patient" "Unfiltered" "FHIR.Patient")))]
    [(8) (format "define ~a~a(~a)~a: ~a" (pick '("function " "fluent function " "private function " "public fluent function "))
                 (pick '("F" "\"Fn\"" "version" "start"))
                 (pick '("" "a Integer" "a Integer, b List<FHIR.Observation>" "code code"))
                 (pick '("" " returns Boolean" " returns Interval<Date>"))
                 (if (chance 20) "external" (gen-expr 3)))]
    [else (format "define ~a~a: ~a" (pick '("" "private " "public ")) (pick '("X" "\"A Def\"" "code")) (gen-expr 3))]))
(define (gen-library)
  (string-append (if (chance 50) "library L version '1.0'\n" "")
                 (string-join (for/list ([_ (in-range (add1 (random 4 rng)))])
                                (string-append (if (chance 10) "  " "") (gen-definition)))
                              "\n")))
(define generated-libraries (compare-libraries (for/list ([_ (in-range 600)]) (gen-library))))
(report "generated libraries" generated-libraries)
(check-true (> (tally-brag-ok generated-libraries) 200) "the generator writes mostly CQL libraries")

;; a token soup: the pieces of CQL, in no particular order
(define pieces (append binary-ops names literals
                       '("." "(" ")" "[" "]" "," ":" "{" "}" "->" "is" "as" "null" "not" "exists" "if" "then"
                         "else" "case" "when" "end" "where" "return" "sort" "by" "let" "with" "such that" "from"
                         "start" "of" "day" "days" "before" "after" "on" "or" "same" "starts" "ends" "between"
                         "Interval" "List" "Tuple" "Code" "Concept" "distinct" "flatten" "3" "or less" "within"
                         "properly" "during" "included" "includes" "a" "E" "[Encounter]" "convert" "cast" "to")))
(define (gen-soup) (string-join (for/list ([_ (in-range (add1 (random 8 rng)))]) (pick pieces)) " "))
(define soups (compare-expressions (for/list ([_ (in-range 3000)]) (gen-soup))))
(report "token soups" soups)
