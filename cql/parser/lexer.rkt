#lang racket/base

;;; lexer.rkt — CQL source text -> the token stream brag consumes.
;;;
;;; Forked from fhirpath/parser/lexer.rkt (CQL's expression sublanguage is a FHIRPath superset) and
;;; extended for CQL: reserved words used as keyword operators/structure are emitted with their literal
;;; string as the token TYPE (so the grammar's "define"/"and"/"<=" literals match), CQL strings are
;;; single-quoted, delimited identifiers are double-quoted (FHIRPath uses backticks), and CQL adds Long
;;; (`42L`), `null`, `^`, and set-operator keywords. NUMBER keeps its lexeme (decimal precision).
;;; Temporal literals are tagged at lex time so they survive as data. WS + // and /* */ are skipped.

(require brag/support
         br-parser-tools/lex
         (prefix-in : br-parser-tools/lex-sre)
         racket/contract
         (only-in "ast.rkt" date-lit datetime-lit time-lit long-lit))

(provide (contract-out [tokenize (-> input-port? any/c)]))

(define (trim-ends s) (substring s 1 (sub1 (string-length s))))

;; unescape CQL string escapes, incl. \uXXXX (same set as FHIRPath)
(define (unescape s)
  (regexp-replace* #px"\\\\(u[0-9a-fA-F]{4}|.)" s
    (lambda (_ g)
      (cond
        [(char=? (string-ref g 0) #\u) (string (integer->char (string->number (substring g 1) 16)))]
        [else (case g
                [("n") "\n"] [("t") "\t"] [("r") "\r"] [("f") "\f"]
                [("\\") "\\"] [("'") "'"] [("\"") "\""] [("`") "`"] [("/") "/"]
                [else g])]))))

(define-lex-abbrevs
  [tz (:or "Z" (:: (:or "+" "-") numeric numeric ":" numeric numeric))]
  [timeformat (:: numeric numeric
                  (:? (:: ":" numeric numeric
                          (:? (:: ":" numeric numeric (:? (:: "." (:+ numeric))))))))]
  [dateformat (:: numeric numeric numeric numeric
                  (:? (:: "-" numeric numeric (:? (:: "-" numeric numeric)))))])

(define cql-lexer
  (lexer
   [(eof) (void)]
   [(:or " " "\t" "\r" "\n" "\f") (token 'WS lexeme #:skip? #t)]
   [(:: "//" (:* (:~ "\n"))) (token 'COMMENT lexeme #:skip? #t)]
   [(:: "/*" (:* (:or (:~ "*") (:: "*" (:~ "/")))) (:* "*") "*/") (token 'COMMENT lexeme #:skip? #t)]

   ;; string literal (single-quoted)
   [(:: "'" (:* (:or (:: "\\" any-char) (:~ "'" "\\"))) "'")
    (token 'STRING (unescape (trim-ends lexeme)))]

   ;; temporal literals — precise so a trailing "." stays a navigation dot
   [(:: "@" "T" timeformat) (token 'TIME (time-lit (substring lexeme 2)))]
   [(:: "@" dateformat "T" (:? (:: timeformat (:? tz)))) (token 'DATETIME (datetime-lit (substring lexeme 1)))]
   [(:: "@" dateformat) (token 'DATE (date-lit (substring lexeme 1)))]

   ;; Long literal (integer with an L suffix) before NUMBER — tagged so it's distinct from STRING
   [(:: (:+ numeric) "L") (token 'LONGNUMBER (long-lit (substring lexeme 0 (sub1 (string-length lexeme)))))]
   ;; number (integer or decimal) — keep the lexeme for precision
   [(:: (:+ numeric) (:? (:: "." (:+ numeric)))) (token 'NUMBER lexeme)]

   ;; boolean before identifiers/keywords
   [(:or "true" "false") (token 'BOOL (string=? lexeme "true"))]

   ;; reserved keywords (as literal-string token types) before identifiers. Maximal munch means a
   ;; longer identifier like `defined` outmatches the keyword `define`; equal-length ties go to this
   ;; rule because it precedes the identifier rule.
   [(:or "library" "version" "using" "called" "include" "public" "private" "parameter" "default"
         "codesystem" "valueset" "codesystems" "code" "concept" "from" "define" "context"
         "function" "returns" "external"
         "if" "then" "else" "case" "when" "end" "null" "not" "exists"
         "and" "or" "xor" "implies" "in" "contains" "is" "as" "cast" "convert" "to"
         "div" "mod" "union" "intersect" "except" "properly" "between" "difference" "duration"
         ;; timing / interval operators
         "before" "after" "same" "on" "of" "meets" "overlaps" "starts" "ends" "includes" "included"
         "during" "within" "start" "end" "width" "point" "per" "collapse" "expand" "singleton" "distinct" "flatten"
         ;; query
         "return" "where" "sort" "let" "aggregate" "starting" "all" "asc" "desc" "ascending" "descending"
         "minimum" "maximum" "predecessor" "successor"
         "Interval" "List" "Tuple" "Code" "Concept")
    (token lexeme lexeme)]

   ;; identifiers (plain and double-quoted delimited)
   [(:: (:or alphabetic "_") (:* (:or alphabetic numeric "_"))) (token 'IDENTIFIER (string->symbol lexeme))]
   [(:: "\"" (:* (:or (:: "\\" any-char) (:~ "\"" "\\"))) "\"")
    (token 'IDENTIFIER (string->symbol (unescape (trim-ends lexeme))))]

   ;; multi-char operators before single-char
   [(:or "<=" ">=" "!=" "!~") (token lexeme lexeme)]
   [(:or "." "[" "]" "(" ")" "," ";" ":" "{" "}" "|" "&" "+" "-" "*" "/" "^" "=" "<" ">" "~")
    (token lexeme lexeme)]))

(define (tokenize ip)
  (port-count-lines! ip)
  (lambda () (cql-lexer ip)))

(module+ test
  (require rackunit brag/support)
  ;; drain a token stream into a list of (type . value)
  (define (lex str)
    (define next (tokenize (open-input-string str)))
    (let loop ([acc '()])
      (define t (next))
      (cond
        [(void? t) (reverse acc)]
        [(eq? (token-struct-type t) 'WS) (loop acc)]
        [else (loop (cons (cons (token-struct-type t) (token-struct-val t)) acc))])))
  ;; keyword/operator token types are interned to symbols by brag's `token`; `defined` stays an identifier
  (check-equal? (map car (lex "define X: 1 + 2"))
                (list 'define 'IDENTIFIER ': 'NUMBER '+ 'NUMBER))
  (check-equal? (map car (lex "defined")) (list 'IDENTIFIER))
  ;; long vs number vs decimal
  (check-equal? (lex "42L") (list (cons 'LONGNUMBER (long-lit "42"))))
  (check-equal? (lex "3.14") (list (cons 'NUMBER "3.14")))
  ;; strings single-quoted, delimited identifiers double-quoted
  (check-equal? (lex "'hi'") (list (cons 'STRING "hi")))
  (check-equal? (lex "\"my id\"") (list (cons 'IDENTIFIER '|my id|)))
  ;; null / booleans
  (check-equal? (map car (lex "null true false")) (list 'null 'BOOL 'BOOL))
  ;; set operators are keywords
  (check-equal? (map car (lex "a union b")) (list 'IDENTIFIER 'union 'IDENTIFIER)))
