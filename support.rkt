#lang racket/base

;;; support.rkt — what CQL offers the family's editor and agent hubs (fhir/lang/support): the
;;; diagnostics of a library (parse, then every definition translated), the libraries beside it, its
;;; declarations as an outline, hover and go to definition (an include's alias leads to the library's
;;; file), and completion of declared names and keywords. Declarations are read from the text, not a
;;; parse, so they work while the library does not parse yet. What an agent runs is workspace.rkt.

(require racket/contract racket/string racket/list
         fhir/lang/support fhir/lang/declared
         (only-in fhir/result/result ok? err? ok-value err-error)
         (only-in "parser/parse.rkt" parse-cql)
         (only-in "compile.rkt" compile-library/diagnostics)
         (only-in "error.rkt" cql-error-diagnostic cql-error-message cql-error-phase))

(provide (contract-out [support lang-support?]))

(define (path-string p) (if (path? p) (path->string p) p))

;; diagnose : text path -> diagnostics   parse, then translate every definition; the errors, each
;; positioned when it carries a position
(define (diagnose text path)
  (define (->diagnostic e)
    (or (cql-error-diagnostic e)
        (positionless 'cql (case (cql-error-phase e) [(translate) 'compile] [(interpret) 'run] [else (cql-error-phase e)])
                      (cql-error-message e) path)))
  (define r (parse-cql (blank-lang-line text) #:source (path-string path)))
  (cond
    [(err? r) (list (->diagnostic (err-error r)))]
    [else (let-values ([(elm errors warnings) (compile-library/diagnostics (ok-value r))])
            (append (map ->diagnostic errors) warnings))]))

;; the declarations: a line regexp per kind; an include is its alias, its id the library it names
(define declarations
  (list (list "library"    #px"^library\\s+(\\S+)")
        (list "include"    #px"^include\\s+(\\S+)(?:\\s+version\\s+'[^']*')?\\s+called\\s+(\\S+)" (lambda (gs) (cons (cadr gs) (car gs))))
        (list "include"    #px"^include\\s+(\\S+)\\s*$" (lambda (gs) (cons (car gs) (car gs))))
        (list "using"      #px"^using\\s+(\\S+)")
        (list "valueset"   #px"^(?:public\\s+|private\\s+)?valueset\\s+\"([^\"]+)\"")
        (list "codesystem" #px"^(?:public\\s+|private\\s+)?codesystem\\s+\"([^\"]+)\"")
        (list "code"       #px"^(?:public\\s+|private\\s+)?code\\s+\"([^\"]+)\"")
        (list "concept"    #px"^(?:public\\s+|private\\s+)?concept\\s+\"([^\"]+)\"")
        (list "parameter"  #px"^(?:public\\s+|private\\s+)?parameter\\s+(?:\"([^\"]+)\"|(\\w+))")
        (list "context"    #px"^context\\s+(\\w+)")
        (list "define"     #px"^(?:public\\s+|private\\s+)?define\\s+(?:fluent\\s+)?(?:function\\s+)?(?:\"([^\"]+)\"|(\\w+))")))

(define keywords
  '("library" "using" "include" "called" "version" "public" "private" "valueset" "codesystem" "code"
    "concept" "parameter" "default" "context" "define" "function" "fluent" "returns" "from" "where"
    "return" "sort" "by" "asc" "desc" "with" "without" "such" "that" "let" "aggregate" "all" "distinct"
    "exists" "not" "and" "or" "xor" "implies" "is" "as" "cast" "if" "then" "else" "case" "when" "end"
    "in" "contains" "between" "properly" "includes" "during" "before" "after" "starts" "ends" "same"
    "overlaps" "meets" "union" "intersect" "except" "null" "true" "false" "Interval" "List" "Tuple"
    "Code" "Concept" "Quantity" "Integer" "Decimal" "String" "Boolean" "Date" "DateTime" "Time"
    "Patient" "Population" "Unfiltered" "Practitioner"))

;; CompletionItemKind by declaration kind
(define (item-kind kind)
  (case kind [("define") 3] [("library" "include" "using") 9] [("valueset" "codesystem" "code" "concept") 12]
             [("parameter" "context") 6] [else 1]))

(define (entities text file) (declared-entities declarations text file))
(define (project-files path) (files-beside path '("cql")))

;; definition : project text position -> entity|#f   the declaration the name refers to; an include
;; alias leads to the library it names when that library is in the project
(define (definition pr text pos)
  (define n (name-at text pos #:quoted? #t))
  (define es (project-entities pr))
  ;; a qualified name (`C.X`): the whole, else its library alias
  (define e (and n (or (find-entity es n) (find-entity es (car (string-split n "."))))))
  (cond
    [(and e (equal? (entity-kind e) "include"))
     (or (findf (lambda (x) (and (equal? (entity-kind x) "library") (equal? (entity-name x) (entity-id e)))) es) e)]
    [else e]))
(define (hover pr text pos)
  (define e (definition pr text pos))
  (and e (declaration-hover e ((project-text-of pr) (entity-file e)) "cql")))
(define (completion pr text pos) (prefix-completions (project-entities pr) keywords item-kind text pos))

(define support (lang-support "cql" diagnose project-files entities hover definition completion))

(module+ test
  (require rackunit racket/file)
  (define cql "library L version '1'\nusing FHIR version '4.0.1'\ninclude Common version '1' called C\nvalueset \"VS\": 'http://x'\nparameter \"P\" Integer\ncontext Patient\ndefine \"A B\": 1\ndefine function f(x Integer): x\ndefine D: \"A B\" + C.X\n")
  (define es (entities cql "/tmp/nowhere/l.cql"))
  (check-equal? (map entity-name es) '("L" "FHIR" "C" "VS" "P" "Patient" "A B" "f" "D"))
  (check-equal? (entity-id (find-entity es "C")) "Common")
  ;; a project with the included library beside: the alias leads to it
  (define common "library Common version '1'\ndefine X: 2\n")
  (define pr (project "/tmp/nowhere/l.cql" (append es (entities common "/tmp/nowhere/Common.cql"))
                      (lambda (f) (if (regexp-match? #rx"Common" (path-string f)) common cql))))
  (check-equal? (entity-file (definition pr cql (cons 8 18))) "/tmp/nowhere/Common.cql")
  (check-equal? (entity-line (definition pr cql (cons 8 12))) 6)                  ; "A B"
  (check-regexp-match #rx"define \"A B\"" (hover pr cql (cons 8 12)))
  (check-equal? (map (lambda (i) (hash-ref i 'label)) (completion pr "define E: \"A" (cons 0 12))) '("A B"))
  ;; diagnostics: a library that does not parse, one whose definition does not translate
  (check-true (pair? (diagnose "library L\ndefine X: 1 +\n" "/tmp/nowhere/bad.cql")))
  (check-equal? (diagnose "library L\ndefine X: 1 + 2\n" "/tmp/nowhere/ok.cql") '())
  (define dir (make-temporary-file "cql-support-~a" 'directory))
  (with-output-to-file (build-path dir "a.cql") (lambda () (display "library a\n")))
  (check-equal? (length (project-files (build-path dir "x.cql"))) 1)
  (delete-directory/files dir))
