#lang racket

(define program-counter null)
(define registers '())

(define lua-keywords
  '(and break do else elseif
        end false for function
        if in local nil not
        or repeat return then
        true until while))

; file->list : String -> List-of-String
(define file->list
  (λ (file-name)
    (letrec [(file->list
              (λ (file)
                (let [(text (read file))]
                  (if (eq? eof text)
                      (begin (close-input-port file) null)
                      (cons text (file->list file))))))]
      (file->list (open-input-file file-name)))))

; value-append : (String | Symbol | Number)* -> String
(define value-append
  (λ args
    (apply
     string-append
     (map (λ (item)
            (cond
              [(symbol? item) (symbol->string item)]
              [(number? item) (number->string item)]
              [(char? item) (string item)]
              [else item])) args))))

; safe : Symbol -> String
(define safe
  (λ (exp)
    (cond
      [(memv exp lua-keywords) (safe (string->symbol (value-append exp "_safe")))]
      [else
       (letrec [(safe-char
                 (λ (chars)
                   (cond
                     [(null? chars) '()]
                     [else
                      (cons
                       (match (car chars)
                         (#\? "_huh")
                         (#\! "_bang")
                         (#\. "_dot")
                         (#\+ "_plus")
                         (#\- "_dash")
                         (#\* "_")
                         (#\/ "_slash")
                         (#\< "_lt")
                         (#\> "_gt")
                         (#\: "_colon")
                         (#\$ "_")
                         (#\% "_pct")
                         (#\^ "_hat")
                         (#\& "_amp")
                         (#\~ "_")
                         (#\_ "_")
                         (c #:when (char-numeric? c) (value-append "r" c))
                         (c (string c))) (safe-char (cdr chars)))])))] (apply value-append (safe-char (string->list (if (symbol? exp) (symbol->string exp) exp)))))])))

; tabs : Number -> String
(define tabs (λ (n) (make-string (* n 2) #\space)))

; union-defs : String List-of-Symbol -> List-of-String
(define union-defs
  (λ (name cases)
    (match cases
      ['() '()]
      [`((,case-name . ,parameters) . ,rest) (cons (let [(fname (safe (value-append name "_" case-name)))]
                                                     (let [(parameters (map safe (map symbol->string parameters)))]
                                                       (value-append "function "
                                                                     fname
                                                                     "(" (string-join parameters ", ") ")\n"
                                                                     (tabs 1) "return {\n"
                                                                     (tabs 2) "tag = \"" (safe case-name) "\",\n"
                                                                     (tabs 2) "values = {" (string-join parameters  ", ") "}\n"
                                                                     (tabs 1) "}\n"
                                                                     "end"))) (union-defs name rest))])))

; convert-format : List-of-Character -> List-of-Character
(define convert-format
  (λ (s)
    (cond
      [(null? s) null]
      [(char=? #\newline (car s)) (cons #\\ (cons #\n (convert-format (cdr s))))]
      [(null? (cdr s)) (cons (car s) '())]
      [(and (char=? #\~ (car s)) (or (char=? #\s (cadr s)) (char=? #\a (cadr s)))) (cons #\% (cons #\d (convert-format (cddr s))))]
      [else (cons (car s) (convert-format (cdr s)))])))

(define parse-conditionals
  (lambda (exp level after-first? last?)
    (cond [(null? exp) ""]
          [else (match exp
                  [`(cond . ,rest) (parse-conditionals rest level after-first? last?)]
                  [`((,cond ,conseq) . ,rest) (string-append
                                               (tabs level)
                                               (string-join
                                                (filter
                                                 (λ (s) (> (string-length s) 0))
                                                 (list (if (or last? (eqv? cond 'else)) "else" (if after-first? "elseif" "if"))
                                                       (if (eqv? cond 'else) "" (parse-expression cond level #t))
                                                       (if (eqv? cond 'else) "\n" "then\n"))) " ")
                                               (parse-expression conseq (add1 level) #f)
                                               "\n"
                                               (if last? (tabs level) "") (if last? "end" "")
                                               (parse-conditionals rest level #t (if (not (null? rest)) (null? (cdr rest)) #f)))])])))

; parse-expressions : List-of-Symbol -> List-of-String
(define parse-expression
  (λ (expression level expression?)
    (match expression
      [b #:when (boolean? b) (value-append (if expression? "" (tabs level)) (if b "true" "false"))]
      [n #:when (number? n) (value-append (if expression? "" (tabs level)) (number->string n))]
      [s #:when (symbol? s) (value-append (if expression? "" (tabs level)) (safe s))]
      [`(define ,formal ,e) (value-append (if expression? "" (tabs level)) "local " (safe formal) " = " (parse-expression e level #t))]
      [`(mount-trampoline ,continuation-type ,continuation-reg ,_) (value-append
                                                                    (if expression? "" (tabs level)) (safe continuation-reg) " = " continuation-type "(\":3\")\n"
                                                                    (if expression? "" (tabs level)) "mount_trampoline()")]
      [`(dismount-trampoline ,_) (value-append (if expression? "" (tabs level)) "dismount_trampoline()")]
      [`(zero? ,n) (value-append (if expression? "" (tabs level)) "(" (parse-expression n level expression?) " == 0)")]
      [`(not ,e) (value-append (if expression? "" (tabs level)) "(not " (parse-expression e level expression?) ")")]
      [`(sub1 ,n) (value-append (if expression? "" (tabs level)) "(" (parse-expression n level expression?) " - 1)")]
      [`(add1 ,n) (value-append (if expression? "" (tabs level)) "(" (parse-expression n level expression?) " + 1)")]
      [`(error ,s) (value-append (if expression? "" (tabs level)) "error(\"" s "\")")]
      [`(error ',s-name ,s) (value-append (if expression? "" (tabs level)) "error(\"" s-name "(" s ")\")")]
      [`(random ,n) (value-append (if expression? "" (tabs level)) "math.random(0, " n ")")]
      [`(printf ,s . ,params) (value-append (if expression? "" (tabs level)) "io.write(string.format(\""
                                            (list->string (convert-format (string->list s))) "\"" (if (null? params) "" ", ")
                                            (string-join (map (λ (expression) (parse-expression expression level #t)) params) ", ") "))")]
      [`(+ ,n1 ,n2) (value-append (if expression? "" (tabs level)) "(" (parse-expression n1 level expression?) " + " (parse-expression n2 level expression?) ")")]
      [`(- ,n1 ,n2) (value-append (if expression? "" (tabs level)) "(" (parse-expression n1 level expression?) " - " (parse-expression n2 level expression?) ")")]
      [`(* ,n1 ,n2) (value-append (if expression? "" (tabs level)) "(" (parse-expression n1 level expression?) " * " (parse-expression n2 level expression?) ")")]
      [`(/ ,n1 ,n2) (value-append (if expression? "" (tabs level)) "(" (parse-expression n1 level expression?) " / " (parse-expression n2 level expression?) ")")]
      [`(and ,e1 ,e2) (value-append (if expression? "" (tabs level)) "(" (parse-expression e1 level expression?) " and " (parse-expression e2 level expression?) ")")]
      [`(or ,e1 ,e2) (value-append (if expression? "" (tabs level)) "(" (parse-expression e1 level expression?) " or " (parse-expression e2 level expression?) ")")]
      [`(< ,n1 ,n2) (value-append (if expression? "" (tabs level)) "(" (parse-expression n1 level expression?) " < " (parse-expression n2 level expression?) ")")]
      [`(> ,n1 ,n2) (value-append (if expression? "" (tabs level)) "(" (parse-expression n1 level expression?) " > " (parse-expression n2 level expression?) ")")]
      [`(<= ,n1 ,n2) (value-append (if expression? "" (tabs level)) "(" (parse-expression n1 level expression?) " <= " (parse-expression n2 level expression?) ")")]
      [`(>= ,n1 ,n2) (value-append (if expression? "" (tabs level)) "(" (parse-expression n1 level expression?) " >= " (parse-expression n2 level expression?) ")")]
      [`(if ,condition ,consequent ,alternative) (value-append
                                                  "if " (parse-expression condition level #t) " then\n"
                                                  (parse-expression consequent (add1 level) #f) "\n"
                                                  "else\n"
                                                  (parse-expression alternative (add1 level) #f) "\n"
                                                  "end")]
      [`(cond . ,_) (parse-conditionals expression level #f #f)]
      ;; [`(cond . ,conditions)
      ;;  (let loop [(conditions conditions)]
      ;;    (cond
      ;;      [(null? conditions) (value-append (tabs level) "end")]
      ;;      [else (cons (match ) (loop (cdr conditions))
      ;;                  ]))]
      [`(begin . ,expressions) (string-join (map (λ (expression) (parse-expression expression level #f)) expressions) "\n")]
      [`(set! ,formal ,value) (value-append (if expression? "" (tabs level)) (safe formal) " = " (parse-expression value level #t))]
      [`(union-case ,reg ,_ . ,cases) (string-join (parse-cases (safe reg) cases level) "\n")]
      (`(let ,bindings ,e)
       (value-append
        (string-join (map
                      (λ (binding)
                        (match binding
                          [`(,formal ,expression)
                           (value-append (if expression? "" (tabs level)) "local " (safe formal) " = " (parse-expression expression level #t))])) bindings) "\n") "\n" (parse-expression e level #f) "\n"))
      [`(,func . ,args) (value-append (safe func) "(" (string-join (map (λ (expression) (parse-expression expression level #t)) args) ", ") ")")]
      )))

; parse-function : Symbol List-of-Symbol -> String
(define parse-function
  (λ (function expressions)
    (value-append
     "function "
     (safe function)
     "()\n"
     (string-join (map (λ (expression) (parse-expression expression 1 #f)) expressions) "\n")
     "\n"
     "end"
     (if (eqv? function 'main) "\n\nmain()" ""))))

(define parse-union-case
  (λ (cases level)
    (match cases
      [`(union-case ,register ,_ . ,cases) (parse-cases (safe register) cases level)])))

; parse-cases : Symbol Symbol List-of-Symbol : List-of-String
(define parse-cases
  (λ (register cases level)
    (cond
      [(null? cases) (list "")]
      [else
       (match (car cases)
         [`((,tag . ,values) ,expression)
          (cons (let ([tag (safe tag)])
                  (string-join (filter
                                (λ (s) (> (string-length s) 0))
                                `(
                                  ,(value-append (tabs level) "if " register ".tag == \"" tag "\" then")
                                  ,(value-append
                                    (string-join
                                     (let loop [(values values) (n 1)]
                                       (cond
                                         [(null? values) '()]
                                         [else (cons
                                                (value-append
                                                 (tabs (add1 level))
                                                 "local " (safe (car values)) " = " register ".values[" n "]")
                                                (loop (cdr values) (add1 n)))])) "\n"))
                                  ,(parse-expression expression (add1 level) #f)
                                  ,(value-append (tabs (add1 level)) "return")
                                  ,(value-append (tabs level) "end"))) "\n")
                  ) (parse-cases register (cdr cases) level))])])
    #|(match cases
      [`(union-case ,register ,name . ,cases)
       (let loop [(case cases) (n 0)]
         (cond
           [(null? case) ""]
           [else (match case
                   [`(,tag . ,_) (value-append
                                  "if " ".tag == " (safe tag) " then\n"
                                  (let loop [(n 0)])
                                  "" (loop (cdr case) (add1 n)))])]))])|#
    ))

;; (display (parse-union-case
;;           '(union-case
;;             *m* ll
;;             [(link v rest)
;;              (begin
;;                (set! *m* rest)
;;                (set! *pc* process))]
;;             [(empty)
;;              (begin
;;                (set! *pc* process-k))]) 1))

;; (let loop [(values '(v rest)) (n 1)]
;;   (cond
;;     [(null? values) '()]
;;     [else (cons (value-append "local " (safe (car values)) " = " "namehere.values[" n "]") (loop (cdr values) (add1 n)))]
;;     ))

(define parse-header
  (λ (lines)
    (cond
      [(null? lines) (cons (value-append
                            "function mount_trampoline()\n"
                            (tabs 1) "while " program-counter " ~= nil do\n"
                            (tabs 2) program-counter "()\n"
                            (tabs 1) "end\n"
                            "end\n\n"
                            "function dismount_trampoline()\n"
                            (tabs 1) program-counter " = nil\n"
                            (tabs 1) "return nil\n"
                            "end\n") null)]
      [else (match (car lines)
              [`(define-registers . ,regs) (set! registers (map safe regs))
                                           (cons (string-join (map (λ (reg) (value-append "local " reg " = nil")) (map safe regs)) "\n") (parse-header (cdr lines)))]
              [`(define-program-counter ,pc-reg) (let [(pc (safe pc-reg))]
                                                   (set! program-counter pc)
                                                   (cons (value-append "local " pc " = nil") (parse-header (cdr lines))))]
              [_ (parse-header (cdr lines))]

              )])))

(define parse-body
  (λ (lines)
    (cond
      [(null? lines) '()]
      [else
       ;; (display (car lines))
       (match (car lines)
         [`(define-union ,name . ,cases) (cons (string-join (union-defs name cases) "\n\n") (parse-body (cdr lines)))]
         [`(define-label ,name . ,function-lines) (cons (parse-function name function-lines) (parse-body (cdr lines)))]
         [_ (parse-body (cdr lines))]
         )])))

(define pc->lua
  (λ (lines)
    (value-append (string-join (parse-header lines) "\n\n") "\n" (string-join (parse-body lines) "\n\n"))))
