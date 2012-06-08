;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname islinterpreter) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; Course on "Konzepte der Programmiersprachen"
; by Klaus Ostermann, University of Marburg

; Interpreter for the core language of ISL+

; Setzen sie unter "Sprache auswählen" die Ausgabenotation "quasiquote"


;;;;;;;;;;;;;;;;;;;;
; Syntax of ISL+
;;;;;;;;;;;;;;;;;;;;

(define-struct var-def (name e))
; a Var-Def is: (make-var-def String Exp)
(define-struct struct-def (name fields))
; a Struct-Def is: (make-struct-def Symbol (list-of Symbol))

; 
; a Def is either:
; - a Var-Def
; - a Struct-Def

(define-struct app (fun args))
(define-struct lam (args body))
(define-struct var (x))
(define-struct locl (defs e))
(define-struct structv (def fieldvalues))
(define-struct closure (args body env))
(define-struct cnd (clauses))
(define-struct cnd-clause (c e))
(define-struct primval (v))



; a PrimVal is either:
; - a Number
; - a String
; - a Boolean


; an Exp is either:
; - (make-app Exp (list-of Exp))
; - (make-lam (list-of Symbol) Exp)
; - (make-var Symbol)
; - (make-locl (list-of Def) Exp)
; - (make-cnd (list-of (make-cnd-clause Exp Exp))
; - (make-primval PrimVal)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parsing and printing of programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; s-exp -> Exp
; parses an s-expression to the abstract syntax tree
(define (parse sexp)
  (if (cons? sexp)
      (cond
        [(equal? (first sexp) 'lambda) 
         (make-lam (second sexp) (parse (third sexp)))]
        ((equal? (first sexp) 'define-struct) 
         (make-struct-def (second sexp) (third sexp)))
        [(equal? (first sexp) 'define) 
         (make-var-def (second sexp) (parse (third sexp)))]
        [(equal? (first sexp) 'local) 
         (make-locl (map parse (second sexp)) (parse (third sexp)))]
        [(equal? (first sexp) 'cond) 
         (make-cnd (map (lambda (cl) 
                          (make-cnd-clause 
                           (parse (first cl)) 
                           (parse (second cl)))) 
                        (rest sexp)))]
        [else (make-app (parse (first sexp)) (map parse (rest sexp)))])
      (if (symbol? sexp)
          (make-var sexp)
          (make-primval sexp))))

; Exp -> s-exp
; prints an abstract syntax tree to an s-expression
; for all expressions exp, the following property should hold:
;    (parse (print exp)) == exp
(define (print exp)
  (cond [(lam? exp) `(lambda ,(lam-args exp) ,(print (lam-body exp))) ]
        [(struct-def? exp) `(define-struct ,(struct-def-name exp) ,(struct-def-fields exp))]
        [(var-def? exp) `(define ,(var-def-name exp) ,(print (var-def-e exp)))]
        [(locl? exp)  (list 'local (map print (locl-defs exp)) (print (locl-e exp)))]
        [(cnd? exp) (cons 'cond (map (lambda (cc) (list (print (cnd-clause-c cc)) (print (cnd-clause-e cc)))) (cnd-clauses exp)))]
        [(app? exp) (cons (print (app-fun exp)) (map print (app-args exp)))]
        [(var? exp) (var-x exp)]
        [(closure? exp) (list 'closure (closure-args exp) (print (closure-body exp)) (map print (closure-env exp)))]
        [(primval? exp) (if (procedure? (primval-v exp))
                            '<primfun>
                            (primval-v exp))]
        [else exp]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Runtime entities: Values and Environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; a Value is the set of all values v for which (value? v) 

; Exp -> Boolean
(define (value? e)
  (or (closure? e) (primval? e) (structv? e)))

; (list-of Exp) -> Boolean
; are all elements in the list values?
(define (all-value? es)
  (foldr (lambda (v b) (and (value? v) b)) true es)) 


; an Env is the set of all values v for which (env? v) = true
(define (env? env)
  (or
   (empty? env)
   (and
    (var-def? (first env))
    (value? (var-def-e (first env)))
    (env? (rest env)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reduction semantics of ISL+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exp Env -> Exp
; reduces an expression in an environment
(define (reduce e env)
  (cond 
    ; expression has the form (v-1 v-2 ... v-n)?
    [(and (app? e) (value? (app-fun e)) (all-value? (app-args e))) 
     ; then call auxiliary function to deal with this case
     (reduce-app (app-fun e) (app-args e))]
    
    ; expression has the form (lambda (x-1 ... x-n) exp)?
    [(lam? e) 
     ; then create closure and store current environment
     (make-closure (lam-args e) (lam-body e) env)] 
    
    ; expression has the form x?
    [(var? e) 
     ; Then look it up in the environment
     (lookup-env env (var-x e))] 
    
    ; expression has the form (v-0 v-1 ... e-i ... e-n)? 
    [(and (app? e) (value? (app-fun e))) 
     ; reduce leftmost non-value
     (make-app (app-fun e) (reduce-first (app-args e) env))] 
    
    ; expression has the form (e-0 ... e-n)?
    [(app? e) 
     ; then reduce function argument  
     (make-app (reduce (app-fun e) env) (app-args e) )] 
    
    ; expression has the form (local [(define x-1 v1)...] v)?
    [(and (locl? e) (env? (locl-defs e)) (value? (locl-e e))) 
     ; then reduce to v 
     (locl-e e)] 
    
    ; expression has the form (local [(define x-1 v1)...] e)?  
    [(and (locl? e) (env? (locl-defs e))) 
     ; then reduce e 
     (make-locl (locl-defs e) (reduce (locl-e e) (append (locl-defs e) env)))] 
    
    ; expression has the form (local [(define x-1 v1)...(define x-i e-i) ...] e) ?
    [(locl? e) 
     ; then reduce left-most e-i which is not a value 
     (make-locl (reduce-first-def (locl-defs e) env) (locl-e e))] 
    
    ; expression has the form (cond [(v e) rest])?
    [(and (cnd? e) (value? (cnd-clause-c (first (cnd-clauses e)))))
     ; check if v is true or v is false
     (if (primval-v (cnd-clause-c (first (cnd-clauses e)))) 
         (cnd-clause-e (first (cnd-clauses e))) ; if true reduce to e 
         (make-cnd (rest (cnd-clauses e))))] ; if false reduce to (cond [rest])
    
    ; expression has the form (cond [(e-1 e-2) rest]) and e-1 is not a value?
    [(cnd? e) 
     ; then reduce e-1
     (make-cnd
      (cons
       (make-cnd-clause
        (reduce (cnd-clause-c (first (cnd-clauses e))) env)
        (cnd-clause-e (first (cnd-clauses e))))
       (rest (cnd-clauses e))))]
    
    [else (error "Cannot reduce: " e)]))

; (list-of Def) Env -> (list-of Def)
(define (reduce-first-def defs env)
  (cond [(struct-def? (first defs))
         (append
          (make-struct-funcs (first defs))
          (rest defs))]
        [(value? (var-def-e (first defs)))
         (cons (first defs) (reduce-first-def (rest defs) (cons (first defs) env)))]
        [else (cons (make-var-def (var-def-name (first defs))
                                  (reduce (var-def-e (first defs)) env))
                    (rest defs))]))


; Env Symbol -> Value
(define (lookup-env env x)
  (local 
    [(define (lookup-env-helper env x)
       (cond [(empty? env) 
              (error (string-append "Unbound variable: " (symbol->string x)))]
             [(and (var-def? (first env))
                   (eq? (var-def-name (first env)) x))
              (var-def-e (first env))]
             [else (lookup-env-helper (rest env) x)]))]
    (lookup-env-helper (append env initial-env) x)))

; (list-of Exp) -> (list-of Exp)
; reduces first non-value of es
(define (reduce-first es env)
  (if (value? (first es))
      (cons (first es) (reduce-first (rest es) env))
      (cons (reduce (first es) env) (rest es))))

; Value (list-of Value) -> Exp
(define (reduce-app fun args)
  (cond [(closure? fun) 
         (make-locl (closure-env fun) 
                    (make-locl
                     (map (lambda (x v) (make-var-def x v)) (closure-args fun) args)
                     (closure-body fun)))]
        [(primval? fun) 
         (make-primval (apply (primval-v fun) (map primval-v args)))]))


; Struct-Def -> Env
; creates the constructor, selector, and predicate functions for a structure definition sd
; and represents them as an environment
(define (make-struct-funcs sd)
  ; assume as example that sd is (define-struct a (b c))
  (local 
    [(define name (symbol->string (struct-def-name sd))) ; then name = "a"
     (define (gen-selector fn)  ; generates selector function for each field name fn
       (make-var-def 
        (string->symbol (string-append name "-" (symbol->string fn))) ; such as "a-b" and "a-c"
        (make-primval 
         (lambda (sv)
           (if (and (structv? sv) 
                    ; eq? is reference equality: Structures are compatible if they 
                    ; stem from the same place in the program
                    (eq? (structv-def sv) sd)) 
               (second (assq fn                     ; extract corresponding field value
                             (map 
                              list 
                              (struct-def-fields sd) 
                              (structv-fieldvalues sv))))
               (error "Argument of selector function for " fn " must be structure value of structure " name))))))]
    (cons
     (make-var-def  ; constructor function
      (string->symbol (string-append "make-" name)) ; name of constructor function, such as 'make-a
      (make-primval 
       (compose ; compose with "list" function to turn it into an n-ary function 
        ; where n = (length (struct-def-fields sd))
        (lambda (fv)
          (if (= (length fv) ; check that number of args matches number of fields
                 (length (struct-def-fields sd)))
              (make-structv sd fv) ; everything OK -> create structure value
              (error "Wrong number of args in constructor call for struct: " 
                     (struct-def-name sd))))
        list)))
     (cons (make-var-def ; predicate function
            (string->symbol (string-append name "?")) ; such as 'a?
            (make-primval (lambda (v)
                            (and (structv? v)
                                 (eq? (structv-def v) sd)))))
           (map gen-selector (struct-def-fields sd)) ; create selector function for each field
           ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infrastructure for executing programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; we use the initial environment to encode the primitive values and functions
; can be extended as needed
(define initial-env 
  (map (lambda (x)
         (make-var-def (first x) (make-primval (second x))))
       (list
        (list '+ +)
        (list '- -)
        (list '* *)
        (list 'zero? zero?)
        (list 'cons cons)
        (list 'empty empty)
        (list 'first first)
        (list 'rest rest)
        (list 'true true)
        (list 'false false))))

; Exp -> Value
; reduces expression until it becomes a value (or loops)
(define (eval e)
  (if (value? e) 
      e
      (eval (reduce e empty))))

; S-Exp -> Value
; like eval, but parses input first
(define (parse-and-eval sexp)
  (eval (parse sexp)))

; Exp -> (list-of Exp)
; generates sequence of reduction steps until e becomes a value
(define (reduction-sequence e)
  (if (value? e)
      (list (print e))
      (cons (print e) (reduction-sequence (reduce e empty)))))

; s-exp -> (list-of-Exp)
; like reduction-sequence, but parses input first
(define parse-and-reduce-sequence (compose reduction-sequence parse))


;;;;;;;;;;
; Tests  ;
;;;;;;;;;;

; parser tests

(check-expect (parse '(local [(define x (+ 1 2)) (define y (lambda (z) z)) (define-struct a (b c))] (+ x (f 7))))
              (make-locl
               (list
                (make-var-def 'x (make-app (make-var '+) (list (make-primval 1) (make-primval 2))))
                (make-var-def 'y (make-lam (list 'z) (make-var 'z)))
                (make-struct-def 'a (list 'b 'c)))
               (make-app (make-var '+) (list (make-var 'x) (make-app (make-var 'f) (list (make-primval 7)))))))
(check-expect (parse '(cond [(a? x) (+ b 2)] [55 19]))
              (make-cnd
               (list
                (make-cnd-clause (make-app (make-var 'a?) (list (make-var 'x))) (make-app (make-var '+) (list (make-var 'b) (make-primval 2))))
                (make-cnd-clause (make-primval 55) (make-primval 19)))))

; printer tests
(check-expect (print (parse '(local [(define fact (lambda (n) (cond [(zero? n) 1]
                                                                    [true (* n (fact (- n 1)))])))]
                               (fact 5))))
              `(local ((define fact (lambda (n) (cond ((zero? n) 1) (true (* n (fact (- n 1)))))))) (fact 5)))
(check-expect (print (parse '(local [(define-struct a (b c))] (a-b 55))))
              '(local ((define-struct a (b c))) (a-b 55)))

; reduction tests
(check-expect (reduce (reduce (parse '((lambda (x) (+ x 5)) 7)) empty) empty)
              (make-locl
               empty
               (make-locl (list (make-var-def 'x (make-primval 7))) (make-app (make-var '+) (list (make-var 'x) (make-primval 5))))))


; evaluator test
(check-expect (parse-and-eval '(+ 1 2)) (make-primval 3))
(check-expect (parse-and-eval '(+ (+ 1 2) (+ 3 4))) (make-primval 10))
(check-expect (parse-and-eval '((lambda (x) (+ x (+ 1 2))) (+ 2 3))) (make-primval 8))
(check-expect (parse-and-eval '(local [(define x (+ 1 2))] x)) (make-primval 3))
(check-expect (parse-and-eval '(cond [(zero? (+ 1 2)) (+ 3 4)] [(zero? 0) (+ 5 6)])) (make-primval 11))
(check-expect (parse-and-eval '(local [(define fact (lambda (n) (cond [(zero? n) 1]
                                                                      [true (* n (fact (- n 1)))])))]
                                 (fact 5))) (make-primval 120))


(check-expect (parse-and-eval '(local [(define x 5) (define y (+ x 6))] (+ x y))) 
              (make-primval 16))
(check-expect (parse-and-eval '(local [(define-struct a (b c))] (a? (make-a (+ 1 2) (+ 3 4)))))
              (make-primval true))
(check-expect (parse-and-eval '(local [(define-struct a (b c))] (a-c (make-a (+ 1 2) (+ 3 4)))))
              (make-primval 7))

(check-expect (parse-and-eval '(local [(define x 5)
                                       (define f (lambda (y) (+ x y)))]
                                 (local [(define x 12)]
                                   (f x))))
              (make-primval 17))

; check that equal structure definitions are not compatible
(check-error (parse-and-eval '(local [(define-struct a (b c))
                                      (define v (make-a 1 2))]
                                (local [(define-struct a (b c))]
                                  (a-b v)))))

(check-expect (parse-and-reduce-sequence '(local [(define x 5) (define y (+ x 6))] (+ x y)))
              '((local ((define x 5) (define y (+ x 6))) (+ x y))
                (local ((define x 5) (define y (<primfun> x 6))) (+ x y))
                (local ((define x 5) (define y (<primfun> 5 6))) (+ x y))
                (local ((define x 5) (define y 11)) (+ x y))
                (local ((define x 5) (define y 11)) (<primfun> x y))
                (local ((define x 5) (define y 11)) (<primfun> 5 y))
                (local ((define x 5) (define y 11)) (<primfun> 5 11))
                (local ((define x 5) (define y 11)) 16)
                16))
(check-expect 
 (parse-and-reduce-sequence  '(local [(define x 5)
                                      (define f (lambda (y) (+ x y)))]
                                (local [(define x 12)]
                                  (f x))))
 '((local ((define x 5) (define f (lambda (y) (+ x y)))) (local ((define x 12)) (f x)))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) (f x)))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) ((closure (y) (+ x y) ((define x 5))) x)))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) ((closure (y) (+ x y) ((define x 5))) 12)))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) (local ((define x 5)) (local ((define y 12)) (+ x y)))))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) (local ((define x 5)) (local ((define y 12)) (<primfun> x y)))))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) (local ((define x 5)) (local ((define y 12)) (<primfun> 5 y)))))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) (local ((define x 5)) (local ((define y 12)) (<primfun> 5 12)))))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) (local ((define x 5)) (local ((define y 12)) 17))))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) (local ((define x 5)) 17)))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) (local ((define x 12)) 17))
   (local ((define x 5) (define f (closure (y) (+ x y) ((define x 5))))) 17)
   17))              