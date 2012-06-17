;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname islinterpreterwostructs) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp"))) (htdp-settings #(#t quasiquote repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp")))))
; Course on "Konzepte der Programmiersprachen"
; by Klaus Ostermann, University of Marburg

; Interpreter for the core language of ISL+

; Setzen sie unter "Sprache auswählen" die Ausgabenotation "quasiquote"


;;;;;;;;;;;;;;;;;;;;
; Syntax of ISL+
;;;;;;;;;;;;;;;;;;;;

(define-struct var-def (name e))
; a Var-Def is: (make-var-def String Exp)

(define-struct app (fun args))
(define-struct lam (args body))
(define-struct var (x))
(define-struct locl (defs e))
(define-struct closure (args body env))
(define-struct cnd (clauses))
(define-struct cnd-clause (c e))
(define-struct primval (v))

; a Value is either
; - (make-primval Any)
; - (make-closure (list-of Symbol) Exp Env)

; an Exp is either:
; - a Value
; - (make-app Exp (list-of Exp))
; - (make-lam (list-of Symbol) Exp)
; - (make-var Symbol)
; - (make-locl (list-of Var-Def) Exp)
; - (make-cnd (list-of (make-cnd-clause Exp Exp))
;
; Note: the case (make-closure (list-of Symbol) Exp Env)
; must not
; occur in the original program and are only produced
; during evaluation. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parsing and printing of programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; s-exp -> Exp
; parses an s-expression to an abstract syntax tree
(define (parse sexp)
  (if (cons? sexp)
      (cond
        [(equal? (first sexp) 'lambda) 
         (make-lam (second sexp) (parse (third sexp)))]
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
; for all expressions  exp, the following property should hold:
;    (parse (print exp)) == exp
; This property does not hold if exp contains structv or closures
; because they cannot be parsed and are only introduced during reduction
(define (print exp)
  (cond [(lam? exp) 
         (list 'lambda (lam-args exp) (print (lam-body exp)))]
        [(var-def? exp) 
         (list 'define (var-def-name exp) (print (var-def-e exp)))]
        [(locl? exp)  
         (list 'local (map print (locl-defs exp)) (print (locl-e exp)))]
        [(cnd? exp) 
         (cons 'cond (map (lambda (cc) (list (print (cnd-clause-c cc)) 
                                             (print (cnd-clause-e cc)))) 
                          (cnd-clauses exp)))]
        [(app? exp) 
         (cons (print (app-fun exp)) (map print (app-args exp)))]
        [(var? exp) (var-x exp)]
        [(closure? exp) 
         (list 'closure 
               (closure-args exp) 
               (print (closure-body exp)) 
               (map print (closure-env exp)))]
        [(primval? exp) (if (procedure? (primval-v exp))
                            '<primfun>
                            (primval-v exp))]
        [else exp]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Runtime entities: Values and Environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exp -> Boolean
(define (value? e)
  (or (closure? e) (primval? e)))

; (list-of Exp) -> Boolean
; are all elements in the list values?
(define (all-value? es)
  (foldr (lambda (v b) (and (value? v) b)) true es)) 

; an Env is a (list-of (make-var-def Value))

; (list-of Var-Def) -> Boolean
; determines whether env is an Env
(define (env? env)
  (or
   (empty? env)
   (and
    (var-def? (first env))
    (value? (var-def-e (first env)))
    (env? (rest env)))))

; Env Symbol -> Value
; looks up x in (append env intial-env)
(define (lookup-env env x)
  (local 
    [(define (lookup-env-helper env x)
       (cond [(empty? env) 
              (error (string-append "Unbound variable: " 
                                    (symbol->string x)))]
             [(and (var-def? (first env))
                   (eq? (var-def-name (first env)) x))
              (var-def-e (first env))]
             [else (lookup-env-helper (rest env) x)]))]
    (lookup-env-helper (append env initial-env) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reduction semantics of ISL+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exp Env -> Exp
; reduces an expression in an environment
(define (reduce e env)
  (cond 
    [(app? e) (reduce-app (app-fun e) (app-args e) env)]
    [(lam? e) (make-closure (lam-args e) (lam-body e) env)] 
    [(var? e) (lookup-env env (var-x e))]     
    [(locl? e) (reduce-local (locl-defs e) (locl-e e) env)]
    [(cnd? e) (reduce-cond (cnd-clause-c (first (cnd-clauses e)))
                           (cnd-clause-e (first (cnd-clauses e)))
                           (rest (cnd-clauses e))
                           env)]
    [else (error "Cannot reduce: " e)]))

; Exp (list-of Exp) Env -> Exp
; reduction of an application of fun to args in env
(define (reduce-app fun args env)
  (cond 
    ; expression has the form (v-1 v-2 ... v-n)?
    [(and (value? fun) (all-value? args)) 
     (cond [(closure? fun) 
            (make-locl (closure-env fun) 
                       (make-locl
                        (map make-var-def 
                             (closure-args fun) args)
                        (closure-body fun)))]
           [(primval? fun) 
            (apply (primval-v fun) args)])]
    ; expression has the form (v-0 v-1 ... e-i ... e-n)? 
    [(value? fun) 
     ; reduce leftmost non-value
     (make-app fun (reduce-first args env))]
    [else ; expression has the form (e-0 ... e-n)?
     ; then reduce function argument  
     (make-app (reduce fun env) args )]))

; (list-of Var-Def) Exp Env -> Exp
; reduction of (local defs e) in env
(define (reduce-local defs e env)
  (cond 
    ; expression has the form (local [(define x-1 v1)...] v)?
    [(and (env? defs) (value? e)) 
     ; then reduce to v 
     e] 
    ; expression has the form (local [(define x-1 v1)...] e)?  
    [(env? defs) 
     ; then reduce e 
     (make-locl defs (reduce e (append defs env)))]
    [else ; expression has the form 
     ; (local [(define x-1 v1)...(define x-i e-i) ...] e) ?
     ; then reduce left-most e-i which is not a value 
     (make-locl (reduce-first-def defs env) e)])) 


; Exp Exp (list-of (make-cnd-clause Exp Exp) -> Exp
; reduction of a cond expression
(define (reduce-cond condition e clauses env)
  (cond 
    ; expression has the form (cond [(v e) rest])?
    [(value? condition)
     ; check if v is true or v is false
     (if (primval-v condition) 
         e ; if true reduce to e 
         (make-cnd clauses))] ; if false reduce to (cond [rest])
    [else  ; expression has the form (cond [(e-1 e-2) rest]) 
     ; and e-1 is not a value?
     ; then reduce e-1
     (make-cnd
      (cons
       (make-cnd-clause
        (reduce condition env)
        e)
       clauses))]))


; (list-of Def) Env -> (list-of Def)
; reduces the first expression which is not a value 
; in a list of definitions
(define (reduce-first-def defs env)
  (cond [(value? (var-def-e (first defs)))
         (cons (first defs) 
               (reduce-first-def (rest defs) 
                                 (cons (first defs) env)))]
        [else (cons (make-var-def 
                     (var-def-name (first defs))
                     (reduce (var-def-e (first defs)) env))
                    (rest defs))]))



; (list-of Exp) -> (list-of Exp)
; reduces first non-value of es
(define (reduce-first es env)
  (if (value? (first es))
      (cons (first es) (reduce-first (rest es) env))
      (cons (reduce (first es) env) (rest es))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infrastructure for executing programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; We use the initial environment to encode the primitive values and functions.
; We also provide some predefined non-primitive values/functions/structs for lists
; Can be extended as needed

(define initial-env 
  (local ; [X Y Z] (X Y -> Z) -> ( (make-primval X) (make-primval Y) -> (make-primval Z))
    [(define (lift2 op) 
       (lambda (x y) 
         (make-primval (op (primval-v x) (primval-v y)))))]
    (append
     (map (lambda (x)
            (make-var-def (first x) (make-primval (second x))))
          (list
           (list '+ (lift2 +))
           (list '- (lift2 -))
           (list '* (lift2 *))
           (list '= (lift2 =))
           (list 'true true)     ; workaround for bug in HTDP reader:
           (list 'false false))) ;   'true evaluates to 'true and not true
)))
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

(define (parse-eval-print sexp)
  (print (parse-and-eval sexp)))

; Exp -> (list-of s-Exp)
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

(check-expect (parse '(cond [(a? x) (+ b 2)] [55 19]))
              (make-cnd
               (list
                (make-cnd-clause (make-app (make-var 'a?) (list (make-var 'x))) (make-app (make-var '+) (list (make-var 'b) (make-primval 2))))
                (make-cnd-clause (make-primval 55) (make-primval 19)))))

; printer tests
(check-expect (print (parse '(local [(define fact (lambda (n) (cond [(= n 0) 1]
                                                                    [true (* n (fact (- n 1)))])))]
                               (fact 5))))
              `(local ((define fact (lambda (n) (cond ((= n 0) 1) (true (* n (fact (- n 1)))))))) (fact 5)))
(check-expect (print (parse '(local [(define-struct a (b c))] (a-b 55))))
              '(local ((define-struct a (b c))) (a-b 55)))

; reduction tests
(check-expect (reduce (reduce (parse '((lambda (x) (+ x 5)) 7)) empty) empty)
              (make-locl
               empty
               (make-locl (list (make-var-def 'x (make-primval 7))) (make-app (make-var '+) (list (make-var 'x) (make-primval 5))))))


; evaluator test
(check-expect (parse-eval-print '(+ 1 2)) 3)
(check-expect (parse-eval-print '(+ (+ 1 2) (+ 3 4))) 10)
(check-expect (parse-eval-print '((lambda (x) (+ x (+ 1 2))) (+ 2 3))) 8)
(check-expect (parse-eval-print '(local [(define x (+ 1 2))] x)) 3)
(check-expect (parse-eval-print '(cond [(= 0 (+ 1 2)) (+ 3 4)] [(= 0 0) (+ 5 6)])) 11)
(check-expect (parse-eval-print '(local [(define fact (lambda (n) (cond [(= n 0) 1]
                                                                        [true (* n (fact (- n 1)))])))]
                                   (fact 5))) 
              120)

(check-expect (parse-and-eval '(local [(define x 5)
                                       (define f (lambda (y) (+ x y)))]
                                 (local [(define x 12)]
                                   (f x))))
              (make-primval 17))


; check unbound variable error
(check-error (parse-and-eval 'x))

; check function arity error
(check-error (parse-and-eval '((lambda (x y) (+ x y)) 5)))

; check local definitions
(check-expect (parse-and-eval '(local [(define x 5) (define y (+ x 6))] (+ x y))) 
              (make-primval 16))

; check name shadowing
(check-expect (parse-and-eval '(local [(define x 1)] (local [(define x 2)] x))) 
              (make-primval 2))

; check name shadowing
(check-expect (parse-and-eval '(local [(define x 1)] ((local [(define x 2) (define f (lambda (y) (+ x y)))] f) 3))) 
              (make-primval 5))


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