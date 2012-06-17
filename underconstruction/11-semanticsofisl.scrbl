#lang scribble/manual
@(require scribble/eval)
@(require scribble/core)
@(require "marburg-utils.rkt")
@(require (for-label lang/htdp-beginner))
@(require (for-label (except-in 2htdp/image image?)))
@(require (for-label 2htdp/universe))
@(require scribble/bnf)
@(require scribble/decode
          scribble/html-properties
          scribble/latex-properties)
   
@(define inbox-style
    (make-style "InBox"
                (list (make-css-addition "inbox.css")
                      (make-tex-addition "inbox.tex"))))

@title[#:version ""]{Bedeutung von ISL+}

In diesem Abschnitt wollen wir genau definieren, was die neuen Sprachkonstrukte, die wir kennengelernt haben (insbesondere
lokale Definitionen und Funktionen als Werte), bedeuten. In der Terminologie von HTDP/2e heißt diese Sprache ISL+ oder "Zwischenstufe mit Lambda".

Wir werden wieder die Bedeutung in Form von Reduktionsschritten angeben. Allerdings werden wir diesmal
die Reduktionsschritte anders aufschreiben, nämlich als Programm in ISL+! 
Den kompletten Code (und mehr) dieses Kapitels finden Sie unter @url{https://github.com/klauso/KdP2012/raw/master/islinterpreter.rkt} (für
die vollständige Version) oder 
@url{https://github.com/klauso/KdP2012/raw/master/islinterpreterwostructs.rkt} (für die vereinfachte Version ohne Strukturen).
Man könnte sagen, dass wir eine Variante des "Stepper" aus DrRacket definieren.

Wir haben uns aus mehreren Gründen dafür entschieden, die Bedeutung von ISL+ als ISL+ Programm zu definieren. Zum einen wird dadurch
die Bedeutungsdefinition "lebendiger" --- sie können damit Programme ausführen, testen, mit Varianten und "was wäre wenn" Szenarien experimentieren.
Zum zweiten lernen Sie einige wichtige Programmiertechniken kennen, die unabhängig von dieser Sprache für jeden Programmierer relevant sind.
Zum dritten ist ein Lernziel dieses Kurses, dass sie nicht nur eigene Programme von Null auf neu schreiben können, sondern auch
bestehende, größere Programme lesen, verstehen und erweitern können.

@section{Syntax von Kern-ISL+}

Wir werden nicht die gesamte Sprache ISL+ modellieren, sondern wie sie es bereits aus den vorherigen Kapiteln kennen, nur
die Kernkonstrukte; was als syntaktischer Zucker darauf aufbauend definiert werden kann, lassen wir aus.

Durch die λ-Notation wird die Kernsprache erstmal deutlich vereinfacht, weil wir nicht mehr zwischen Variablendefinitionen und
Funktionsdefinitionen unterscheiden müssen, daher nehmen wir nur Syntax für Variablendefinitionen in unsere formale Syntaxdefinition
auf. Aus dem gleichen Grund muss syntaktisch nicht mehr zwischen primitiven Werten und primitiven Funktionen unterschieden werden.

In diesem Abschnitt verzichten wir der Einfachheit halber auf die Behandlung der boolschen Operatoren, da es diesbezüglich nichts
neues im Vergleich zur Definition aus Abschnitt @secref{semanticsbsl} gibt. Außerdem verzichten wir hier auf die Behandlung
von Strukturen; in der Vorlesung werden wir jedoch die Variante des Interpreters mit Unterstützung für Strukturen (oben verlinkt)
präsentieren. Wenn Sie dieses einfachere Programm verstanden haben, werden sie dann auch ohne Probleme das größere Programm 
mit Unterstützung für Strukturen verstehen. Insgesamt ergibt sich folgende Syntax für die Kernsprache:

@(define open (litchar "("))
@(define close (litchar ")"))
@(define lb (litchar "["))
@(define rb (litchar "]"))
@(define (mv s)
       (make-element #f  (list (make-element 'italic s))))


@BNF[(list @nonterm{definition} 
         @BNF-seq[open @litchar{define} @nonterm{name} @nonterm{e} close])
         
     (list @nonterm{e}
         @BNF-seq[open @nonterm{e} @kleeneplus[@nonterm{e}] close]
         @BNF-seq[open @litchar{lambda} open @kleeneplus[@nonterm{name}] close @nonterm{e} close]
         @BNF-seq[open @litchar{local} lb @kleeneplus[@nonterm{definition}] rb @nonterm{e} close]
         @BNF-seq[open @litchar{cond} @kleeneplus[@BNF-group[@BNF-seq[lb @nonterm{e} @nonterm{e} rb ] ]] close]
         @nonterm{name}
         @nonterm{v}
         )
     (list @nonterm{v}
         @BNF-seq[open @litchar{closure} open @litchar{lambda} open @kleeneplus[@nonterm{name}] close @nonterm{e} close @nonterm{env} close]
         @nonterm{number}
         @nonterm{boolean}
         @nonterm{string}
         @nonterm{image}
         @nonterm{+}
         @nonterm{*}
         @nonterm{...})]          ]

Wenn wir die Reduktionsschritte als ISL+ Programm definieren wollen, müssen wir uns erstmal überlegen, wie wir Programme
repräsentieren. Zu diesem Zweck können wir Datentypen so definieren, daß sie genau den Nichtterminalen 
der Grammatik entsprechen. Jede Alternative einer Regel können wir als Produkttypen definieren und die Menge aller
Alternativen als Summentyp. Die Terminalsymbole (wie @litchar{define}) sind hierbei nicht interessant, sondern wir repräsentieren nur
die relevanten Informationen:
     
@#reader scribble/comment-reader
(racketblock
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
; - (make-cnd (list-of (make-cnd-clause Exp Exp)))
)

@section{Umgebungen}

@#reader scribble/comment-reader
(racketblock
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
   (cond [(empty? env) 
          (error (string-append "Unbound variable: " 
                                (symbol->string x)))]
         [(and (var-def? (first env))
               (eq? (var-def-name (first env)) x))
          (var-def-e (first env))]
        [else (lookup-env (rest env) x)]))
)

@section{Reduktion von Ausdrücken}

@#reader scribble/comment-reader
(racketblock
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
)

@subsection{Reduktion von Funktionsapplikation}

@#reader scribble/comment-reader
(racketblock
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

; (list-of Exp) -> (list-of Exp)
; reduces first non-value of es
(define (reduce-first es env)
  (if (value? (first es))
      (cons (first es) (reduce-first (rest es) env))
      (cons (reduce (first es) env) (rest es))))
)

@subsection{Reduktion lokaler Definitionen}

@#reader scribble/comment-reader
(racketblock
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
)

@subsection{Reduktion konditionaler Ausdrücke}
@#reader scribble/comment-reader
(racketblock
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
)


@subsection{Auswertung von Ausdrücken}

@#reader scribble/comment-reader
(racketblock
; Exp -> Value
; reduces expression until it becomes a value (or loops)
(define (eval e)
  (if (value? e) 
      e
      (eval (reduce e empty))))
)

@section{Ist das keine zirkuläre Sprachdefinition?}
Wir haben im Interpreter zwar ISL+-Features benutzt, aber nicht in einer essentiellen Art und Weise (wir könnten ihn leicht so umbauen,
dass er nur BSL-Features benutzt). Insbesondere verwenden wir lokale Definitionen und λ nicht in essentieller Art und Weise.

Für BSL haben wir bereits die Bedeutung definiert.

Es gibt jedoch Eigenschaften der Sprache, deren Bedeutung durch diese Reduktionssemantik nicht festgelegt wird, wie beispielsweise
die Präzision der Arithmetik oder was passiert wenn durch 0 dividiert wird.


@section{Ist dies ein Interpreter?}

Nein, Interpreter ist typischerweise "big-step".