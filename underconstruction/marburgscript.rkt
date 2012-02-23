#lang racket/base

(require scribble/eval)
(require racket/sandbox)

(provide block ev ex)

;(define stdeval (make-base-eval))

(define stdeval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'lang/htdp-intermediate)))

(define-syntax block
  (syntax-rules ()
    [(_ e ...)
     (racketblock+eval #:eval stdeval #:escape unsyntax e ...)]))



(define-syntax ev
  (syntax-rules ()
    [(_ e )
     (interaction-eval-show #:eval stdeval e)]))

(define-syntax ex
  (syntax-rules ()
    [(_ e ...)
       (interaction #:eval stdeval e ...)]))

