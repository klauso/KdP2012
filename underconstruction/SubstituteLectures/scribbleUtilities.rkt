#lang racket

(require scribble/eval)
(require racket/sandbox)

(provide eg)

(define rkteval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (make-evaluator 'racket) ) )

(define-syntax eg
  (syntax-rules ()
    [(_ e ...)
     (interaction #:eval rkteval e ...) ] ) )