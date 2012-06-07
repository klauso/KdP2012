#lang racket

(require scribble/base)
(require scribble/core)
(require scribble/eval)
(require racket/base)
(require racket/sandbox)

(provide eg ex)

(define rkteval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (make-evaluator 'racket) ) )

(define-syntax eg
  (syntax-rules ()
    [(_ e ...)
     (interaction #:eval rkteval e ...) ] ) )

(define ex-ids (make-hash))

(define (ex-ref id)
  (format "~a" (hash-ref ex-ids id) ) )

(define i 0)

(define (ex [id #f])
  (set! i (+ i 1))
  (when id (hash-set! ex-ids id i))
  (element (style 'bold '())
           (format "Exercise ~a. " i) ) )

