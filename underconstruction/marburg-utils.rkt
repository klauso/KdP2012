#lang racket/base

(require scribble/eval)
(require racket/sandbox)
(require mzlib/pconvert)
(require racket/pretty)
(require file/convertible)



(provide block ev ex stdeval)



(define stdeval (isl-eval '()))

(void (interaction-eval #:eval stdeval (require 2htdp/image)))
(void (interaction-eval #:eval stdeval (define rocket (bitmap "rocket-s.jpg"))))


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


(define-syntax-rule
  (*sl-eval module-lang reader def ...)
  ;; ===>>>
  (let ()
    (define me (make-base-eval))
    (me '(require (only-in racket empty? first rest cons? sqr true false)))
    (me '(require lang/posn))
    (me '(require racket/pretty))
    (me '(current-print pretty-print-handler))
    (me '(pretty-print-columns 65))
    (me 'def)
    ...
    (call-in-sandbox-context me (lambda () (error-print-source-location #f)))
    (call-in-sandbox-context me (lambda ()
				  (current-print-convert-hook
				    (let ([prev (current-print-convert-hook)])
				      ;; tell `print-convert' to leave images as themselves:
				      (lambda (v basic sub)
					(if (convertible? v)
					    v
					    (prev v basic sub)))))

				  (pretty-print-size-hook
				    (let ([prev (pretty-print-size-hook)])
				      ;; tell `pretty-print' that we'll handle images specially:
				      (lambda (v w? op)
					(if (convertible? v) 1 (prev v w? op)))))
				  
				  (pretty-print-print-hook
				    (let ([prev (pretty-print-print-hook)])
				      ;; tell `pretty-print' how to handle images, which is
				      ;; by using `write-special':
				      (lambda (v w? op)
					(if (convertible? v) (write-special v op) (prev v w? op)))))

				  ((dynamic-require 'htdp/bsl/runtime 'configure)
				   (dynamic-require reader 'options))))
    (call-in-sandbox-context me (lambda () (namespace-require module-lang)))
    (interaction-eval #:eval me (require 2htdp/image))
    (interaction-eval #:eval me (require 2htdp/batch-io))
    me))

(define-syntax-rule
  (bsl-eval def ...)
  (*sl-eval 'lang/htdp-beginner 'htdp/bsl/lang/reader def ...))

(define-syntax-rule
  (bsl-eval+ def ...)
  (*sl-eval 'lang/htdp-beginner-abbr 'htdp/bsl+/lang/reader def ...))

(define-syntax-rule
  (isl-eval def ...)
  (*sl-eval 'lang/htdp-intermediate 'htdp/isl/lang/reader def ...))

(define-syntax-rule 
  (isl-eval+ def ...)
  (*sl-eval 'lang/htdp-intermediate-lambda 'htdp/isl/lang/reader def ...))


