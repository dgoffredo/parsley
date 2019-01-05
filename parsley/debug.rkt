#lang racket

(provide debug debug*)

(define-for-syntax debugging? #f)

(define-syntax (debug stx)
  (syntax-case stx ()
    [(debug args ...)
     (if (not debugging?)
       #'(void)
       #'(begin
           (displayln (~a args ...) (current-error-port))
           (sleep 0.25)))]))

(define-syntax-rule (debug* form)
  (debug 'form ": " form))
