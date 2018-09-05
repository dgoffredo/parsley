#lang racket

(provide make-counter)

(require racket/generator)

(define (make-counter from)
  "Return a procedure of zero arguments that returns the next integer when
   invoked, initially returning the integer @var{from}."
  (generator ()
    (let recur ([value from])
      (yield value)
      (recur (+ 1 value)))))
