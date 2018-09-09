#lang racket

(provide key-by
         string-join*)

(define (string-join* separator strings)
  "string-join, but with the arguments in the opposite order"
  (string-join strings separator))

(define (key-by accessor entries)
  " :: (entry -> key) (list entry ...) -> (hash key entry ...)"
  (for/hash ([entry entries])
    (values (accessor entry) entry)))
