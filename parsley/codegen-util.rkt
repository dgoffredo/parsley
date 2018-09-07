#lang racket

(provide string-join*)

(define (string-join* separator strings)
  (string-join strings separator))
