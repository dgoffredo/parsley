#lang racket

(provide (struct-out key-view))

(require threading)

; A key-view implements the gen:set interface using the keys of a specified
; dict (which is itself associated with an interface).
(struct key-view (dict)
  #:transparent
  #:methods gen:set
  [(define (set-member? view key)
     (dict-has-key? (key-view-dict view) key))
   (define (set->stream view)
     (~> view key-view-dict in-dict-keys sequence->stream))])
