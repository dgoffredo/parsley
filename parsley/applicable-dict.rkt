#lang racket

; `applicable-dict` is a `struct` that wraps any object that implements the
; `dict` generic interface. `applicable-dict` also implements the `dict`
; interface, forwarding to the wrapped object, but in addition,
; `applicable-dict` instances are invokable as procedures. For some `instance`
; object,
;
;     (instance key optional-default)
;
; results in the equivalent of
;
;     (dict-ref instance key optional-default)
;
; Also, a procedure `applicable-hash` is provided. `applicable-hash` is like
; the built-in `hash` procedure, except that is returns the immutable hash
; table wrapped in an `applicable-dict` instance.
;
; Here's some example usage:
;
;     (let ([number->name (applicable-hash 1 "one" 2 "two" 3 "three")])
;       (string-join
;         (map
;           (lambda (n)
;             (~a n " is named " (number->name n "N/A") "."))
;           '(1 2 3 4))
;         " "))
;
; prints "1 is named one. 2 is named two. 3 is named three. 4 is named N/A."

(provide (struct-out applicable-dict)
         applicable-hash)

; "anchor" and "ns", defined below, are used in the "eval" call used within
; "proxy". eval is not easy in Racket. See the accepted answer to:
; https://stackoverflow.com/questions/20778926/
(define-namespace-anchor anchor)

(define ns (namespace-anchor->namespace anchor))

(define (proxy procedure-name . args)
  "Invoke the procedure having the specified name with the specified args. This
   proxying allows dict methods defined for applicable-dict to call the
   shadowed generic dict methods."
  (apply (eval procedure-name ns) args))

(define (forward method-name object . args)
  "Invoke the specified method name on the dict within the specified object,
   passing to the method additionally the specified arguments."
  (match object
    [(applicable-dict dict)
     (apply proxy `(,method-name ,dict ,@args))]))

(struct applicable-dict (dict)
  #:transparent
  #:property prop:procedure
  (lambda (object key 
           [default (lambda () (error "key not found" key))])
    (dict-ref object key default))
  #:methods gen:dict
  [; primitive methods
   (define (dict-ref object key 
                     [default (lambda () (error "key not found" key))])
     (forward 'dict-ref object key default))
   (define (dict-set! object key value)
     (forward 'dict-set! object key value))
   (define (dict-set object key value)
     (forward 'dict-set object key value))
   (define (dict-remove! object key)
     (forward 'dict-remove! object key))
   (define (dict-remove object key)
     (forward 'dict-remove object key))
   (define (dict-iterate-first object)
     (forward 'dict-iterate-first object))
   (define (dict-iterate-next object position)
     (forward 'dict-iterate-next object position))
   (define (dict-iterate-key object position)
     (forward 'dict-iterate-key object position))
   (define (dict-iterate-value object position)
     (forward 'dict-iterate-value object position))
   ; TODO: derived methods
   ; dict-has-key?
   ; dict-set*!
   ; dict-set*
   ; dict-ref!
   ; dict-update!
   ; dict-update
   ; dict-map
   ; dict-for-each
   ; dict-empty?
   ; dict-count
   ; dict-copy
   ; dict-clear
   ; dict-clear!
   ; dict-keys
   ; dict-values
   ; dict->list
   ])

(define (applicable-hash . keys-and-values)
  "Return an applicable-dict containing an immutable hash table containing
   the specified keys-and-values, e.g.

       (define h (applicable-hash 1 2 'x 'y))
       (dict-ref h 1)                         ; 2
       (h 'x)                                 ; 'y"
  (applicable-dict (apply hash keys-and-values)))
