#lang racket

(provide type-of)

(require "sandbox.rkt"
         threading)

(define (alternation-or-concatenation? sym)
  (member sym '(Alternation Concatenation)))

(define (debug . args)
  (displayln (apply ~a args) (current-error-port))
  (sleep 0.5))

(define (type-of binding tree)
  (let recur ([tree tree])
    (match tree
      ; a leaf of the binding
      [`(Bound ,(== binding) ,type)
       (debug "Bound ..." ": " tree)
       `(#:type ,type)]
  
      ;; combination rules
      
      ; - concatenation of two+ of the same type are an array of that type
      [`(Concatenation 
          ,@(list-no-order 
              `(,(or '#:array '#:type '#:nullable) ,type)
              `(,(or '#:array '#:type '#:nullable) ,same-type) ; TODO: bug in match?
              _ ...))
        (debug "Concatenation any ..2" ": " tree)
        `(#:array ,type)]

      ; - otherwise, concatenation doesn't change the type
      [`(Concatenation
          ,@(list-no-order 
              `(,(? keyword? kind) ,type)
              _ ...))
       (debug "Concatenation of just the type" ": " tree)
       `(,kind ,type)]

      ; - alternation between an array and anything is an array
      [`(Alternation ,@(list-no-order `(#:array ,type) _ ...))
       (debug "Alternation array" ": " tree)
       `(#:array ,type)]
  
      ; - besides arrays, alternation between a nullable and anything is nullable
      [`(Alternation ,@(list-no-order `(#:nullable ,type) _ ...))
       (debug "Alternation nullable" ": " tree)
       `(#:nullable ,type)]
  
      ; - alternation among only the type is just the type
      [`(Alternation (#:type ,type) ..1)
       (debug "Alternation type only" ": " tree)
       `(#:type ,(first type))]
  
      ; besides arrays and nullables, alternation between the type and anything
      ; is nullable
      [`(Alternation ,@(list-no-order `(#:type ,type) _ ...))
       (debug "Alternation type" ": " tree)
       `(#:nullable ,type)]
  
      ; - star of anything with the type is an array
      [`(Star
          ,@(list-no-order 
              (or `(#:array ,type) `(#:type ,type) `(#:nullable ,type))
              _ ...))
        (debug "Star" ": " tree)
        `(#:array ,type)]

      ; the type is just itself
      [`(,(? keyword? kind) ,type)
       `(,kind ,type)]

      ;; termination

      ; - any operation with all n/a (or nothing, which is impossible) is n/a
      [(list _ '#:n/a ...) '#:n/a]

      ;; recursion
  
      [(list (? alternation-or-concatenation? operation) subtrees ...)
       (debug "recur alternation-or-concatenation" ": " tree)
       (~>> subtrees (map recur) (cons operation) recur)]
  
      [(list 'Star subtree)
       (debug "recur Star" ": " tree)
       (recur `(Star ,(recur subtree)))]

      ;; other

      ; - otherwise, n/a
      [_
       (debug "other" ": " tree) 
       '#:n/a])))

; Foo  ::=  a:A | (C b:B | b:B C) | d:D
;      --> choice { a:A b:B c:C }
;
; Foo  ::=  a:A | (C b:B | b:B C) | D
;      --> sequence { a:A? b:B? }
;

