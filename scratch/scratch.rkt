#lang racket

(provide binding-type)

(require "sandbox.rkt"
         threading
         srfi/1)

(define-for-syntax debugging? #f)

(define-syntax (debug stx)
  (syntax-case stx ()
    [(debug args ...)
     (if (not debugging?)
       #'(void)
       #'(begin
           (displayln (~a args ...) (current-error-port))
           (sleep 0.5)))]))

(define-syntax-rule (define-tree-transformer name clauses ...)
  (define (name tree)
    (match tree
      clauses ...

      (...
        [(list operation patterns ...)
         `(,operation ,@(map name patterns))])

      [other other])))

(define-tree-transformer remove-plus
  [`(Plus ,pattern)
    (let ([pattern (remove-plus pattern)])
      `(Concatenation ,pattern (Star ,pattern)))])

(define-tree-transformer remove-question
  [`(Question ,pattern)
   `(Alternation ,(remove-question pattern) ())])

(define (alternation/concatenation? symbol)
  (member symbol '(Alternation Concatenation)))

; The input language is the parse tree with the following modifications:
; - (Plus expr) has been transformed into (Concatenation expr (Star expr))
; - (Question expr) has been transformed into (Alternation expr '())

(define (binding-type binding tree)
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
  
      ; - besides arrays, alternation between nullable and anything is nullable
      [`(Alternation ,@(list-no-order `(#:nullable ,type) _ ...))
       (debug "Alternation nullable" ": " tree)
       `(#:nullable ,type)]
  
      ; - alternation among only the type is just the type
      [`(Alternation (#:type ,type) ..1)
       (debug "Alternation type only" ": " tree)
       ; TODO: Might this code be the right place to assert consistent types?
       `(#:type ,(first type))] ; `first` because `type` is a (list t t)
  
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

      ; - the type is just itself
      [`(,(? keyword? kind) ,type)
       `(,kind ,type)]

      ;; termination

      ; - any operation with all n/a (or nothing, which is impossible) is n/a
      [(list _ '#:n/a ...) '#:n/a]

      ;; recursion
  
      [(list (? alternation/concatenation? operation) subtrees ...)
       (debug "recur alternation/concatenation" ": " tree)
       (~>> subtrees (map recur) (cons operation) recur)]
  
      [(list 'Star subtree)
       (debug "recur Star" ": " tree)
       (recur `(Star ,(recur subtree)))]

      ;; other

      ; - otherwise, n/a
      [_
       (debug "other" ": " tree) 
       '#:n/a])))

(define (common-prefix . lists)
  (reverse
    (let recur ([common-backwards '()] [lists lists])
      (match lists
        [(list (cons heads tails) ...)
         (match (remove-duplicates heads)
           [(list common-head)
            (recur (cons common-head common-backwards) tails)]
           [_ common-backwards])]
        [_ common-backwards]))))

(define (binding-paths tree)
  ; :: parse-tree -> (list (list name path) ...)
  ; where name :: string?
  ;       path :: (list integer?)
  (let recur ([path-reversed '()] [binding-paths '()] [tree tree])
    (match tree
      ; found a binding
      [(list 'Bound binding-name _)
       (cons (list binding-name (reverse path-reversed)) binding-paths)]

      ; some tree node -- recur on each branch, modifying the path as necessary
      [(list _ branches ...)
       (append* 
         (for/list ([branch branches] [i (in-range (length branches))])
           (recur (cons i path-reversed) binding-paths branch)))]

      [_ binding-paths])))

(struct rule/terminal (name modifiers pattern) #:transparent)
(struct rule/class    (name pattern bindings)  #:transparent)
(struct rule/other    (name pattern)           #:transparent)

(define (terminal-pattern? pattern)
  (match pattern 
    [(? string? _)   #t]
    [(list 'regex _) #t]
    [_ #f]))

(define (categorize-rule rule)
  (match rule
    [(list 'Rule modifiers ... name pattern)
     (if (terminal-pattern? pattern)
       (rule/terminal modifiers name pattern)
       (let ([bindings (binding-paths pattern)])
         ; If there are bindings, then it's a class. Otherwise, it's an other.
         ; First, though, make sure that there are no modifiers, since only
         ; terminals may have modifiers.
         (unless (empty? modifiers)
           (raise-user-error
             (~a "The grammar rule named " (~s (~a name))
               " contains the modifers " modifiers
               " but was not deduced to be terminal. Only terminal rules may "
               "have modifiers. A terminal rule is any rule whose pattern is "
               "either just a string or a regular expression.")))
         (if (empty? bindings)
           (rule/other name pattern)
           (rule/class name pattern bindings))))]))

(define (follow-path path tree)
  (for/fold ([tree tree]) ([index path])
    (match tree
      [(list _ subtrees ...)
       (list-ref subtrees index)])))

(define (drop-last lst)
  (take lst (- (length lst) 1)))

(define (find-along-path found? path tree)
  (let recur ([path path] [tree tree])
    (cond 
      [(found? tree) tree]
      [(empty? path) #f]
      [else
       (match tree 
         [(list _ subtrees ...) 
          (recur (rest path) (list-ref subtrees (first path)))]
         [_ #f])])))

(define (alternation/star? tree)
  (match tree
    [(list 'Alternation _ ...)  #t]
    [(list 'Star _)             #t]
    [_                          #f]))

(define (after-prefix prefix-path path tree)
  (~> path
    (take (+ 1 (length prefix-path)))
    (follow-path tree)))

(define (class-category rule)
  (match rule
    [(rule/class name pattern bindings)
     (let* ([paths (map second bindings)]
            [prefix-path (apply common-prefix paths)]
            [subtree (follow-path prefix-path pattern)])
       (match subtree
         [(list 'Alternation _ ...)
          ; If all of the following are true:
          ; - the most recent common ancestor is an Alternation,
          ; - there are no Star or Alternation nodes between the common
          ;   ancestor and root,
          ; - the "binding type" of each binding relative to the node just
          ;   below the common ancestor is not #:array or #:nullable,
          ; then the "class category" of the rule is 'choice. Otherwise, it's
          ; 'sequence.
          (if 
            (and
              ; - no Star or Alternation leading up to common ancestor
              (or (empty? prefix-path)
                  (not (find-along-path alternation/star? 
                                        (drop-last prefix-path) 
                                        pattern)))
              ; types as calculated from just below the common ancestor are not
              ; #:array or #:nullable
              (for/and ([binding bindings])
                (match binding
                  [(list name path)
                   (match (binding-type 
                            name 
                            (after-prefix prefix-path path pattern))
                     [`(#:array ,_)    #f]
                     [`(#:nullable ,_) #f]
                     [_                #t])])))
            ; The above is true, so this is a choice.
            'choice
            ; The above is not true, so this is a sequence.
            'sequence)]
         ; The common ancestor is not an Alternation, so this is a sequence.
         [_ 'sequence]))]))
                 