#lang racket

(require "sandbox.rkt"
         threading
         srfi/1)

(define-for-syntax debugging? #t)

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

(struct occurrence            (type) #:transparent)
(struct scalar     occurrence ()     #:transparent) ; exactly one
(struct array      occurrence ()     #:transparent) ; zero or more
(struct nullable   occurrence ()     #:transparent) ; zero or one

(struct basic   (name) #:transparent) ; e.g. string, integer
(struct complex (name) #:transparent) ; user-defined type

(define (terminal-type rule)
  (match rule
    [(rule/terminal _ modifier _)
     (case modifier
       [(ignore #f) (basic 'string)]      ; ignored/default terminal -> string
       [else        (basic modifier)])])) ; type-annotated terminal -> the type

; The input language is the parse tree with the following modifications:
; - (Plus expr) has been transformed into (Concatenation expr (Star expr))
; - (Question expr) has been transformed into (Alternation expr '())

; TODO: First make sure the types of all instances of each binding are the same.
(define (binding-type binding tree)
  (let recur ([tree tree])
    (match tree
      ; a leaf of the binding
      [`(Bound ,(== binding) ,bound-pattern)
       (debug "Bound ..." ": " tree)
       (scalar bound-pattern)]
  
      ;; combination rules
      
      ; - concatenation of two+ of the same type are an array of that type
      [`(Concatenation
          ,@(list-no-order (occurrence type) (occurrence same-type) _ ...))
        (debug "Concatenation any ..2" ": " tree)
        (array type)]

      ; - otherwise, concatenation doesn'type change the type
      [`(Concatenation ,@(list-no-order (? occurrence? type) _ ...))
       (debug "Concatenation of just the type" ": " tree)
       type]

      ; - alternation between an array and anything is an array
      [`(Alternation ,@(list-no-order (array type) _ ...))
       (debug "Alternation array" ": " tree)
       (array type)]
  
      ; - besides arrays, alternation between nullable and anything is nullable
      [`(Alternation ,@(list-no-order (nullable type) _ ...))
       (debug "Alternation nullable" ": " tree)
       (nullable type)]
  
      ; - alternation among only the type is just the type
      [(list 'Alternation (scalar type) ..1)
       (debug "Alternation type only" ": " tree)
       (scalar (first type))] ; `first` because `type` is bound to a list
  
      ; besides arrays and nullables, alternation between the type and anything
      ; is nullable
      [`(Alternation ,@(list-no-order (scalar type) _ ...))
       (debug "Alternation type" ": " tree)
       (nullable type)]
  
      ; - star of anything with the type is an array
      [`(Star ,@(list-no-order  (occurrence type) _ ...))
        (debug "Star" ": " tree)
        (array type)]

      ; - the type is just itself
      [(occurrence type)
       (occurrence type)]

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

(struct rule/terminal    (name modifier pattern)  #:transparent)
(struct rule/class       (name pattern bindings)  #:transparent)
(struct rule/enumeration (name strings)           #:transparent)
(struct rule/other       (name pattern)           #:transparent)

(define (terminal-pattern? pattern)
  (match pattern 
    [(? string? _)   #t]
    [(list 'regex _) #t]
    [_ #f]))

(define (enumeration-strings name pattern)
  "Return a list of the string values associated with the enumeration having
   the specified rule @var{pattern}. Raise a user error if the @var{pattern}
   is not consistent with that of an enumeration. The specified rule
   @var{name} is used in diagnostics only."
  (match pattern
    [(list 'Alternation (? string? strings) ...) strings]
    [_ (raise-user-error 
         (~a "The rule named " (qt name) " is declared as an enumeration but "
           "does not have a pattern consistent with an enumeration. An "
           "enumeration is of the form (Alternation string-literals ...) but "
           name " has the pattern " (~s pattern)))]))

(define (validate-modifier name modifiers)
  "Return the one modifier symbol contained within the specified list of
   @var{modifiers} if it is valid, or return #f if the list is empty. Raise
   a user error if @var{modifiers} contains any unsupported modifier. The
   specified rule @var{name} is used in diagnostics only."
  (match modifiers
    ['() #f]
    [(list modifier)
     (let ([supported '(ignore enumeration byte decimal int integer long
                        negativeInteger nonNegativeInteger
                        nonPositiveInteger positiveInteger short
                        unsignedLong unsignedInt unsignedShort unsignedByte)])
       (if (member modifier supported)
         modifier
         (raise-user-error
           (~a "The rule named " (qt name) " is declared with an unsupported "
             "modifier " (qt modifier) ". Here are the supported modifiers: "
             modifiers))))]))

(define (assert-terminal-modifier name modifier)
  (if (not (equal? modifier 'enumeration))
    modifier ; good to go
    (raise-user-error
      (~a "The rule named " (qt name) " is declared with the \"enumeration\" "
        "modifier but does not have a enumeration-compatible pattern. "
        "Instead, " name " looks like a terminal (string literal or regular "
        "expression). An enumeration's pattern must instead be an alternation "
        "among two or more string literals."))))

(define (qt symbol-or-string)
  "Return a string that contains the specified @var{symbol-or-string} in double
   quotes."
  (~s (~a symbol-or-string)))

(define (verify-binding-consistency-for-class rule
                                              class/enum-names 
                                              terminal->type)
  "Verify that all bindings within the specified @var{rule} that have the same
   name also have the same type. Use the specified @var{class/enum-names} set
   and the specified @var{terminal->type} dict to identify the types of names
   to which bindings might be attached."
  (match rule
    [(rule/class rule-name pattern bindings)
     (for/fold ([binding->type (hash)]) ([binding bindings])
       (match binding
         [(list name path)
          (match (follow-path path pattern)
            [(list 'Bound (== name) bound)
             (debug "rule: " rule-name 
                    " binding: " name
                    " class/enum-names: " class/enum-names
                    " terminal->type: " terminal->type)
             (let ([type
                    (or (and (set-member? class/enum-names bound) bound)
                        (dict-ref terminal->type bound #f)
                        (and (terminal-pattern? bound) (basic 'string))
                        (raise-user-error
                          (~a rule-name "." name " has undefined type.")))])
               (match (hash-ref binding->type name #f)
                 [#f        (hash-set binding->type name type)] ; first time
                 [(== type) binding->type]                      ; consistent
                 [other     (raise-user-error
                              (~a "Rule named " (qt rule-name) " has binding "
                                (qt name) " with two incompatible types. As "
                                "bound to " (qt bound) " it has type " type
                                ", but elsewhere it is bound with type " other
))]))])]))]))

(define (verify-binding-consistency rules)
  "Verify that all occurrences of any particular binding in any `rule/class`
   within the specified @var{rules} are of the same type. In addition to the
   class rules, @{rules} must contain any `rule/enumeration` or `rule/terminal`
   that might be bound in class rules. Raise a user error if a type mismatch is
   found; otherwise, return @var{rules}."
  (let ([class/enum-names
         (for/fold ([names (set)]) ([rule rules])
           (match rule
             [(or (rule/class name _ _) (rule/enumeration name _))
              (set-add names name)]
             [_ names]))]
        [terminal->type
         (for/fold ([name->type (hash)]) ([rule rules])
           (match rule
             [(rule/terminal name _ _)
              (hash-set name->type name (terminal-type rule))]
             [_ name->type]))])
    (for ([rule rules])
      (when (rule/class? rule)
        (verify-binding-consistency-for-class rule 
                                              class/enum-names 
                                              terminal->type)))
    rules))

(define (categorize-rule rule)
  (match rule
    [(list 'Rule modifiers ... name pattern)
     (let ([modifier (validate-modifier name modifiers)])
       (if (terminal-pattern? pattern)
         ; If it's a terminal, it's a terminal.
         (rule/terminal name (assert-terminal-modifier name modifier) pattern)
         (let ([bindings (binding-paths pattern)])
           (cond
             ; If it has bindings, then it's a class.
             [(not (empty? bindings)) 
              (rule/class name pattern bindings)]
             ; If it's declared as an enumeration, then it's an enumeration.
             [(equal? modifier 'enumeration)
              (rule/enumeration name (enumeration-strings name pattern))]
             ; Otherwise, it's an other.
             [else (rule/other name pattern)]))))]))

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
    [(rule/class name pattern (list (list binding-names binding-paths) ...))
     (let* ([prefix-path             (apply common-prefix binding-paths)]
            [nearest-common-ancestor (follow-path prefix-path pattern)])
       (match nearest-common-ancestor
         [(list 'Alternation _ ...)
          ; If all of the following are true:
          ; - the most recent common ancestor is an Alternation,
          ; - there are no Star or Alternation nodes between the common
          ;   ancestor and root,
          ; - the "binding type" of each binding relative to the node just
          ;   below the common ancestor is not array or nullable,
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
              ; array or nullable
              (for/and ([name binding-names] [path binding-paths])
                (match (binding-type 
                         name 
                         (after-prefix prefix-path path pattern))
                  [(array _)    #f]
                  [(nullable _) #f]
                  [_            #t])))
            ; The above is true, so this is a choice.
            'choice
            ; The above is not true, so this is a sequence.
            'sequence)]
         ; The common ancestor is not an Alternation, so this is a sequence.
         [_ 'sequence]))]))
                 