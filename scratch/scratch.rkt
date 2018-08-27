#lang racket

(require "sandbox.rkt"
         "key-view.rkt"
         "mark-and-sweep.rkt"
         graph
         threading
         srfi/1
         racket/generator)

(define-for-syntax debugging? #t)

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

(define (inline-others rules [name->rule (key-by rule/base-name rules)])
  (define-tree-transformer transform
    [(list 'Bound name pattern)
     (list 'Bound name (transform pattern))]
    [(? symbol? name) (=> pass)
     (match (dict-ref name->rule name)
       [(rule/other _ pattern) pattern]
       [_ (pass)])])

  (for/list ([rule rules] #:when (not (rule/other? rule)))
    (replace-pattern rule transform)))

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
    [(rule/terminal _ _ modifier)
     (case modifier
       [(ignore #f) (basic 'string)]      ; ignored/default terminal -> string
       [else        (basic modifier)])])) ; type-annotated terminal -> the type

; The input language is the parse tree with the following modifications:
; - (Plus expr) has been transformed into (Concatenation expr (Star expr))
; - (Question expr) has been transformed into (Alternation expr '())

(define (bound-pattern-type pattern name->rule)
  (debug* (dict-keys name->rule))
  (match (dict-ref name->rule pattern)
    [(rule/terminal name _ modifier)
     (case modifier
       [(ignore) (raise-user-error
                   (~a "The terminal " (qt name) " appears in a binding, but "
                     "is marked \"ignored\". Ignored terminals may not be "
                     "bound."))]
       [(#f) (basic 'string)]    ; If nothing was specified, it's a string.
       [else (basic modifier)])] ; If a type was specified, that's it.
    [(rule/complex name _)
     (complex name)]))           ; If name maps to some class or enum, use it.

(define (binding-type-relative-to binding tree name->rule)
  "TODO"
  (let recur ([tree tree])
    (match tree
      ; a leaf of the binding
      [`(Bound ,(== binding) ,bound-pattern)
       (debug "Bound ..." ": " tree)
       (scalar (bound-pattern-type bound-pattern name->rule))]
  
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

      [(list 'Question _ ...)
       (raise-arguments-error 'binding-type
         "Pattern (tree) must not contain any Question nodes."
         "tree" tree)]

      [(list 'Plus _ ...)
       (raise-arguments-error 'binding-type
         "Pattern (tree) must not contain any Plus nodes."
         "tree" tree)]

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

(struct rule/base        (name pattern)       #:transparent)
(struct rule/terminal    rule/base    (modifer)  #:transparent)
(struct rule/other       rule/base    ()         #:transparent)
(struct rule/complex     rule/base    ()         #:transparent)
(struct rule/class       rule/complex (bindings) #:transparent)
(struct rule/enumeration rule/complex (values)  #:transparent)

(struct schema/base        (name rule)                 #:transparent)
(struct schema/sequence    schema/base (element-types) #:transparent)
(struct schema/choice      schema/base (element-types) #:transparent)
(struct schema/enumeration schema/base (values)        #:transparent)

(define (replace-pattern rule value-or-procedure)
  ; Return a copy of @var{rule} that has had its pattern replace by the
  ; specified value if @var{value-or-procedure} is not a procedure, or
  ; otherwise has had its pattern transformed by the procedure
  ; @var{value-or-procedure}. If @var{pattern-or-procedure} is a procedure,
  ; then it must take a pattern as its one argument and produce a pattern.
  (if (procedure? value-or-procedure)
    (replace-pattern rule (value-or-procedure (rule/base-pattern rule)))
    (let ([new-pattern value-or-procedure])
      (match rule
        [(rule/terminal    name pattern     modifier)
         (rule/terminal    name new-pattern modifier)]
        [(rule/class       name pattern     bindings)
         (rule/class       name new-pattern bindings)]
        [(rule/enumeration name pattern     values)
         (rule/enumeration name new-pattern values)]
        [(rule/other       name             pattern)
         (rule/other       name             new-pattern)]))))

(define (terminal-pattern? pattern)
  (match pattern 
    [(? string? _)   #t]
    [(list 'regex _) #t]
    [_ #f]))

(define (make-counter from)
  (generator ()
    (let recur ([value from])
      (yield value)
      (recur (+ 1 value)))))

(define (unique-terminal-name names counter)
  (let recur ([name (string->symbol (~a "TOKEN_" (counter)))])
    (if (set-member? names name) ; already taken?
      (recur names)              ; re-roll
      name)))                    ; found a unique name

(define (extract-terminals-from-pattern 
           pattern counter rules name->rule pattern->terminal)
  "Inspect the specified @var{pattern}. For each distinct literal (string or
   regular expression) for which there is no corresponding terminal rule (as
   determined by lookup in @var{pattern->terminal}), create a new terminal
   rule. Use the specified @var{name->rule} to ensure that the name of the
   created rule is not already taken. Return three values:
   - the specified @var{rules} extended by any terminal rules created,
   - @var{name->rule} extended by any terminal rules created,
   - and @var{pattern->terminal} extended by any terminal rules created."
   (let recur ([rules rules] 
               [name->rule name->rule] 
               [pattern->terminal pattern->terminal]
               [tree pattern])
     (match tree
       [(? terminal-pattern? pattern)
        (if (not (dict-ref pattern->terminal pattern #f))
          ; Discovered a novel terminal literal. Make it a rule.
          (let* ([name (unique-terminal-name (key-view name->rule) counter)]
                 [rule (rule/terminal name pattern #f)])
            (values (cons rule rules)
                    (dict-set name->rule name rule)
                    (dict-set pattern->terminal pattern rule)))
          ; Found a terminal that already has an associated rule. Move along.
          (values rules name->rule pattern->terminal))]
       [(list _ children ...)
        ; Not a leaf of the pattern tree. Fold over the children of this node.
        (for/fold ([rules rules] 
                   [name->rule name->rule] 
                   [pattern->terminal pattern->terminal])
                  ([child children])
          (recur rules name->rule pattern->terminal child))]
       [otherwise
        ; Non-terminal leaf. Must be the name of another rule. Move along.
        (values rules name->rule pattern->terminal)])))

(define (key-by accessor entries)
  (for/hash ([entry entries])
    (values (accessor entry) entry)))

(define (replace-literals-with-terminals pattern pattern->terminal)
  (define-tree-transformer transform
    [(? terminal-pattern? pattern)
     (rule/base-name (dict-ref pattern->terminal pattern))])

  (transform pattern))

(define (extract-terminals rules)
  "Return an extended version of the specified list of @var{rules}, where
   each literal string or regular expression appearing within the patterns of
   non-terminal rules has been given a name and its own (terminal) rule, and
   all references to the terminal literal have been replaced with its name."
  ; First, get the extended list of rules by extracting terminal literals from
  ; rule patterns.
  (let ([new-name-counter (make-counter 1)]) ; generates integers for new names
    (let-values
      ([(extended-rules name->rule pattern->terminal)
        (for/fold ([extended-rules rules]
                   [name->rule
                    (key-by rule/base-name rules)]
                   [pattern->terminal
                    (key-by rule/base-pattern (filter rule/terminal? rules))])
                  ([rule rules])
          (extract-terminals-from-pattern 
            (rule/base-pattern rule)
            new-name-counter
            extended-rules
            name->rule
            pattern->terminal))])
      ; Then, go through the non-terminal rule patterns and replace instances of
      ; terminal literals with the name of the corresponding terminal rule.
      ; Return the complete list of rules (terminals and non-terminals), where
      ; now patterns within non-terminal rules do not contain any literals --
      ; only references to other rules.
      (let-values ([(terminals non-terminals)
                    (partition rule/terminal? extended-rules)])
        (append 
          terminals
          (for/list ([rule non-terminals])
            (replace-pattern rule
              (lambda (pattern) 
                (replace-literals-with-terminals pattern pattern->terminal
)))))))))

(define (enumeration-values name pattern)
  "Return a list of the string values associated with the enumeration having
   the specified rule @var{pattern}. Raise a user error if the @var{pattern}
   is not consistent with that of an enumeration. The specified rule
   @var{name} is used in diagnostics only."
  (match pattern
    [(list 'Alternation (? string? values) ...) values]
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
         (for/set ([rule rules] #:when (rule/complex? rule))
           (rule/base-name rule))]
        [terminal->type
         (for/hash ([rule rules] #:when (rule/terminal? rule))
           (values (rule/base-name rule) (terminal-type rule)))])
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
         (rule/terminal name pattern (assert-terminal-modifier name modifier))
         (let ([bindings (binding-paths pattern)])
           (cond
             ; If it has bindings, then it's a class, and it better not have
             ; any modifiers.
             [(not (empty? bindings))
              (unless (empty? modifiers)
                (raise-user-error
                  (~a "Rule named " (qt name) " looks like a class because it "
                    "has bindings " (map first bindings) ", yet it has "
                    "modifier " (qt (first modifier)) ". A class cannot have "
                    "a modifier.")))
              (rule/class name pattern bindings)]
             ; If it's declared as an enumeration, then it's an enumeration.
             [(equal? modifier 'enumeration)
              (rule/enumeration name 
                                pattern 
                                (enumeration-values name pattern))]
             ; Otherwise, it's an other, and it better not have modifiers.
             [else
              (unless (empty? modifiers)
                (raise-user-error
                  (~a "Rule named " (qt name) " does not look like a terminal "
                    "(a terminal is just a string literal or a regular "
                    "expression), but it has modifier " (qt (first modifiers))
                    ". Only terminals and enumerations may have a modifier.")))
              (rule/other name pattern)]))))]))

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

(define (class-category rule name->rule)
  (match rule
    [(rule/class name pattern (list (list binding-names binding-paths) ...))
     (let* ([with-type               (lambda (binding)
                                       (list binding 
                                             (binding-type-relative-to
                                               binding pattern name->rule)))]
            [binding-types           (map with-type 
                                          (remove-duplicates binding-names))]
            [prefix-path             (apply common-prefix binding-paths)]
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
                (match (binding-type-relative-to
                         name 
                         (after-prefix prefix-path path pattern)
                         name->rule)
                  [(array _)    #f]
                  [(nullable _) #f]
                  [_            #t])))
            ; The above is true, so this is a choice.
            (schema/choice name rule binding-types)
            ; The above is not true, so this is a sequence.
            (schema/sequence name rule binding-types))]
         ; The common ancestor is not an Alternation, so this is a sequence.
         [_ (schema/sequence name rule binding-types)]))]))

(define (rules->schema-types rules [name->rule (key-by rule/base-name rules)])
  "TODO"
  (let ([rules (verify-binding-consistency rules)])
    (for/list ([rule rules] #:when (rule/complex? rule))
      (match rule
        [(struct rule/class _) 
         (class-category rule name->rule)]
        [(rule/enumeration name pattern values)
         (schema/enumeration name rule values)]))))

; Input language: "others" have been inlined, terminals have been extracted.
(define (dependency-list rules)
  "Return a list of edges (list A B), where A and B are rule names, and an edge
   (list A B) indicates that A depends upon B, i.e. B is referenced in the
   pattern of A."
  (sequence->list
    (in-generator
      (for ([rule rules] #:when (rule/complex? rule))
        (match rule
          [(rule/complex name rule-pattern)
            (let recur ([tree rule-pattern])
              (match tree
                [(? symbol? dependency)
                (let ([edge (list name dependency)])
                  (yield edge))]
                [(list 'Bound _ pattern) (recur pattern)]
                [(list _ patterns ...) (for-each recur patterns)]))])))))

(define (dependency-graph rules)
  "Return a directed graph whose vertices are the names of rules and whose
   edges A -> B indicate that rule A depends upon B, i.e. B is referenced in
   the pattern of A."
   (let ([graph (directed-graph (dependency-list rules))])
     ; Now that all of the edges are in, add each rule name as a vertex. Any
     ; vertices mentioned in the edges, above, will have already been added,
     ; but there might be isolated vertices that weren't included. Adding a
     ; vertex is idempotent.
     (for ([rule rules])
       (add-vertex! graph (rule/base-name rule)))

     graph))

(define (remove-unreachable rules)
  "Return a list of rules that is a copy of the specified @var{rules} having
   had removed from it all rules that are not depended upon by some class or
   enumeration."
  (let* ([graph (dependency-graph rules)]
         [roots (~>> rules (filter rule/complex?) (map rule/base-name))]
         [garbage (list->set (mark-and-sweep* graph roots))]
         [keep? (lambda (rule)
                  (not (set-member? garbage (rule/base-name rule))))])
    (filter keep? rules)))
