#lang racket

(provide get-productions
         (struct-out productions))

(require "types.rkt"
         "key-view.rkt"
         "mark-and-sweep.rkt"
         "counter.rkt"
         "codegen-util.rkt" ; TODO: not the best name, since no codegen here
         graph
         threading
         srfi/1
         racket/generator)

(define-for-syntax debugging? #f)

; This struct contains the results of this module's work. "types" is a list
; of instances of the schema/* structs, and "tokens" is a list of instances of
; the token struct.
(struct productions (types tokens) #:transparent)

(define (get-productions grammar)
  (let* ([rules
          (~> grammar
              rest                         ; get rules from (Grammar rules ...)
              categorize-rules             ; class, enumeration, rule, or other
              inline-others                ; non-terminal, non-class -> inlined
              extract-terminals            ; literals like "(" and /[0-9]/
              remove-pluses                ; simplify: expr+  -->  expr expr*
              simplify-patterns            ; further optimizations
              remove-unreachable           ; run "garbage collection" on rules 
              remove-questions             ; simplify: expr?  -->  (expr | ())
              recalculate-bindings         ; binding paths likely changed above
              verify-binding-consistency)] ; binding has same type everywhere
         [types (rules->schema-types rules)]  
         [tokens (rules->tokens rules)])
    (productions types tokens)))

(define (rules->tokens rules)
  (~>> rules (filter rule/terminal?) (map rule->token)))

(define (escape-pcre-pattern pattern)
  (regexp-replace* #px"[.?*+^$[\\]\\\\(){}|-]" pattern "\\\\&"))

(define (regexify regex-or-string)
  (match regex-or-string
    [(list 'regex pcre-pattern)
     pcre-pattern]
    [(? string? raw-pattern)
     (escape-pcre-pattern raw-pattern)]))

(define (rule->token rule)
  "Assuming @var{rule} is a terminal, return a token struct from it."
  (match rule
    [(rule/terminal name pattern modifier)
     (token name
            (regexify pattern)
            (terminal-modifier->type modifier) 
            (equal? modifier 'ignore))]))

(define (terminal-modifier->type modifier)
  "Return the terminal type implied by the specified @var{modifier}."
  (match modifier
    [(or #f 'ignore) (basic 'string)]
    ['enumeration    (error "logic error: modifier should come from terminal")]
    [other           (basic other)]))

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
  ; pattern+  ->  pattern pattern*
  [`(Plus ,pattern)
    (let ([pattern (remove-plus pattern)])
      `(Concatenation ,pattern (Star ,pattern)))])

(define-tree-transformer remove-question
  ; pattern?  ->  pattern | ()
  [`(Question ,pattern)
   `(Alternation ,(remove-question pattern) ())])

(define (simplify-pattern pattern)
  "Transform the specified rule pattern in the following ways:

   - Flatten directly nested alternations, e.g. A | (B | C) -> A | B | C.
   - Elide duplicate alternates, e.g. A | A -> A, A | B | A -> A | B.
   - Flatten directly nested concatenations, e.g. A (B C) -> A B C.
   - Combine directly nested stars, e.g. (A*)* -> A*.
   - Combine directly nested questions, e.g. (A?)? -> A?.
   - Elide questions of starred patterns, e.g. (A*)? -> A*.
   - Truncate and questionify alternations that have a question or star within
     them, e.g. A | B? | C | D -> (A | B)?
                A | B* | C | D -> A | B*"
  (match pattern
    [(list 'Alternation before ... (list 'Alternation inner ...) after ...)
     (debug "simplify Alternation:" pattern)
     (simplify-pattern `(Alternation ,@before ,@inner ,@after))]

    [(list 'Alternation  before ... same between ... same after ...)
     (debug "simplify duplicated Alternation:" pattern)
     (simplify-pattern `(Alternation ,@before ,same ,@between ,@after))]

    [(list 'Concatentaion before ... (list 'Concatenation inner ...) after ...)
     (debug "simplify Concatenation:" pattern)
     (simplify-pattern `(Concatenation ,@before ,@inner ,@after))]

    [`(Star (Star ,inner))
     (debug "simplify Star:" pattern)
     (simplify-pattern `(Star ,inner))]

    [`(Question (Question ,inner))
     (debug "simplify Question:" pattern)
     (simplify-pattern `(Question ,inner))]

    [`(Question (Star ,inner))
     (debug "simplify Question:" pattern)
     (simplify-pattern `(Star ,inner))]

    [(list 'Alternation before ... (list 'Star starred) _ ...)
     (debug "simplify Alternation containing star:" pattern)
     (simplify-pattern `(Alternation ,@before (Star ,starred)))]

    [(list 'Alternation before ... (list 'Question questioned) _ ...)
     (debug "simplify Alternation contaning question:" pattern)
     (simplify-pattern `(Question (Alternation ,@before questioned)))]

    [(list operation args ...)
     (debug "simplify Recur down list:" pattern)
     (let ([simplified (cons operation (map simplify-pattern args))])
       (debug "pattern:" pattern "simplified:" simplified)
       (if (equal? pattern simplified)
         simplified                       ; no more simplifying can be done
         (simplify-pattern simplified)))] ; maybe we can simplify further

    [otherwise
     (debug "simplify otherwise:" pattern)
     otherwise]))

(define (remove-pluses rules)
  (for/list ([rule rules])
    (replace-pattern rule remove-plus)))

(define (remove-questions rules)
  (for/list ([rule rules])
    (replace-pattern rule remove-question)))

(define (simplify-patterns rules)
  (for/list ([rule rules])
    (replace-pattern rule simplify-pattern)))

(define (recalculate-bindings rules)
  (for/list ([rule rules])
    (match rule
      [(rule/class name pattern _)
       (rule/class name pattern (binding-paths pattern))]
      [otherwise otherwise])))

(define (inline-others rules)
  ; Transform the list of @var{rules} such that it's as if there were never
  ; any rules of the category rule/other, and that instead the patterns that
  ; define those rule/other were expanded wherever they're referenced in other
  ; rules.
  (let ([name->rule (key-by rule/base-name rules)])
    (define-tree-transformer transform
      [(list 'Bound name pattern)
       (list 'Bound name (transform pattern))]
      [(? symbol? name) (=> pass)
       (match (dict-ref name->rule name)
         [(rule/other _ pattern) pattern]
         [_ (pass)])])

    (for/list ([rule rules] #:when (not (rule/other? rule)))
      (replace-pattern rule transform))))

(define (alternation/concatenation? symbol)
  (member symbol '(Alternation Concatenation)))

(define (terminal-type rule)
  "Return the (basic) type of the specified terminal @var{rule}. If its type
   was annotated in the grammar, then it will have that type. Otherwise, it
   will default to the string type."
  (match rule
    [(rule/terminal _ _ modifier)
     (case modifier
       [(ignore #f) (basic 'string)]      ; ignored/default terminal -> string
       [else        (basic modifier)])])) ; type-annotated terminal -> the type

(define (bound-pattern-type pattern name->rule)
  "Return the type (either (basic something) or (complex something)) of the
   specified @var{pattern} using the dict @var{name->rule} to look up rules
   (from which types can be deduced). This procedure applies to the particular
   case of the pattern in '(Bound name pattern), i.e. in a grammar rule,
   \"name:pattern\"."
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
  "Return the full type (name and occurence) associated with the specified
   @var{binding} name as seen relative to the specified rule pattern
   @var{tree}. Use the dict @var{name->rule} in order to deduce the types of
   other rules referenced in the @var{tree}."
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

(define (binding-paths tree)
  " :: parse-tree -> (list (list name path) ...)
    where name :: string?
          path :: (list integer?)"
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

(define (replace-pattern rule value-or-procedure)
  "Return a copy of @var{rule} that has had its pattern replace by the
   specified value if @var{value-or-procedure} is not a procedure, or
   otherwise has had its pattern transformed by the procedure
   @var{value-or-procedure}. If @var{pattern-or-procedure} is a procedure,
   then it must take a pattern as its one argument and produce a pattern."
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

(define (unique-terminal-name names counter)
  "Return a symbol suitable for use as the name of a lexer token. Use the
   specified set of @var{names} to ensure that symbol is not already taken,
   and use the specified @var{counter} procedure to generate integers from
   which to make new symbols."
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

(define (replace-literals-with-terminals pattern pattern->terminal)
  "Return a modified copy of @var{pattern} that has had within it replaced
   all literal strings and regular expressions with the names of the
   corresponding terminals as looked up in @var{pattern->terminal}."
  (define-tree-transformer transform
    [(? terminal-pattern? pattern)
     (rule/base-name (dict-ref pattern->terminal pattern))])

  (transform pattern))

(define (extract-terminals rules)
  "Return an extended version of the specified list of @var{rules}, where
   each literal string or regular expression appearing within the patterns of
   non-terminal rules has been given a name and its own (terminal) rule, and
   all references to the terminal literal have been replaced with its name. Use
   the dict @var{name->rule} look up rules when they are seen in other rule
   patterns.
   As an example,
     (list (rule/class 'Comment '(Concatenation \"#\" (Regex \".*\") ...)
           (rule/enumeration 'Color '(Alternation \"red\" \"green\" B) ...)
           (rule/terminal 'B \"blue\" ...))
   becomes
     (list (rule/class 'Comment '(Concatenation TOKEN_1 TOKEN_2) ...)
           (rule/enumeration 'Color '(Alternation TOKEN_3 TOKEN_4 B) ...)
           (rule/terminal 'B \"blue\" ...)
           (rule/terminal 'TOKEN_1 \"#\" ...)
           (rule/terminal 'TOKEN_2 '(Regex \".*\") ...)
           (rule/terminal 'TOKEN_3 \"red\" ...)
           (rule/terminal 'TOKEN_4 \"green\" ...))"
  (let ([name->rule (key-by rule/base-name rules)] ; dict to look up rules
        [new-name-counter (make-counter 1)])       ; for making new names
    ; First, get the extended list of rules by extracting terminal literals
    ; from rule patterns.
    (let-values
      ([(extended-rules name->rule pattern->terminal)
        (for/fold ([extended-rules rules]
                   [name->rule name->rule]
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
                          (~a rule-name "." name " has undefined type. Note "
                            "that a binding must refer to either a terminal "
                            "or to a rule that itself has bindings.")))])
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

(define (categorize-rules rules)
  (map categorize-rule rules))

(define (categorize-rule rule)
  " :: (list 'Rule args ...) -> rule/class 
                              | rule/terminal 
                              | rule/enumeration 
                              | rule/other"
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
  "Return the final subtree within @var{tree} reached by visiting the i'th
   child of the current subtree, for each i in @var{path}. e.g.
       (follow-path '(2 1 3) '(A x u (B t (C y x h (D q r s) w) v u)))
                              ;  0 1 2  0 1  0 1 2 3  0 1 2  4  2 3
   is
       '(D q r s)
   Note how the initial element of each list (the one in call position) is
   ignored, and the index starts at zero with the second element in the list."
  (for/fold ([tree tree]) ([index path])
    (match tree
      [(list _ subtrees ...)
       (list-ref subtrees index)])))

(define (get-choice-bindings pattern name->rule)
  "Return the list of typed bindings, each a (list name type), from which
   the specified @var{pattern} will produce exactly one (not none of them
   and not two or more of them), or return #f if @var{pattern} does not so
   describe a choice."
  (struct choice-set (binding-types-set) #:transparent)

  (match
    (let recur ([pattern pattern])
      (match pattern
        ; a binding is a set of one
        [(list 'Bound name bound-pattern)
         (debug "in Bound case with " name ": " bound-pattern)
         (let ([type (scalar (bound-pattern-type bound-pattern name->rule))])
           (choice-set (set (list name type))))]
    
        ; an alternation between only sets is their union
        [(list 'Alternation (choice-set binding-types-sets) ...)
         (debug "in Alternation case with the sets: " binding-types-sets)
         (choice-set (apply set-union binding-types-sets))]
    
        ; a concatenation of one set with non-sets is just that same set
        [`(Concatenation
            ,@(list-no-order 
                (? choice-set? choices) (? (negate choice-set?) _) ...))
         (debug "in Concatenation case with choices: " choices)
         choices]
    
        ; a star eliminates choice-ness, since it can produce more than one
        [(list 'Star _ ...)
         (debug "found a star")
         #f]
  
        ; it's a precondition that questions and pluses are removed
        [(list 'Question _ ...)
         (raise-arguments-error 'pattern
           "Pattern (tree) must not contain any Question nodes."
           "pattern" pattern)]
  
        [(list 'Plus _ ...)
         (raise-arguments-error 'pattern
           "Pattern (tree) must not contain any Plus nodes."
           "pattern" pattern)]
  
        ; if we recurred down a tree and found nothing, then nothing
        [(list operation #f ...)
         (debug "found an operation whose children were all #f")
         #f]
  
        ; recur on down the tree
        [(list operation args ...)
         (debug "Recurring for operation " operation)
         (recur (cons operation (map recur args)))]
    
        ; otherwise, not interested
        [otherwise
         (debug "In the \"otherwise\" case for: " otherwise) 
         #f]))
    ; A choice-set was produced. Return the list of (list binding-name type)
    [(choice-set bindings) (set->list bindings)]
    ; Not a choice. Return #f.
    [_ #f]))

(define (class-category rule name->rule)
  " :: rule/class -> schema/choice | schema/sequence"
  (match rule
    [(rule/class name pattern (list (list binding-names _) ...))
     (match (get-choice-bindings pattern name->rule)
       ; The pattern represents a choice among its bindings if
       ; get-choice-bindings returns a list of bindings/types containing every
       ; binding in this rule, and if there're two or more bindings (a single
       ; element choice is silly).
       [(list bindings ..2)
        (=> fail)
        (match bindings
          [(list (list names _) ...)
           (if (equal? (list->set names) (list->set binding-names))
             (schema/choice name rule bindings)                     ; -> choice
             (fail))])]
       ; The pattern is not a choice, so it'll be a sequence. Figure out what
       ; the types of its members are.
       [_
        (let* ([with-type (lambda (binding) 
                            (list binding 
                                  (binding-type-relative-to 
                                    binding pattern name->rule)))]
               [binding-types (~>> binding-names
                                   remove-duplicates
                                   (map with-type))])
          (schema/sequence name rule binding-types))])]))         ; -> sequence
  
(define (rules->schema-types rules)
  " :: (list (rule/class | rule/enumeration) ...)
        -> (list (schema/sequence | schema/choice | schema/enumeration) ...)"
  (let ([name->rule (key-by rule/base-name rules)])
    (for/list ([rule rules] #:when (rule/complex? rule))
      (match rule
        [(struct rule/class _) 
         (class-category rule name->rule)]
        [(rule/enumeration name pattern values)
         (schema/enumeration name rule values)]))))
        

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

(define (ignored-terminal? rule)
  (match rule
    [(rule/terminal _ _ 'ignore) #t]
    [_ #f]))

(define (remove-unreachable rules)
  "Return a list of rules that is a copy of the specified @var{rules} having
   had removed from it all rules that are not depended upon by some class or
   enumeration."
  (let* ([graph (dependency-graph rules)]
         [roots (~>> rules (filter rule/complex?) (map rule/base-name))]
         [garbage (list->set (mark-and-sweep* graph roots))]
         [keep? (lambda (rule)
                  ; If the rule is not garbage, then keep it. Make a special
                  ; exception for ignored terminals, which while not referenced
                  ; by anything, should still be kept.
                  (or (not (set-member? garbage (rule/base-name rule)))
                      (ignored-terminal? rule)))])
    (filter keep? rules)))
