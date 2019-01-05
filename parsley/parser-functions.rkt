#lang racket

(provide reader-functions
         (struct-out reader-function)
         (struct-out reader/base)
         (struct-out reader/compound)
         (struct-out reader/concatenation)
         (struct-out reader/alternation)
         (struct-out reader/star)
         (struct-out reader/question)
         (struct-out reader/term)
         (struct-out reader/token)
         (struct-out reader/enumeration)
         (struct-out member-accessor))

(require "productions.rkt"
         "types.rkt"
         "names.rkt"
         "counter.rkt"
         "codegen-util.rkt"
         "debug.rkt"
         srfi/1
         racket/generator
         racket/hash)

; This struct describes a C++ function that will appear in the anonymous
; namespace of the parser's implementation. Each reader-function reads either
; a class or some part of a class.
(struct reader-function (name output-type reader) #:prefab)

; These structs describe parts of a reader-function. Each node in a production
; rule pattern corresponds to a strategy for reading that production from
; input, e.g. (Concatenation A B?) produces a concatenation-reader whose
; readers are a term-reader and a question-reader, respectively.
(struct reader/base          (pattern)             #:prefab)
(struct reader/compound      reader/base (readers) #:prefab)
(struct reader/concatenation reader/compound ()    #:prefab)
(struct reader/alternation   reader/compound ()    #:prefab)
(struct reader/star          reader/base (term)    #:prefab)
(struct reader/question      reader/base (term)    #:prefab)
; The term-output field of reader/term can be any of:
; - 'forward, indicating that the output parameter of the calling function
;   is forwarded to it, or
; - a member-accessor object, indicating that the output goes to a member of
;   the output parameter of the calling function, or
; - #f, indicating no output (pass a null pointer for the output parameter).
; The function field of reader/term can be any of:
; - a string naming the function, or
; - a reader/token object indicating the token to be read.
(struct reader/term        reader/base (term-output function) #:prefab)
(struct reader/token       reader/base (name)                 #:prefab)
(struct reader/enumeration reader/base (class-name function)  #:prefab)
(struct member-accessor    (name)                             #:prefab)

(define (alternation/concatenation? symbol)
  (member symbol '(Alternation Concatenation)))

(define (star/question? symbol)
  (member symbol '(Star Question)))

(define (contains-bindings? pattern)
  (match pattern
    [(list 'Bound _ _) #t]
    [(list _ args ...) (any contains-bindings? args)]
    [_ #f]))

(define (pattern->output-type pattern output-type)
  (debug "in" (~s (list 'pattern->output-type pattern output-type)))
  (match output-type
    [(occurrence (basic _)) output-type] ; basic -> keep (e.g. for enum values)
    [#f #f]                              ; none -> none
    [_                                   ; otherwise, only if has bindings
     (if (contains-bindings? pattern)
       output-type
       #f)]))

; TODO: The arithmetic grammar makes me realize that an optimization is
;       available after this step: Find functions that are the same, and pick
;       just one of them, renaming as necessary.
(define (pattern->reader pattern 
                         output-type 
                         element->accessor-name
                         name->production
                         get-reader-function-name)
  (let recur ([pattern (restore-question pattern)]
              [output-type output-type] 
              [element->accessor-name element->accessor-name]
              [name->production name->production]
              [get-reader-function-name get-reader-function-name])
    (match pattern ; TODO: 
                   ; Probably want the restore-question step to be more
                   ; visible from the outside, but then need a replace-rule
                   ; procedure for schema/* types analogous to replace-pattern
                   ; for rule/* types.
      ; TODO: This case and the symbol case share some code. Figure out how to
      ;       refactor the common stuff.
      [(list 'Bound element-name binding-pattern)
       (debug "pattern->reader Bound case:" element-name binding-pattern)
       (reader/term
         ; pattern
         pattern
         ; term-output
         (if output-type
           (member-accessor (dict-ref element->accessor-name element-name))
           #f)
         ; function
         (match (dict-ref name->production binding-pattern)
           ; classes get read by their "read" function overload
           [(schema/base name _) "read"]
           ; tokens get read by a call to a read overload with a token kind
           [(token name pattern _ _) (reader/token pattern name)]))]
      [(? symbol? name)
       (debug "pattern->reader symbol case:" name)
       (let ([output (if output-type 'forward #f)])
         (match (dict-ref name->production name)
           ; classes get read by their "read" function overload
           [(schema/base name _) (reader/term name output "read")]
           ; tokens get read by a call to a read overload with a token kind
           [(token name pattern _ _)
            (reader/term pattern output (reader/token pattern name))]))]
      ; TODO: This case and the star/question case share a lot of code. Figure
      ;       out how to refactor the common stuff.
      [(list (? alternation/concatenation? which) patterns ...)
       (debug "pattern->reader alt/cat case:" which patterns)
       (let ([reader-constructor
              (match which
                ['Concatenation reader/concatenation]
                ['Alternation reader/alternation])])
         (reader-constructor
           pattern
           (for/list ([pattern patterns])
             (let* ([output-type (pattern->output-type pattern output-type)]
                    [reader
                     (recur pattern
                            output-type
                            element->accessor-name
                            name->production
                            get-reader-function-name)])
                (debug "in pattern->reader alt/cat with output-type:"
                       output-type)
                (match reader
                  [(struct reader/compound _)
                   (let ([name (get-reader-function-name output-type reader)])
                     (debug "in pattern->reader alt/cat about to emit term "
                            "for output-type:" output-type 
                            " and reader/compound function name:" name)
                     (reader/term pattern (if output-type 'forward #f) name))]
                  [_ 
                   (debug "in pattern->reader alt/cat about to emit term for "
                          "output-type:" output-type 
                          " and reader:" reader)
                   reader])))))]
      [(list (? star/question? which) term-pattern)
       (debug "pattern->reader star/question case:" which term-pattern)
       (let* ([output-type (pattern->output-type term-pattern output-type)]
              [reader
               (recur term-pattern
                      output-type
                      element->accessor-name
                      name->production
                      get-reader-function-name)]
              [reader-constructor
               (match which 
                 ['Star reader/star]
                 ['Question reader/question])])
           (reader-constructor
             pattern
             (match reader
               [(struct reader/compound _)
                (let ([name (get-reader-function-name output-type reader)])
                  (reader/term term-pattern 
                               (if output-type 'forward #f)
                               name))]
               [_ reader])))])))

(define (productions-by-name types-and-tokens)
  (match types-and-tokens
    [(productions types tokens)
     (hash-union
       (for/hash ([type types])
         (values (schema/base-name type) type))
       (for/hash ([token tokens])
         (values (token-name token) token)))]))

(define (sequence/choice-reader-function class-name
                                         pattern
                                         element-types
                                         name->production
                                         name-casing)
  (let ([output-type  (scalar (complex class-name))])
    (reader-function
      ; name
      "read"
      ; output type
      output-type
      ; reader
      (pattern->reader
        ; pattern
        pattern
        ; output type
        output-type
        ; element->accessor-name
        (for/hash ([elem-type element-types])
          (match elem-type
            [(list elem-name _)
              (values elem-name (name-casing elem-name))]))
        ; hash-table mapping productions by name
        name->production
        ; get-reader-function-name (to create child readers)
        (let ([tick (make-counter 1)])
          (lambda (output-type reader)
            (debug "in"
              (~s (list 'get-reader-function-name output-type reader)))
            (let ([name (~a "read" class-name "_" (tick))])
              (yield (reader-function name output-type reader))
              name)))))))

(define (reader-functions types-and-tokens)
  (let ([name->production (productions-by-name types-and-tokens)])
    (match types-and-tokens
      [(productions types tokens)
       (sequence->list
         (in-generator
           (for ([type types])
             (match type
               [(schema/sequence name rule element-types)
                (yield
                  (sequence/choice-reader-function
                    (class-case name)        ; class-name
                    (rule/base-pattern rule) ; pattern
                    element-types
                    name->production
                    attribute-case))] ; name-casing (different in choice)
               [(schema/choice name rule element-types)
                (yield
                  (sequence/choice-reader-function
                    (class-case name)        ; class-name
                    (rule/base-pattern rule) ; pattern
                    element-types
                    name->production
                    maker-case))] ; name-casing (different in sequence)
               [(schema/enumeration name rule values)
                ; An enumeration class "Foo" yields two functions:
                ; first "readFooValue", which outputs to a string and is an
                ; alternation reader among the tokens matching the values of
                ; Foo, and second "read" overloaded for "Foo::Value", which
                ; calls "readFooValue" and then converts the resulting string
                ; into a "Foo::Value" using the conversion routine in "Foo".
                (let ([read-value-name (~a "read" (class-case name) "Value")]
                      [string-type (scalar (basic 'string))])
                  ; the function that outputs the string value of the enum.
                  (yield
                    (reader-function
                      ; name
                      read-value-name
                      ; output type
                      string-type
                      ; reader
                      (pattern->reader
                        ; pattern
                        (rule/base-pattern rule)
                        ; output type
                        string-type
                        ; element->accessor-name (not used in this case)
                        #f
                        ; hash-table mapping productions by name
                        name->production
                        ; get-reader-function-name (not used in this case)
                        #f)))
                  ; the function that outputs the enum value
                  (yield
                    (reader-function
                    ; name
                    "read"
                    ; output type (the name of the enum type within the class)
                    (scalar (complex (~a (class-case name) "::Value")))
                    ; reader
                    (reader/enumeration
                      ; pattern
                      (rule/base-pattern rule)
                      ; class name
                      (class-case name)
                      ; (value reader) function
                      read-value-name))))]))))])))

(define (restore-question pattern)
  "Replace occurrences of alternations involving nil instead with a question
   mark operator, e.g. transform
       foo | ()
   into
       foo?
   Question mark operators are removed during previous analysis of the rules,
   but they are useful to recognize for parser generation (the resulting code
   makes more sense)."
  (match pattern
    [(list 'Alternation subpattern '())
     (debug "restore-question Alternation:" pattern)
     (list 'Question (restore-question subpattern))]

    [(list operation args ...)
     (debug "restore-question recur:" pattern)
     (cons operation (map restore-question args))]

    [otherwise
     (debug "restore-question otherwise:" pattern)
     otherwise]))