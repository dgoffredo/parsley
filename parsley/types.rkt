#lang racket

(provide (all-defined-out))
; These structs are shared by multiple modules. They describe the types arising
; from a parsley grammar.

; These rule structs categorize the raw Rule nodes from the grammar parse tree.
(struct rule/base        (name pattern)          #:transparent)
(struct rule/terminal    rule/base    (modifer)  #:transparent)
(struct rule/other       rule/base    ()         #:transparent)
(struct rule/complex     rule/base    ()         #:transparent)
(struct rule/class       rule/complex (bindings) #:transparent)
(struct rule/enumeration rule/complex (values)   #:transparent)

; These structs distinguish types by the multiplicity of values they allow.
(struct occurrence            (type) #:transparent)
(struct scalar     occurrence ()     #:transparent) ; exactly one
(struct array      occurrence ()     #:transparent) ; zero or more
(struct nullable   occurrence ()     #:transparent) ; zero or one

; These structs distinguish types that are user-defined, in the XSD sense, from
; those that are builtin.
(struct kind    (name)  #:transparent) ; user-defined or builtin
(struct basic   kind () #:transparent) ; e.g. string, integer
(struct complex kind () #:transparent) ; user-defined type

; These schema structs contain sufficient information to generate corresponding
; XSD types or C++ class parsers.
(struct schema/base        (name rule)                 #:transparent)
(struct schema/sequence    schema/base (element-types) #:transparent)
(struct schema/choice      schema/base (element-types) #:transparent)
(struct schema/enumeration schema/base (values)        #:transparent)

; This struct contains all of the information necessary to lex a token and then
; handle it appropriately. "pattern" is a regular expression pattern. "type"
; is of the form (basic symbol?), and "ignore?" is a boolean indicating whether
; the parser should skip this kind of token.
(struct token (name pcre-pattern type ignore?) #:transparent)
