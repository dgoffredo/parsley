#lang racket

(provide (all-defined-out))
; These structs are shared by multiple modules. They describe the types arising
; from a parsley grammar.

; These rule structs categorize the raw Rule nodes from the grammar parse tree.
(struct rule/base        (name pattern)          #:prefab)
(struct rule/terminal    rule/base    (modifer)  #:prefab)
(struct rule/other       rule/base    ()         #:prefab)
(struct rule/complex     rule/base    ()         #:prefab)
(struct rule/class       rule/complex (bindings) #:prefab)
(struct rule/enumeration rule/complex (values)   #:prefab)

; These structs distinguish types by the multiplicity of values they allow.
(struct occurrence            (type) #:prefab)
(struct scalar     occurrence ()     #:prefab) ; exactly one
(struct array      occurrence ()     #:prefab) ; zero or more
(struct nullable   occurrence ()     #:prefab) ; zero or one

; These structs distinguish types that are user-defined, in the XSD sense, from
; those that are builtin.
(struct kind    (name)  #:prefab) ; user-defined or builtin
(struct basic   kind () #:prefab) ; e.g. string, integer
(struct complex kind () #:prefab) ; user-defined type

; These schema structs contain sufficient information to generate corresponding
; XSD types or C++ class parsers.
(struct schema/base        (name rule)                 #:prefab)
(struct schema/sequence    schema/base (element-types) #:prefab)
(struct schema/choice      schema/base (element-types) #:prefab)
(struct schema/enumeration schema/base (values)        #:prefab)

; This struct contains all of the information necessary to lex a token and then
; handle it appropriately. "pattern" is a regular expression pattern. "type"
; is of the form (basic symbol?), and "ignore?" is a boolean indicating whether
; the parser should skip this kind of token.
(struct token (name pcre-pattern type ignore?) #:prefab)
