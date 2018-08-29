#lang racket

(provide (all-defined-out))
; These structs are shared by multiple modules. They describe the types arising
; from a parsley grammar.

; These structs distinguish types the multiplicity of values they allow.
(struct occurrence            (type) #:transparent)
(struct scalar     occurrence ()     #:transparent) ; exactly one
(struct array      occurrence ()     #:transparent) ; zero or more
(struct nullable   occurrence ()     #:transparent) ; zero or one

; These structs distinguish types that are user-defined, in the XSD sense, or
; builtin.
(struct kind    (name)  #:transparent) ; user-defined or builtin
(struct basic   kind () #:transparent) ; e.g. string, integer
(struct complex kind () #:transparent) ; user-defined type

; These schema structs contain sufficient information to generate corresponding
; XSD types or C++ class parsers.
(struct schema/base        (name rule)                 #:transparent)
(struct schema/sequence    schema/base (element-types) #:transparent)
(struct schema/choice      schema/base (element-types) #:transparent)
(struct schema/enumeration schema/base (values)        #:transparent)
