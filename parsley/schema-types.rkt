#lang racket

(require "grammar.rkt" ; grammar of parsley grammar files
         "lexer.rkt"   ; tokenizer for parsley grammar files
         threading)    ; thrush combinator macros

; TODO: how to organize the code needed to implement schema-types.

(define (schema-types grammar-path)
  (~> grammar-path               ; path to the input grammary file (*.parsley)
      open-input-file            ; open the input grammar for reading
      lex                        ; read tokens from the input file
      parse                      ; use token sequence to produce parse tree
      syntax->datum              ; extract data from parser's resulting syntax
      rest                       ; get the rules from (list 'Grammar rules ...)
      categorize-rules           ; rule is class, enumeration, rule, or other
      inline-others              ; non-terminal, non-class rules get inlined
      extract-terminals          ; give names to literals like "(" and /[0-9]/
      remove-unreachable         ; run "garbage collection" on rules 
      remove-pluses              ; simplification: expr+  -->  expr expr*
      remove-questions           ; simplification: expr?  -->  (expr | ())
      verify-binding-consistency ; a binding has the same type everywhere
      rules->schema-types))      ; deduce class types and their element types
  
; schema-types->sxml 
; srl:sxml->xml 
; displayln)