#lang racket

(provide parse parse-from-file)

(require (prefix-in grammar: "grammar.rkt") ; parsley grammar file grammar
         (prefix-in lexer:   "lexer.rkt")   ; parsley grammar file tokenizer
         threading)                         ; thrush combinator macros

(define (parse input-port)
  "Return a parse tree derived from the contents of the specified input port."
  (~> input-port
      lexer:lex       ; read tokens from the input file
      grammar:parse   ; use token sequence to produce parse tree
      syntax->datum)) ; extract data from parser's resulting syntax

(define (parse-from-file grammar-path)
  "Return a parse tree derived from the specified parsley grammar file."
  (~> grammar-path    ; path to the input grammary file (*.parsley)
      open-input-file ; open the input grammar for reading
      parse))
