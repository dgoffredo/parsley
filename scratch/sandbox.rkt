#lang racket

(provide parse parse-it)

(require (prefix-in parser: "../parsley/grammar.rkt")
         (prefix-in lexer:   "../parsley/lexer.rkt")
         threading)

; TODO: Use a location-aware tokenizer!

(define (parse-it)
  (~> "scratch.parsley" open-input-file lexer:lex parser:parse syntax->datum))

(define (parse string)
  (~> string open-input-string lexer:lex parser:parse syntax->datum))
