#lang racket

(require threading
         "../parsley/lexer.rkt"
         "../parsley/parser.rkt"
         "../parsley/types.rkt"
         "../parsley/productions.rkt"
         "../parsley/parser-functions.rkt"
         "../parsley/generate-schema.rkt"
         "../parsley/generate-lexer.rkt"
         "../parsley/generate-parser.rkt")

(define this-dir 
  (~> 'run-file
      find-system-path
      path->complete-path
      path-only))

(define parsley-file
  (build-path this-dir 'up "parsley" "test-grammars" "parsley.parsley"))

(define parsley-text
  (port->string (open-input-file parsley-file)))

(define raw-tokens
  (for/list ([token (lex (open-input-string parsley-text))])
      token))

(define (to-file-named name object)
  (with-output-to-file (build-path this-dir name) #:exists 'replace
    (lambda () (pretty-print object))))

(to-file-named "tokens.rkt" raw-tokens)

(define grammar
  (parse (open-input-string parsley-text)))

(to-file-named "grammar.rkt" grammar)

(define types-and-tokens
  (get-productions grammar))

(to-file-named "productions.rkt" types-and-tokens)

(match-define (productions types tokens) types-and-tokens)

(with-output-to-file (build-path this-dir "schema.xsd") #:exists 'replace
  (lambda () (print-schema types parsley-text (current-output-port))))

(generate-lexer tokens "snazzy" "Lexer" "BloombergLP" this-dir)

(define functions
  (reader-functions types-and-tokens))

(to-file-named "functions.rkt" functions)

(with-output-to-file (build-path this-dir "snazzy_parser.h") #:exists 'replace
  (lambda ()
    (display 
      (parser-header (map schema/base-name types)
                     "snazzy"
                     "BloombergLP" 
                     "Parser" 
                     "Lexer"))))
