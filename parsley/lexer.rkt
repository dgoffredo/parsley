#lang racket

(provide lex)

(require racket/generator
         threading
         brag/support)

(define (pregexp-alternation . args)
  (~>> args
    (map (lambda (pattern) (string-append "(" pattern ")")))
    (string-join _ "|")
    pregexp))

(define get-token
  (let ([regex (pregexp-alternation
           "['0-9a-zA-Z_\\-]+"        ; IDENTIFIER
           "\"(([^\\\\\"]|\\\\.)*)\"" ; STRING
           "/(([^\\\\/]|\\\\.)*)/"    ; REGEX
           "\\(\\)"                   ; EMPTY
           "(?m:#(.*)$)"              ; COMMENT
           "\\s*\n\\s*\n\\s*"         ; BLANK_LINE
           "\\s+(?=\\S)"              ; WS_LEFT
           "\\s+$"                    ; WS_END
           "::=|[:|*?+()\\.]")])      ; other
    (lambda (in)
      "Read a token from the specified input port."
      (~>> in (regexp-try-match regex) matches->utf-8 matches->token))))

(define (matches->utf-8 regex-groups)
  "Convert any match results in the specified groups from bytes into utf-8."
  (match regex-groups
    [#f #f]
    [(list items ...)
     (map 
       (match-lambda 
         [#f #f] 
         [string (bytes->string/utf-8 string)])
       items)]))

(define (matches->token regex-groups)
  (match regex-groups
    ; Named subgroups would be nice, wouldn't they?
    [(list _ identifier #f ...)
     (token 'IDENTIFIER (string->symbol identifier))]
    [(list _ #f string inside _ #f ...)
     (token 'STRING inside)]
    [(list _ #f #f #f #f regex inside _ #f ...)
     (token 'REGEX `(regex ,inside))]
    [(list _ #f #f #f #f #f #f #f empty #f ...)
     (token 'EMPTY '())]
    [(list _ #f ... comment inside #f #f #f #f)
     (token 'COMMENT inside #:skip? #t)]
    [(list _ #f ... blank-line #f #f #f)
     ; Note that we don't #skip? blank lines, because they're used as a rule
     ; separator. This way, grammars don't need to use semicolons or periods
     ; for that purpose.
     (token 'BLANK_LINE blank-line)]
    [(list _ #f ... whitespace-left #f #f)
     (token 'WS_LEFT whitespace-left #:skip? #t)]
    [(list _ #f ... whitespace-end #f)
     (token 'WS_END whitespace-end #:skip? #t)]
    [(list _ #f ... other)
     ; "other" tokens are just the string value
     other]
    [nonmatching 
     (displayln 
       (~a "The following regex match groups were not anticipated: "
         nonmatching) 
       (current-error-port))
     #f]))

(define debugging #f)

(define (debug token)
  (when debugging
    (match token
      [(token-struct 'WS_LEFT _ _ _ _ _ _)
       (void)]
      [(token-struct type value _ _ _ _ _)
       (displayln (~a type " " (or value "")))]
      [other
       (displayln (~a "OTHER " other))]))
  token)

(define (lex in)
  (in-generator
    (let loop ()
      (when (not (eof-object? (peek-char in)))
        (yield (or (debug (get-token in))
                   (raise-user-error "Unable to parse a token from input.")))
        (loop)))))
