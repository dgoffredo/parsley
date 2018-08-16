#lang at-exp racket

(provide lex)

(require racket/generator
         threading
         brag/support)

(define (pregexp-alternation . args)
  ; This procedure is meant to be used with the @ reader syntax. It takes a
  ; list of arguments produced by @pregexp-alternation{...} syntax, discards
  ; any that are not lists, and then assumes that each of the remaining lists
  ; each has one element. Those elements are stringified using ~a and then
  ; joined together separated by "|" before invoking pregexp on the resulting
  ; string, so that the of this procedure is a regular expression that is an
  ; alternation of subpatterns in the originally specified @ syntax text.
  ; Non-lists are ignored so that they can appear in the @ syntax as comments,
  ; e.g.
  ;     @pregexp-alternation{@list{1st pattern} ignored @list{2nd pattern}}
  ; yields
  ;     (pregexp "1st pattern|2nd pattern")
  (~>> args
    (filter list?)
    (map (match-lambda [(list piece) (~a "(" piece ")")]))
    (string-join _ "|")
    pregexp))

(define get-token
  (let ([regex @pregexp-alternation{
           @list{[a-zA-Z_][0-9a-zA-Z_]*} IDENTIFIER
           @list{"(([^\\"]|\\.)*)"}      STRING
           @list{/(([^\\/]|\\.)*)/}      REGEX
           @list{\(\)}                   EMPTY
           @list{(?m:#(.*)$)}            COMMENT
           @list{@"\\s*\n\\s*\n\\s*"}    BLANK_LINE
           @list{\s+(?=\S)}              WS_LEFT
           @list{\s+$}                   WS_END
           @list{::=|[:|*?+()]}          other]}])
    (lambda (in)
      (~>> in (regexp-try-match regex) matches->utf-8 matches->token))))

(define (matches->utf-8 regex-groups)
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
     (token 'EMPTY)]
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
     (displayln (~a "didn't match: " nonmatching) (current-error-port))
     #f]))

(define (debug token)
  (match token
    [(token-struct 'WS_LEFT _ _ _ _ _ _)
     (void)]
    [(token-struct type value _ _ _ _ _)
     (displayln (~a type " " (or value "")))]
    [other
     (displayln (~a "OTHER " other))])
  token)

(define (lex in)
  (in-generator
    (let loop ()
      (when (not (eof-object? (peek-char in)))
        (yield (or (debug (get-token in))
                   (raise-user-error "Unable to parse a token from input.")))
        (loop)))))
