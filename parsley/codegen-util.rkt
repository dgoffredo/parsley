#lang racket

(provide key-by
         string-join*
         pattern->string
         calculate-bloomberg-prefix)

(require "types.rkt")

(define (string-join* separator strings)
  "string-join, but with the arguments in the opposite order"
  (string-join strings separator))

(define (key-by accessor entries)
  " :: (entry -> key) (list entry ...) -> (hash key entry ...)"
  (for/hash ([entry entries])
    (values (accessor entry) entry)))

; Note that the following procedures, up until and including pattern->string,
; contain assumptions about the grammar of a parsley grammar (pattern->string
; obviously has to). Since the "brag" parser generator doesn't encode rule
; precedence, and because deducing precedence from the grammar itself is
; tricky, these procedures instead make assumptions about the grammar. They
; must be kept in sync with the grammar.
(define (suffix-operator? symbol)
  (member symbol '(Star Plus Question)))

(define (suffix-operator symbol)
  (match symbol
    ['Star "*"]
    ['Plus "+"]
    ['Question "?"]))

(define (in-parens pattern)
  (~a "(" (pattern->string pattern) ")"))

(define (suffix-operator-argument pattern)
  ; Concatenations and alternations have to appear in parentheses.
  (match pattern
    [(list (or 'Concatenation 'Alternation) _ ...)
     (in-parens pattern)]
    [_ (pattern->string pattern)]))

(define (concatenation-argument pattern)
  ; Alternations have to appear in parentheses.
  (match pattern
    [(list 'Alternation _ ...)
     (in-parens pattern)]
    [_ (pattern->string pattern)]))

(define (pattern->string pattern)
  (match pattern
    [(? string? string) (~s string)]
    [(? symbol? symbol) (~a symbol)]
    [(list 'regex string) (~a "/" string "/")]
    [(list 'Bound name bound) (~a name ":" (pattern->string bound))]
    [(list 'Concatenation patterns ...)
     (string-join* " " (map concatenation-argument patterns))]
    [(list 'Alternation patterns ...)
     (string-join* " | " (map pattern->string patterns))]
    [(list (? suffix-operator? operator) starred)
     (~a (suffix-operator-argument starred) (suffix-operator operator))]))

(define (calculate-bloomberg-prefix enterprise-namespace)
  "Return the string by which identifiers in the \"BloombergLP\" namespace must
   be prefixed, as determined from the specified enterprise-namespace. If the
   enterprise namespace is \"BloombergLP\", then there need be no prefix. If
   the enterprise namespace is something else, then identifiers in
   \"BloombergLP\" must be qualified."
  (if (equal? enterprise-namespace "BloombergLP")
      "" 
      "BloombergLP::"))