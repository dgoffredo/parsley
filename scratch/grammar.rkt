'(Grammar
  (Rule
   Grammar
   (Concatenation
    (Bound rules Rule)
    (Star (Concatenation (Plus BLANK_LINE) (Bound rules Rule)))
    (Star BLANK_LINE)))
  (Rule
   Rule
   (Concatenation
    (Question (Bound ignore "ignore"))
    (Bound name IDENTIFIER)
    (Alternation "::=" ":")
    (Bound pattern Pattern)))
  (Rule
   Pattern
   (Alternation
    (Bound alternation Alternation)
    (Bound concatenation Concatenation)))
  (Rule
   Alternation
   (Concatenation
    (Bound patterns PatternTerm)
    (Plus (Concatenation "|" (Bound patterns PatternTerm)))))
  (Rule Concatenation (Plus (Bound patterns PatternTerm)))
  (Rule
   PatternTerm
   (Alternation
    (Bound bound BoundPatternTerm)
    (Bound unbound QuantifiedPatternTerm)))
  (Rule
   BoundPatternTerm
   (Concatenation
    (Bound name IDENTIFIER)
    ":"
    (Bound term QuantifiedPatternTerm)))
  (Rule
   QuantifiedPatternTerm
   (Alternation
    (Concatenation (Bound star UnquantifiedPatternTerm) "*")
    (Concatenation (Bound plus UnquantifiedPatternTerm) "+")
    (Concatenation (Bound question UnquantifiedPatternTerm) "?")
    (Bound term UnquantifiedPatternTerm)))
  (Rule
   UnquantifiedPatternTerm
   (Alternation
    (Bound literal STRING)
    (Bound regex REGEX)
    (Bound rule IDENTIFIER)
    (Bound empty EMPTY)
    (Concatenation "(" (Bound pattern Pattern) ")")))
  (Rule IDENTIFIER (regex "[a-zA-Z_][0-9a-zA-Z_]*"))
  (Rule STRING (regex "\"([^\\\\\"]|\\\\.)*\""))
  (Rule REGEX (regex "/([^\\\\/]|\\\\.)*/"))
  (Rule EMPTY (regex "\\(\\)"))
  (Rule ignore COMMENT (regex "\\(\\*([^*]|\\*[^)])*\\*\\)"))
  (Rule BLANK_LINE (regex "\\s*\\n\\s*\\n\\s*"))
  (Rule ignore WS_LEFT (regex "\\s+(?=\\S)"))
  (Rule ignore WS_END (regex "\\s+$")))
