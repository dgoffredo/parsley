(productions
 (list
  (schema/sequence
   'Grammar
   (rule/class
    'Grammar
    '(Concatenation
      (Bound rules Rule)
      (Star
       (Concatenation
        (Concatenation BLANK_LINE (Star BLANK_LINE))
        (Bound rules Rule)))
      (Star BLANK_LINE))
    '((rules (0)) (rules (1 0 1))))
   (list (list 'rules (array (complex 'Rule)))))
  (schema/sequence
   'Rule
   (rule/class
    'Rule
    '(Concatenation
      (Alternation (Bound ignore TOKEN_1) ())
      (Bound name IDENTIFIER)
      (Alternation TOKEN_2 TOKEN_3)
      (Bound pattern Pattern))
    '((ignore (0 0)) (name (1)) (pattern (3))))
   (list
    (list 'ignore (nullable (basic 'string)))
    (list 'name (scalar (basic 'string)))
    (list 'pattern (scalar (complex 'Pattern)))))
  (schema/choice
   'Pattern
   (rule/class
    'Pattern
    '(Alternation
      (Bound alternation Alternation)
      (Bound concatenation Concatenation))
    '((alternation (0)) (concatenation (1))))
   (list
    (list 'concatenation (scalar (complex 'Concatenation)))
    (list 'alternation (scalar (complex 'Alternation)))))
  (schema/sequence
   'Alternation
   (rule/class
    'Alternation
    '(Concatenation
      (Bound patterns PatternTerm)
      (Concatenation
       (Concatenation TOKEN_4 (Bound patterns PatternTerm))
       (Star (Concatenation TOKEN_4 (Bound patterns PatternTerm)))))
    '((patterns (0)) (patterns (1 0 1)) (patterns (1 1 0 1))))
   (list (list 'patterns (array (complex 'PatternTerm)))))
  (schema/sequence
   'Concatenation
   (rule/class
    'Concatenation
    '(Concatenation
      (Bound patterns PatternTerm)
      (Star (Bound patterns PatternTerm)))
    '((patterns (0)) (patterns (1 0))))
   (list (list 'patterns (array (complex 'PatternTerm)))))
  (schema/choice
   'PatternTerm
   (rule/class
    'PatternTerm
    '(Alternation
      (Bound bound BoundPatternTerm)
      (Bound unbound QuantifiedPatternTerm))
    '((bound (0)) (unbound (1))))
   (list
    (list 'unbound (scalar (complex 'QuantifiedPatternTerm)))
    (list 'bound (scalar (complex 'BoundPatternTerm)))))
  (schema/sequence
   'BoundPatternTerm
   (rule/class
    'BoundPatternTerm
    '(Concatenation
      (Bound name IDENTIFIER)
      TOKEN_3
      (Bound term QuantifiedPatternTerm))
    '((name (0)) (term (2))))
   (list
    (list 'name (scalar (basic 'string)))
    (list 'term (scalar (complex 'QuantifiedPatternTerm)))))
  (schema/choice
   'QuantifiedPatternTerm
   (rule/class
    'QuantifiedPatternTerm
    '(Alternation
      (Concatenation (Bound star UnquantifiedPatternTerm) TOKEN_5)
      (Concatenation (Bound plus UnquantifiedPatternTerm) TOKEN_6)
      (Concatenation (Bound question UnquantifiedPatternTerm) TOKEN_7)
      (Bound term UnquantifiedPatternTerm))
    '((star (0 0)) (plus (1 0)) (question (2 0)) (term (3))))
   (list
    (list 'star (scalar (complex 'UnquantifiedPatternTerm)))
    (list 'plus (scalar (complex 'UnquantifiedPatternTerm)))
    (list 'question (scalar (complex 'UnquantifiedPatternTerm)))
    (list 'term (scalar (complex 'UnquantifiedPatternTerm)))))
  (schema/choice
   'UnquantifiedPatternTerm
   (rule/class
    'UnquantifiedPatternTerm
    '(Alternation
      (Bound literal STRING)
      (Bound regex REGEX)
      (Bound rule IDENTIFIER)
      (Bound empty EMPTY)
      (Concatenation TOKEN_8 (Bound pattern Pattern) TOKEN_9))
    '((literal (0)) (regex (1)) (rule (2)) (empty (3)) (pattern (4 1))))
   (list
    (list 'empty (scalar (basic 'string)))
    (list 'literal (scalar (basic 'string)))
    (list 'pattern (scalar (complex 'Pattern)))
    (list 'rule (scalar (basic 'string)))
    (list 'regex (scalar (basic 'string))))))
 (list
  (token 'TOKEN_9 "\\)" (basic 'string) #f)
  (token 'TOKEN_8 "\\(" (basic 'string) #f)
  (token 'TOKEN_7 "\\?" (basic 'string) #f)
  (token 'TOKEN_6 "\\+" (basic 'string) #f)
  (token 'TOKEN_5 "\\*" (basic 'string) #f)
  (token 'TOKEN_4 "\\|" (basic 'string) #f)
  (token 'TOKEN_3 ":" (basic 'string) #f)
  (token 'TOKEN_2 "::=" (basic 'string) #f)
  (token 'TOKEN_1 "ignore" (basic 'string) #f)
  (token 'IDENTIFIER "[a-zA-Z_][0-9a-zA-Z_]*" (basic 'string) #f)
  (token 'STRING "\"([^\\\\\"]|\\\\.)*\"" (basic 'string) #f)
  (token 'REGEX "/([^\\\\/]|\\\\.)*/" (basic 'string) #f)
  (token 'EMPTY "\\(\\)" (basic 'string) #f)
  (token 'COMMENT "\\(\\*([^*]|\\*[^)])*\\*\\)" (basic 'string) #t)
  (token 'BLANK_LINE "\\s*\\n\\s*\\n\\s*" (basic 'string) #f)
  (token 'WS_LEFT "\\s+(?=\\S)" (basic 'string) #t)
  (token 'WS_END "\\s+$" (basic 'string) #t)))
