(list
 (reader-function
  "readGrammar_1"
  #f
  (reader/concatenation
   '(Concatenation BLANK_LINE (Star BLANK_LINE))
   (list
    (reader/term
     "\\s*\\n\\s*\\n\\s*"
     #f
     (reader/token "\\s*\\n\\s*\\n\\s*" 'BLANK_LINE))
    (reader/star
     '(Star BLANK_LINE)
     (reader/term
      "\\s*\\n\\s*\\n\\s*"
      #f
      (reader/token "\\s*\\n\\s*\\n\\s*" 'BLANK_LINE))))))
 (reader-function
  "readGrammar_2"
  (scalar (complex "Grammar"))
  (reader/concatenation
   '(Concatenation
     (Concatenation BLANK_LINE (Star BLANK_LINE))
     (Bound rules Rule))
   (list
    (reader/term
     '(Concatenation BLANK_LINE (Star BLANK_LINE))
     #f
     "readGrammar_1")
    (reader/term '(Bound rules Rule) (member-accessor "rules") "read"))))
 (reader-function
  "read"
  (scalar (complex "Grammar"))
  (reader/concatenation
   '(Concatenation
     (Bound rules Rule)
     (Star
      (Concatenation
       (Concatenation BLANK_LINE (Star BLANK_LINE))
       (Bound rules Rule)))
     (Star BLANK_LINE))
   (list
    (reader/term '(Bound rules Rule) (member-accessor "rules") "read")
    (reader/star
     '(Star
       (Concatenation
        (Concatenation BLANK_LINE (Star BLANK_LINE))
        (Bound rules Rule)))
     (reader/term
      '(Concatenation
        (Concatenation BLANK_LINE (Star BLANK_LINE))
        (Bound rules Rule))
      'forward
      "readGrammar_2"))
    (reader/star
     '(Star BLANK_LINE)
     (reader/term
      "\\s*\\n\\s*\\n\\s*"
      #f
      (reader/token "\\s*\\n\\s*\\n\\s*" 'BLANK_LINE))))))
 (reader-function
  "readRule_1"
  #f
  (reader/alternation
   '(Alternation TOKEN_2 TOKEN_3)
   (list
    (reader/term "::=" #f (reader/token "::=" 'TOKEN_2))
    (reader/term ":" #f (reader/token ":" 'TOKEN_3)))))
 (reader-function
  "read"
  (scalar (complex "Rule"))
  (reader/concatenation
   '(Concatenation
     (Question (Bound ignore TOKEN_1))
     (Bound name IDENTIFIER)
     (Alternation TOKEN_2 TOKEN_3)
     (Bound pattern Pattern))
   (list
    (reader/question
     '(Question (Bound ignore TOKEN_1))
     (reader/term
      '(Bound ignore TOKEN_1)
      (member-accessor "ignore")
      (reader/token "ignore" 'TOKEN_1)))
    (reader/term
     '(Bound name IDENTIFIER)
     (member-accessor "name")
     (reader/token "[a-zA-Z_][0-9a-zA-Z_]*" 'IDENTIFIER))
    (reader/term '(Alternation TOKEN_2 TOKEN_3) #f "readRule_1")
    (reader/term
     '(Bound pattern Pattern)
     (member-accessor "pattern")
     "read"))))
 (reader-function
  "read"
  (scalar (complex "Pattern"))
  (reader/alternation
   '(Alternation
     (Bound alternation Alternation)
     (Bound concatenation Concatenation))
   (list
    (reader/term
     '(Bound alternation Alternation)
     (member-accessor "makeAlternation")
     "read")
    (reader/term
     '(Bound concatenation Concatenation)
     (member-accessor "makeConcatenation")
     "read"))))
 (reader-function
  "readAlternation_1"
  (scalar (complex "Alternation"))
  (reader/concatenation
   '(Concatenation TOKEN_4 (Bound patterns PatternTerm))
   (list
    (reader/term "\\|" #f (reader/token "\\|" 'TOKEN_4))
    (reader/term
     '(Bound patterns PatternTerm)
     (member-accessor "patterns")
     "read"))))
 (reader-function
  "readAlternation_2"
  (scalar (complex "Alternation"))
  (reader/concatenation
   '(Concatenation TOKEN_4 (Bound patterns PatternTerm))
   (list
    (reader/term "\\|" #f (reader/token "\\|" 'TOKEN_4))
    (reader/term
     '(Bound patterns PatternTerm)
     (member-accessor "patterns")
     "read"))))
 (reader-function
  "readAlternation_3"
  (scalar (complex "Alternation"))
  (reader/concatenation
   '(Concatenation
     (Concatenation TOKEN_4 (Bound patterns PatternTerm))
     (Star (Concatenation TOKEN_4 (Bound patterns PatternTerm))))
   (list
    (reader/term
     '(Concatenation TOKEN_4 (Bound patterns PatternTerm))
     'forward
     "readAlternation_1")
    (reader/star
     '(Star (Concatenation TOKEN_4 (Bound patterns PatternTerm)))
     (reader/term
      '(Concatenation TOKEN_4 (Bound patterns PatternTerm))
      'forward
      "readAlternation_2")))))
 (reader-function
  "read"
  (scalar (complex "Alternation"))
  (reader/concatenation
   '(Concatenation
     (Bound patterns PatternTerm)
     (Concatenation
      (Concatenation TOKEN_4 (Bound patterns PatternTerm))
      (Star (Concatenation TOKEN_4 (Bound patterns PatternTerm)))))
   (list
    (reader/term
     '(Bound patterns PatternTerm)
     (member-accessor "patterns")
     "read")
    (reader/term
     '(Concatenation
       (Concatenation TOKEN_4 (Bound patterns PatternTerm))
       (Star (Concatenation TOKEN_4 (Bound patterns PatternTerm))))
     'forward
     "readAlternation_3"))))
 (reader-function
  "read"
  (scalar (complex "Concatenation"))
  (reader/concatenation
   '(Concatenation
     (Bound patterns PatternTerm)
     (Star (Bound patterns PatternTerm)))
   (list
    (reader/term
     '(Bound patterns PatternTerm)
     (member-accessor "patterns")
     "read")
    (reader/star
     '(Star (Bound patterns PatternTerm))
     (reader/term
      '(Bound patterns PatternTerm)
      (member-accessor "patterns")
      "read")))))
 (reader-function
  "read"
  (scalar (complex "PatternTerm"))
  (reader/alternation
   '(Alternation
     (Bound bound BoundPatternTerm)
     (Bound unbound QuantifiedPatternTerm))
   (list
    (reader/term
     '(Bound bound BoundPatternTerm)
     (member-accessor "makeBound")
     "read")
    (reader/term
     '(Bound unbound QuantifiedPatternTerm)
     (member-accessor "makeUnbound")
     "read"))))
 (reader-function
  "read"
  (scalar (complex "BoundPatternTerm"))
  (reader/concatenation
   '(Concatenation
     (Bound name IDENTIFIER)
     TOKEN_3
     (Bound term QuantifiedPatternTerm))
   (list
    (reader/term
     '(Bound name IDENTIFIER)
     (member-accessor "name")
     (reader/token "[a-zA-Z_][0-9a-zA-Z_]*" 'IDENTIFIER))
    (reader/term ":" #f (reader/token ":" 'TOKEN_3))
    (reader/term
     '(Bound term QuantifiedPatternTerm)
     (member-accessor "term")
     "read"))))
 (reader-function
  "readQuantifiedPatternTerm_1"
  (scalar (complex "QuantifiedPatternTerm"))
  (reader/concatenation
   '(Concatenation (Bound star UnquantifiedPatternTerm) TOKEN_5)
   (list
    (reader/term
     '(Bound star UnquantifiedPatternTerm)
     (member-accessor "makeStar")
     "read")
    (reader/term "\\*" #f (reader/token "\\*" 'TOKEN_5)))))
 (reader-function
  "readQuantifiedPatternTerm_2"
  (scalar (complex "QuantifiedPatternTerm"))
  (reader/concatenation
   '(Concatenation (Bound plus UnquantifiedPatternTerm) TOKEN_6)
   (list
    (reader/term
     '(Bound plus UnquantifiedPatternTerm)
     (member-accessor "makePlus")
     "read")
    (reader/term "\\+" #f (reader/token "\\+" 'TOKEN_6)))))
 (reader-function
  "readQuantifiedPatternTerm_3"
  (scalar (complex "QuantifiedPatternTerm"))
  (reader/concatenation
   '(Concatenation (Bound question UnquantifiedPatternTerm) TOKEN_7)
   (list
    (reader/term
     '(Bound question UnquantifiedPatternTerm)
     (member-accessor "makeQuestion")
     "read")
    (reader/term "\\?" #f (reader/token "\\?" 'TOKEN_7)))))
 (reader-function
  "read"
  (scalar (complex "QuantifiedPatternTerm"))
  (reader/alternation
   '(Alternation
     (Concatenation (Bound star UnquantifiedPatternTerm) TOKEN_5)
     (Concatenation (Bound plus UnquantifiedPatternTerm) TOKEN_6)
     (Concatenation (Bound question UnquantifiedPatternTerm) TOKEN_7)
     (Bound term UnquantifiedPatternTerm))
   (list
    (reader/term
     '(Concatenation (Bound star UnquantifiedPatternTerm) TOKEN_5)
     'forward
     "readQuantifiedPatternTerm_1")
    (reader/term
     '(Concatenation (Bound plus UnquantifiedPatternTerm) TOKEN_6)
     'forward
     "readQuantifiedPatternTerm_2")
    (reader/term
     '(Concatenation (Bound question UnquantifiedPatternTerm) TOKEN_7)
     'forward
     "readQuantifiedPatternTerm_3")
    (reader/term
     '(Bound term UnquantifiedPatternTerm)
     (member-accessor "makeTerm")
     "read"))))
 (reader-function
  "readUnquantifiedPatternTerm_1"
  (scalar (complex "UnquantifiedPatternTerm"))
  (reader/concatenation
   '(Concatenation TOKEN_8 (Bound pattern Pattern) TOKEN_9)
   (list
    (reader/term "\\(" #f (reader/token "\\(" 'TOKEN_8))
    (reader/term
     '(Bound pattern Pattern)
     (member-accessor "makePattern")
     "read")
    (reader/term "\\)" #f (reader/token "\\)" 'TOKEN_9)))))
 (reader-function
  "read"
  (scalar (complex "UnquantifiedPatternTerm"))
  (reader/alternation
   '(Alternation
     (Bound literal STRING)
     (Bound regex REGEX)
     (Bound rule IDENTIFIER)
     (Bound empty EMPTY)
     (Concatenation TOKEN_8 (Bound pattern Pattern) TOKEN_9))
   (list
    (reader/term
     '(Bound literal STRING)
     (member-accessor "makeLiteral")
     (reader/token "\"([^\\\\\"]|\\\\.)*\"" 'STRING))
    (reader/term
     '(Bound regex REGEX)
     (member-accessor "makeRegex")
     (reader/token "/([^\\\\/]|\\\\.)*/" 'REGEX))
    (reader/term
     '(Bound rule IDENTIFIER)
     (member-accessor "makeRule")
     (reader/token "[a-zA-Z_][0-9a-zA-Z_]*" 'IDENTIFIER))
    (reader/term
     '(Bound empty EMPTY)
     (member-accessor "makeEmpty")
     (reader/token "\\(\\)" 'EMPTY))
    (reader/term
     '(Concatenation TOKEN_8 (Bound pattern Pattern) TOKEN_9)
     'forward
     "readUnquantifiedPatternTerm_1")))))
