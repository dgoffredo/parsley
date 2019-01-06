(productions
 (list
  (schema/choice
   'Expression
   (rule/class
    'Expression
    '(Alternation
      (Bound sumOrDiff SumOrDiff)
      (Bound prodOrQuot ProdOrQuot)
      (Bound atom Atom))
    '((sumOrDiff (0)) (prodOrQuot (1)) (atom (2))))
   (list
    (list 'sumOrDiff (scalar (complex 'SumOrDiff)))
    (list 'atom (scalar (complex 'Atom)))
    (list 'prodOrQuot (scalar (complex 'ProdOrQuot)))))
  (schema/choice
   'Atom
   (rule/class
    'Atom
    '(Alternation
      (Bound identifier IDENTIFIER)
      (Bound number NUMBER)
      (Concatenation TOKEN_1 (Bound expression Expression) TOKEN_2))
    '((identifier (0)) (number (1)) (expression (2 1))))
   (list
    (list 'expression (scalar (complex 'Expression)))
    (list 'identifier (scalar (basic 'string)))
    (list 'number (scalar (basic 'decimal)))))
  (schema/sequence
   'SumOrDiff
   (rule/class
    'SumOrDiff
    '(Concatenation
      (Bound left SumOrDiffTerm)
      (Concatenation
       (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))
       (Star
        (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm)))))
    '((left (0))
      (op (1 0 0))
      (right (1 0 1))
      (op (1 1 0 0))
      (right (1 1 0 1))))
   (list
    (list 'left (scalar (complex 'SumOrDiffTerm)))
    (list 'op (array (complex 'PlusOrMinus)))
    (list 'right (array (complex 'SumOrDiffTerm)))))
  (schema/choice
   'SumOrDiffTerm
   (rule/class
    'SumOrDiffTerm
    '(Alternation (Bound prodOrQuot ProdOrQuot) (Bound atom Atom))
    '((prodOrQuot (0)) (atom (1))))
   (list
    (list 'atom (scalar (complex 'Atom)))
    (list 'prodOrQuot (scalar (complex 'ProdOrQuot)))))
  (schema/sequence
   'ProdOrQuot
   (rule/class
    'ProdOrQuot
    '(Concatenation
      (Bound left Atom)
      (Concatenation
       (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))
       (Star (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom)))))
    '((left (0))
      (op (1 0 0))
      (right (1 0 1))
      (op (1 1 0 0))
      (right (1 1 0 1))))
   (list
    (list 'left (scalar (complex 'Atom)))
    (list 'op (array (complex 'TimesOrDividedBy)))
    (list 'right (array (complex 'Atom)))))
  (schema/enumeration
   'PlusOrMinus
   (rule/enumeration 'PlusOrMinus '(Alternation TOKEN_3 TOKEN_4) '("+" "-"))
   '("+" "-"))
  (schema/enumeration
   'TimesOrDividedBy
   (rule/enumeration
    'TimesOrDividedBy
    '(Alternation TOKEN_5 TOKEN_6)
    '("*" "/"))
   '("*" "/")))
 (list
  (token 'TOKEN_6 "/" (basic 'string) #f)
  (token 'TOKEN_5 "\\*" (basic 'string) #f)
  (token 'TOKEN_4 "\\-" (basic 'string) #f)
  (token 'TOKEN_3 "\\+" (basic 'string) #f)
  (token 'TOKEN_2 "\\)" (basic 'string) #f)
  (token 'TOKEN_1 "\\(" (basic 'string) #f)
  (token 'NUMBER "(0|[1-9][0-9]*)(\\.[0-9]+)?" (basic 'decimal) #f)
  (token
   'IDENTIFIER
   "[^0-9][a-zA-Z0-9_\\-!@#$%^&*+=:/?~]*"
   (basic 'string)
   #f)))
