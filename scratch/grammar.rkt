'(Grammar
  (Rule
   Expression
   (Alternation
    (Bound sumOrDiff SumOrDiff)
    (Bound prodOrQuot ProdOrQuot)
    (Bound atom Atom)))
  (Rule
   Atom
   (Alternation
    (Bound identifier IDENTIFIER)
    (Bound number NUMBER)
    (Concatenation "(" (Bound expression Expression) ")")))
  (Rule
   SumOrDiff
   (Concatenation
    (Bound left SumOrDiffTerm)
    (Plus (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm)))))
  (Rule
   SumOrDiffTerm
   (Alternation (Bound prodOrQuot ProdOrQuot) (Bound atom Atom)))
  (Rule
   ProdOrQuot
   (Concatenation
    (Bound left Atom)
    (Plus (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom)))))
  (Rule enumeration PlusOrMinus (Alternation "+" "-"))
  (Rule enumeration TimesOrDividedBy (Alternation "*" "/"))
  (Rule decimal NUMBER (regex "(0|[1-9][0-9]*)(\\.[0-9]+)?"))
  (Rule IDENTIFIER (regex "[^0-9][a-zA-Z0-9_\\-!@#$%^&*+=:/?~]*")))
