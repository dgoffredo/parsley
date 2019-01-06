(list
 (reader-function
  "read"
  (scalar (complex "Expression"))
  (reader/alternation
   '(Alternation
     (Bound sumOrDiff SumOrDiff)
     (Bound prodOrQuot ProdOrQuot)
     (Bound atom Atom))
   (list
    (reader/term
     '(Bound sumOrDiff SumOrDiff)
     (member-accessor "makeSumOrDiff")
     "read")
    (reader/term
     '(Bound prodOrQuot ProdOrQuot)
     (member-accessor "makeProdOrQuot")
     "read")
    (reader/term '(Bound atom Atom) (member-accessor "makeAtom") "read"))))
 (reader-function
  "readAtom_1"
  (scalar (complex "Atom"))
  (reader/concatenation
   '(Concatenation TOKEN_1 (Bound expression Expression) TOKEN_2)
   (list
    (reader/term "\\(" #f (reader/token "\\(" 'TOKEN_1))
    (reader/term
     '(Bound expression Expression)
     (member-accessor "makeExpression")
     "read")
    (reader/term "\\)" #f (reader/token "\\)" 'TOKEN_2)))))
 (reader-function
  "read"
  (scalar (complex "Atom"))
  (reader/alternation
   '(Alternation
     (Bound identifier IDENTIFIER)
     (Bound number NUMBER)
     (Concatenation TOKEN_1 (Bound expression Expression) TOKEN_2))
   (list
    (reader/term
     '(Bound identifier IDENTIFIER)
     (member-accessor "makeIdentifier")
     (reader/token "[^0-9][a-zA-Z0-9_\\-!@#$%^&*+=:/?~]*" 'IDENTIFIER))
    (reader/term
     '(Bound number NUMBER)
     (member-accessor "makeNumber")
     (reader/token "(0|[1-9][0-9]*)(\\.[0-9]+)?" 'NUMBER))
    (reader/term
     '(Concatenation TOKEN_1 (Bound expression Expression) TOKEN_2)
     'forward
     "readAtom_1"))))
 (reader-function
  "readSumOrDiff_1"
  (scalar (complex "SumOrDiff"))
  (reader/concatenation
   '(Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))
   (list
    (reader/term '(Bound op PlusOrMinus) (member-accessor "op") "read")
    (reader/term
     '(Bound right SumOrDiffTerm)
     (member-accessor "right")
     "read"))))
 (reader-function
  "readSumOrDiff_2"
  (scalar (complex "SumOrDiff"))
  (reader/concatenation
   '(Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))
   (list
    (reader/term '(Bound op PlusOrMinus) (member-accessor "op") "read")
    (reader/term
     '(Bound right SumOrDiffTerm)
     (member-accessor "right")
     "read"))))
 (reader-function
  "readSumOrDiff_3"
  (scalar (complex "SumOrDiff"))
  (reader/concatenation
   '(Concatenation
     (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))
     (Star (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))))
   (list
    (reader/term
     '(Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))
     'forward
     "readSumOrDiff_1")
    (reader/star
     '(Star (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm)))
     (reader/term
      '(Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))
      'forward
      "readSumOrDiff_2")))))
 (reader-function
  "read"
  (scalar (complex "SumOrDiff"))
  (reader/concatenation
   '(Concatenation
     (Bound left SumOrDiffTerm)
     (Concatenation
      (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))
      (Star
       (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm)))))
   (list
    (reader/term '(Bound left SumOrDiffTerm) (member-accessor "left") "read")
    (reader/term
     '(Concatenation
       (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))
       (Star
        (Concatenation (Bound op PlusOrMinus) (Bound right SumOrDiffTerm))))
     'forward
     "readSumOrDiff_3"))))
 (reader-function
  "read"
  (scalar (complex "SumOrDiffTerm"))
  (reader/alternation
   '(Alternation (Bound prodOrQuot ProdOrQuot) (Bound atom Atom))
   (list
    (reader/term
     '(Bound prodOrQuot ProdOrQuot)
     (member-accessor "makeProdOrQuot")
     "read")
    (reader/term '(Bound atom Atom) (member-accessor "makeAtom") "read"))))
 (reader-function
  "readProdOrQuot_1"
  (scalar (complex "ProdOrQuot"))
  (reader/concatenation
   '(Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))
   (list
    (reader/term '(Bound op TimesOrDividedBy) (member-accessor "op") "read")
    (reader/term '(Bound right Atom) (member-accessor "right") "read"))))
 (reader-function
  "readProdOrQuot_2"
  (scalar (complex "ProdOrQuot"))
  (reader/concatenation
   '(Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))
   (list
    (reader/term '(Bound op TimesOrDividedBy) (member-accessor "op") "read")
    (reader/term '(Bound right Atom) (member-accessor "right") "read"))))
 (reader-function
  "readProdOrQuot_3"
  (scalar (complex "ProdOrQuot"))
  (reader/concatenation
   '(Concatenation
     (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))
     (Star (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))))
   (list
    (reader/term
     '(Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))
     'forward
     "readProdOrQuot_1")
    (reader/star
     '(Star (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom)))
     (reader/term
      '(Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))
      'forward
      "readProdOrQuot_2")))))
 (reader-function
  "read"
  (scalar (complex "ProdOrQuot"))
  (reader/concatenation
   '(Concatenation
     (Bound left Atom)
     (Concatenation
      (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))
      (Star (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom)))))
   (list
    (reader/term '(Bound left Atom) (member-accessor "left") "read")
    (reader/term
     '(Concatenation
       (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))
       (Star (Concatenation (Bound op TimesOrDividedBy) (Bound right Atom))))
     'forward
     "readProdOrQuot_3"))))
 (reader-function
  "readPlusOrMinusValue"
  (scalar (basic 'string))
  (reader/alternation
   '(Alternation TOKEN_3 TOKEN_4)
   (list
    (reader/term "\\+" 'forward (reader/token "\\+" 'TOKEN_3))
    (reader/term "\\-" 'forward (reader/token "\\-" 'TOKEN_4)))))
 (reader-function
  "read"
  (scalar (complex "PlusOrMinus::Value"))
  (reader/enumeration
   '(Alternation TOKEN_3 TOKEN_4)
   "PlusOrMinus"
   "readPlusOrMinusValue"))
 (reader-function
  "readTimesOrDividedByValue"
  (scalar (basic 'string))
  (reader/alternation
   '(Alternation TOKEN_5 TOKEN_6)
   (list
    (reader/term "\\*" 'forward (reader/token "\\*" 'TOKEN_5))
    (reader/term "/" 'forward (reader/token "/" 'TOKEN_6)))))
 (reader-function
  "read"
  (scalar (complex "TimesOrDividedBy::Value"))
  (reader/enumeration
   '(Alternation TOKEN_5 TOKEN_6)
   "TimesOrDividedBy"
   "readTimesOrDividedByValue")))
