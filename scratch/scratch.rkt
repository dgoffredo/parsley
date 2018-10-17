'((reader-function
     "read"
     ((scalar occurrence 1) ((complex kind 1) "Expression"))
     ((reader/alternation reader/compound 1)
        ((reader/term (member-accessor "makeSumOrDiff") "read")
         (reader/term (member-accessor "makeProdOrQuot") "read")
         (reader/term (member-accessor "makeAtom") "read"))))
  (reader-function
     "readAtom_1"
     ((scalar occurrence 1) ((complex kind 1) "Atom"))
     ((reader/concatenation reader/compound 1)
        ((reader/term #f (reader/token TOKEN_1))
         (reader/term (member-accessor "makeExpression") "read")
         (reader/term #f (reader/token TOKEN_2)))))
  (reader-function
     "read"
     ((scalar occurrence 1) ((complex kind 1) "Atom"))
     ((reader/alternation reader/compound 1)
        ((reader/term
            (member-accessor "makeIdentifier")
            (reader/token IDENTIFIER))
         (reader/term
            (member-accessor "makeNumber")
            (reader/token NUMBER))
         (reader/term forward "readAtom_1"))))
  (reader-function
     "readSumOrDiff_1"
     ((scalar occurrence 1) ((complex kind 1) "SumOrDiff"))
     ((reader/concatenation reader/compound 1)
        ((reader/term (member-accessor "op") "read")
         (reader/term (member-accessor "right") "read"))))
  (reader-function
     "readSumOrDiff_2"
     ((scalar occurrence 1) ((complex kind 1) "SumOrDiff"))
     ((reader/concatenation reader/compound 1)
        ((reader/term (member-accessor "op") "read")
         (reader/term (member-accessor "right") "read"))))
  (reader-function
     "readSumOrDiff_3"
     ((scalar occurrence 1) ((complex kind 1) "SumOrDiff"))
     ((reader/concatenation reader/compound 1)
        ((reader/term forward "readSumOrDiff_1")
         (reader/star (reader/term forward "readSumOrDiff_2")))))
  (reader-function
     "read"
     ((scalar occurrence 1) ((complex kind 1) "SumOrDiff"))
     ((reader/concatenation reader/compound 1)
        ((reader/term (member-accessor "left") "read")
         (reader/term forward "readSumOrDiff_3"))))
  (reader-function
     "read"
     ((scalar occurrence 1) ((complex kind 1) "SumOrDiffTerm"))
     ((reader/alternation reader/compound 1)
        ((reader/term (member-accessor "makeProdOrQuot") "read")
         (reader/term (member-accessor "makeAtom") "read"))))
  (reader-function
     "readProdOrQuot_1"
     ((scalar occurrence 1) ((complex kind 1) "ProdOrQuot"))
     ((reader/concatenation reader/compound 1)
        ((reader/term (member-accessor "op") "read")
         (reader/term (member-accessor "right") "read"))))
  (reader-function
     "readProdOrQuot_2"
     ((scalar occurrence 1) ((complex kind 1) "ProdOrQuot"))
     ((reader/concatenation reader/compound 1)
        ((reader/term (member-accessor "op") "read")
         (reader/term (member-accessor "right") "read"))))
  (reader-function
     "readProdOrQuot_3"
     ((scalar occurrence 1) ((complex kind 1) "ProdOrQuot"))
     ((reader/concatenation reader/compound 1)
        ((reader/term forward "readProdOrQuot_1")
         (reader/star (reader/term forward "readProdOrQuot_2")))))
  (reader-function
     "read"
     ((scalar occurrence 1) ((complex kind 1) "ProdOrQuot"))
     ((reader/concatenation reader/compound 1)
        ((reader/term (member-accessor "left") "read")
         (reader/term forward "readProdOrQuot_3"))))
  (reader-function
     "readPlusOrMinusValue"
     ((scalar occurrence 1) ((basic kind 1) string))
     ((reader/alternation reader/compound 1)
        ((reader/term forward (reader/token TOKEN_3))
         (reader/term forward (reader/token TOKEN_4)))))
  (reader-function
     "read"
     ((scalar occurrence 1) ((complex kind 1) "PlusOrMinus::Value"))
     (reader/enumeration "PlusOrMinus" "readPlusOrMinusValue"))
  (reader-function
     "readTimesOrDividedByValue"
     ((scalar occurrence 1) ((basic kind 1) string))
     ((reader/alternation reader/compound 1)
        ((reader/term forward (reader/token TOKEN_5))
         (reader/term forward (reader/token TOKEN_6)))))
  (reader-function
     "read"
     ((scalar occurrence 1) ((complex kind 1) "TimesOrDividedBy::Value"))
     (reader/enumeration "TimesOrDividedBy" "readTimesOrDividedByValue")))