(list
 (token-struct 'COMMENT " e.g. 3*(2 + 4 / 2 + 6) - 1\r" #f #f #f #f #t)
 (token-struct 'WS_LEFT "\n" #f #f #f #f #t)
 (token-struct 'COMMENT " ... which is 29\r" #f #f #f #f #t)
 (token-struct 'WS_LEFT "\n" #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'Expression #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'sumOrDiff #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'SumOrDiff #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n             " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'prodOrQuot #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'ProdOrQuot #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n             " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'atom #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Atom #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'Atom #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'identifier #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'IDENTIFIER #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n       " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'number #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'NUMBER #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n       " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'STRING "(" #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'expression #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Expression #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'STRING ")" #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'SumOrDiff #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'left #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'SumOrDiffTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 "("
 (token-struct 'IDENTIFIER 'op #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'PlusOrMinus #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'right #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'SumOrDiffTerm #f #f #f #f #f)
 ")"
 "+"
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'SumOrDiffTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'prodOrQuot #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'ProdOrQuot #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n                " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'atom #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Atom #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'ProdOrQuot #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'left #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Atom #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 "("
 (token-struct 'IDENTIFIER 'op #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'TimesOrDividedBy #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'right #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Atom #f #f #f #f #f)
 ")"
 "+"
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'enumeration #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'PlusOrMinus #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'STRING "+" #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'STRING "-" #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'enumeration #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'TimesOrDividedBy #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'STRING "*" #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'STRING "/" #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'decimal #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'NUMBER #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "(0|[1-9][0-9]*)(\\.[0-9]+)?") #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'IDENTIFIER #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct
  'REGEX
  '(regex "[^0-9][a-zA-Z0-9_\\-!@#$%^&*+=:/?~]*")
  #f
  #f
  #f
  #f
  #f))
