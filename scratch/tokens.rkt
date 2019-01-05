(list
 (token-struct 'IDENTIFIER 'Grammar #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'rules #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Rule #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 "("
 (token-struct 'IDENTIFIER 'BLANK_LINE #f #f #f #f #f)
 "+"
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'rules #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Rule #f #f #f #f #f)
 ")"
 "*"
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'BLANK_LINE #f #f #f #f #f)
 "*"
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'Rule #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'ignore #f #f #f #f #f)
 ":"
 (token-struct 'STRING "ignore" #f #f #f #f #f)
 "?"
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'name #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'IDENTIFIER #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 "("
 (token-struct 'STRING "::=" #f #f #f #f #f)
 "|"
 (token-struct 'STRING ":" #f #f #f #f #f)
 ")"
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'pattern #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Pattern #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'Pattern #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'alternation #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Alternation #f #f #f #f #f)
 (token-struct 'WS_LEFT "      " #f #f #f #f #t)
 (token-struct 'COMMENT " one from multiple options\r" #f #f #f #f #t)
 (token-struct 'WS_LEFT "\n          " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'concatenation #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Concatenation #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'COMMENT " one or more subpatterns\r" #f #f #f #f #t)
 (token-struct 'BLANK_LINE "\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'Alternation #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'patterns #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'PatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 "("
 (token-struct 'STRING "|" #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'patterns #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'PatternTerm #f #f #f #f #f)
 ")"
 "+"
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'Concatenation #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'patterns #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'PatternTerm #f #f #f #f #f)
 "+"
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'PatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'bound #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'BoundPatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n              " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'unbound #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'QuantifiedPatternTerm #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'BoundPatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'name #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'IDENTIFIER #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'STRING ":" #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'term #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'QuantifiedPatternTerm #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'QuantifiedPatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'star #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'UnquantifiedPatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'STRING "*" #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n                        " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'plus #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'UnquantifiedPatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'STRING "+" #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n                        " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'question #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'UnquantifiedPatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'STRING "?" #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n                        " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'term #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'UnquantifiedPatternTerm #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'UnquantifiedPatternTerm #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'literal #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'STRING #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n                          " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'regex #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'REGEX #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n                          " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'rule #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'IDENTIFIER #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n                          " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'empty #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'EMPTY #f #f #f #f #f)
 (token-struct 'WS_LEFT "\r\n                          " #f #f #f #f #t)
 "|"
 (token-struct 'WS_LEFT "   " #f #f #f #f #t)
 (token-struct 'STRING "(" #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'pattern #f #f #f #f #f)
 ":"
 (token-struct 'IDENTIFIER 'Pattern #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'STRING ")" #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'IDENTIFIER #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "[a-zA-Z_][0-9a-zA-Z_]*") #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'STRING #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "\"([^\\\\\"]|\\\\.)*\"") #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'REGEX #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "/([^\\\\/]|\\\\.)*/") #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'EMPTY #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "\\(\\)") #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'ignore #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'COMMENT #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "\\(\\*([^*]|\\*[^)])*\\*\\)") #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'BLANK_LINE #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "\\s*\\n\\s*\\n\\s*") #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'ignore #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'WS_LEFT #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "\\s+(?=\\S)") #f #f #f #f #f)
 (token-struct 'BLANK_LINE "\r\n\r\n" #f #f #f #f #f)
 (token-struct 'IDENTIFIER 'ignore #f #f #f #f #f)
 (token-struct 'WS_LEFT " " #f #f #f #f #t)
 (token-struct 'IDENTIFIER 'WS_END #f #f #f #f #f)
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 "::="
 (token-struct 'WS_LEFT "  " #f #f #f #f #t)
 (token-struct 'REGEX '(regex "\\s+$") #f #f #f #f #f))
