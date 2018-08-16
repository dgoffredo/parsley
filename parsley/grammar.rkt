#lang brag

Grammar  ::=  Rule (/BLANK_LINE Rule)*

Rule  ::=  "ignore"? IDENTIFIER /("::="|":") Pattern

Pattern  ::=  Alternation   (* one from multiple options *)
          |   Concatenation (* one or more subpatterns *)

Alternation  ::=  PatternTerm (/"|" PatternTerm)+

Concatenation  ::=  PatternTerm+

PatternTerm  ::=  BoundPatternTerm
              |   QuantifiedPatternTerm

BoundPatternTerm  ::=  IDENTIFIER /":" QuantifiedPatternTerm

QuantifiedPatternTerm  ::=  UnquantifiedPatternTerm "*"
                        |   UnquantifiedPatternTerm "+"
                        |   UnquantifiedPatternTerm "?"
                        |   UnquantifiedPatternTerm

UnquantifiedPatternTerm  ::=  STRING
                          |   REGEX
                          |   IDENTIFIER
                          |   EMPTY
                          |   /"(" Pattern /")"
                          |   Pattern
