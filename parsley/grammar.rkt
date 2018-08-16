#lang brag

Grammar  ::=  Rule (/BLANK_LINE+ Rule)* /BLANK_LINE*

Rule  ::=  IDENTIFIER? IDENTIFIER /("::="|":") @Pattern

Pattern  ::=  Alternation
          |   Concatenation
          |   @PatternTerm

Alternation  ::=  @PatternTerm (/"|" @PatternTerm)+

Concatenation  ::=  @PatternTerm+

PatternTerm  ::=  STRING
              |   REGEX
              |   IDENTIFIER
              |   EMPTY
              |   /"(" @Pattern /")"
              |   Bound
              |   Star
              |   Plus
              |   Question

Bound  ::=  IDENTIFIER /":" @PatternTerm

Star  ::=  @PatternTerm /"*"

Plus  ::=  @PatternTerm /"+"

Question  ::=  @PatternTerm /"?"