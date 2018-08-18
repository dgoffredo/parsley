#lang brag

Grammar  ::=  Rule (/BLANK_LINE+ Rule)* /BLANK_LINE*

Rule  ::=  IDENTIFIER? IDENTIFIER /("::="|":") @Pattern

Pattern  ::=  Alternation
          |   Concatenation
          |   @PatternTerm

Alternation  ::=  @PatternTerm (/"|" @PatternTerm)+

Concatenation  ::=  @PatternTerm+

PatternTerm  ::=  @BasicTerm
              |   /"(" @Pattern /")"
              |   Bound
              |   Star
              |   Plus
              |   Question
              |   Concatenation

BasicTerm  ::=  STRING
            |   REGEX
            |   IDENTIFIER
            |   EMPTY

(* TODO: Maybe the requirement "bindings are to basic things only" ought to be
         expressed outside of the grammar rather than within it. Practically,
         I could make the error diagnostics better after the parse.
         Idealogically, the restriction is unnatural. *)

Bound  ::=  IDENTIFIER /":" @BasicTerm

Star  ::=  @PatternTerm /"*"

Plus  ::=  @PatternTerm /"+"

Question  ::=  @PatternTerm /"?"