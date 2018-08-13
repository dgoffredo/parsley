Example grammar:

    IDENTIFIER  ::=  /[a-zA-Z_][0-9a-zA-Z_]*/

    INTEGER  ::=  /0|[1-9][0-9]*/

    STRING  ::=  /"([^\\]|\\.)*"/

    SEPARATOR  ::=  /\s*,\s*/

    Path  ::=  terms:(Term ("/" Term)*)
    
    Term  ::=  identifier:IDENTIFIER
           |   wildcard:"*"
           |   function:Function
           |   selection:Selection
    
    Function  ::=  
        name:IDENTIFIER 
        args:("(" (Argument (SEPARATOR Argument)*)? ")")
    
    Argument  ::= identifier:IDENTIFIER
               |  stringLiteral:STRING
               |  integer:INTEGER

Terminals:

    IDENTIFIER  ::=  /[a-zA-Z_][0-9a-zA-Z_]*/

    INTEGER  ::=  /0|[1-9][0-9]*/

    STRING  ::=  /"([^\\]|\\.)*"/

    SEPARATOR  ::=  /\s*,\s*/

    ignore COMMENT  ::=  /\/\*([^*]|\*[^\/])*\*\//

    "*"

    "("

    ")"

    "/"

Those will form the set of tokens. Give the anonymous ones names:

    IDENTIFIER  ::=  /[a-zA-Z_][0-9a-zA-Z_]*/

    INTEGER  ::=  /0|[1-9][0-9]*/

    STRING  ::=  /"([^\\]|\\.)*"/

    SEPARATOR  ::=  /\s*,\s*/

    ignore COMMENT  ::=  /\/\*([^*]|\*[^\/])*\*\//

    STAR  ::=  "*"

    LPAREN  ::=  "("

    RPAREN  ::=  ")"

    FSLASH  ::=  "/"

Then the grammar is:

    Path  ::=  terms:(Term (FSLASH Term)*)
    
    Term  ::=  identifier:IDENTIFIER
           |   wildcard:STAR
           |   function:Function
           |   selection:Selection
    
    Function  ::=  
        name:IDENTIFIER 
        args:(LPAREN (Argument (SEPARATOR Argument)*)? RPAREN)
    
    Argument  ::= identifier:IDENTIFIER
               |  stringLiteral:STRING
               |  integer:INTEGER