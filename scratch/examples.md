They look like this

    foo/bar

    foo/bar/last(baz)/value[5]

    foo/datum(some_name)

    foo/bar[5]

Grammar might be:

    path  ::=  term ("/" term)*

    term  ::=  indentifier
           |   wildcard
           |   function
           |   selection

    identifier  ::=  C_IDENTIFIER

    wildcard  ::=  "*"

    function  ::=  identifier "(" (argument ("," argument)*)? ")"

    argument  ::= identifier
               |  string
               |  integer

    string  ::=  C_STRING_LITERAL

    integer  ::=  C_INTEGER

    selection  ::=  identifier "[" integer "]"

If I bdlat-ifiy it, I'll need to annotate the types and which bits go into
which attributes.

    Path  ::=  terms:(Term ("/" Term)*)

    Term  ::=  identifier:Identifier
           |   wildcard:Wildcard
           |   function:Function
           |   selection:Selection

    Identifier  ::=  C_IDENTIFIER

    Wildcard  ::=  "*"

    Function  ::=  name:Identifier args:("(" (Argument ("," Argument)*)? ")")

    Argument  ::= identifier:Identifier
               |  stringLiteral:C_STRING_LITERAL
               |  integer:C_INTEGER

    Selection  ::=  array:Identifier "[" index:Integer "]"

So one of the examples from above:

    foo/bar/avg(baz)

would yield:

    Path {
        terms: [
            Term {identifier: Identifier {value:"foo"}},
            Term {identifier: Identifier {value:"bar"}},
            Term {function: Function {
                                name: Identifier {value: "avg"},
                                args: [
                                    Argument {
                                        identifier: {Identifier {value: "baz"}}
                                    }
                                ]
                            }
                 }
            ]
    }

The schema could be:

    <!-- Path  ::=  terms:(Term ("/" Term)*) -->

    <complexType name="Path">
      <sequence>
        <element name="terms" type="Term" minOccurs="1" maxOccurs="unbounded"/>
      </sequence>
    </complexType>

    <!--
    Term  ::=  Indentifier
           |   Wildcard
           |   Function
           |   Selection
    -->

    <complexType name="Term">
      <choice>
        <element name="identifier" type="Identifier" />
        <element name="wildcard"   type="Wildcard" />
        <element name="function"   type="Function" />
        <element name="selection"  type="Selection" />
      </choice>
    </complexType>

    <!-- Identifier  ::=  C_IDENTIFIER -->
    
    <complexType name="Identifier">
      <sequence>
        <element name="value" type="string" />
      </sequence>
    </complexType>

    <!-- Wildcard  ::=  "*" -->
    
    <complexType name="Identifier">
      <sequence>
      </sequence>
    </complexType>

    <!--
    Function  ::=  name:Identifier args:("(" (Argument ("," Argument)*)? ")")
    -->

    <complexType name="Function">
      <sequence>
        <element name="name" type="Identifier" />
        <element name="args" type="Argument" 
                 minOccurs="0" maxOccurs="unbounded" />
      </sequence>
    </complexType>

    <!--
    Argument  ::= identifier:Identifier
               |  stringLiteral:StringLiteral
               |  integer:Integer
    -->

    <!-- StringLiteral  ::=  value:C_STRING_LITERAL -->

    <!-- Integer  ::=  value:C_INTEGER -->

    <!-- Selection  ::=  array:Identifier "[" index:Integer "]" -->

You might end up with code like:

    struct ParserUtil {
        static int parse(Path *output, streambuf *input, ostream *errors);
        static int parse(Term *output, streambuf *input, ostream *errors);
        // etc.
    };

Wouldn't that be nice?

The bdlat grammar will be parsed by Racket into:

    (grammar
      ; Path  ::=  terms:(Term ("/" Term)*)
      ;
      (rule 'Path
        (bind 'terms (seq (type 'Term) (star (seq (literal "/") (type 'Term)))))
        
      ; Term  ::=  identifier:Indentifier
      ;        |   wildcard:Wildcard
      ;        |   function:Function
      ;        |   selection:Selection
      ;
      (rule 'Term
        (choice (bind 'identifer (type 'Identifier))
                (bind 'wildcard (type 'Wildcard))
                (bind 'function (type 'Function))
                (bind 'selection (type 'Selection))))

      ; Function  ::=  name:Identifier args:("(" (Argument ("," Argument)*)? ")")
      ;
      (rule 'Function
        (seq (bind 'name (type 'Identifier))
             (bind 'args (seq (literal "(")
                              (question (seq (type 'Argument)
                                        (star (seq (literal ",") (type 'Argument))))
                              (literal ")")))))
                               