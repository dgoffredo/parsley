Languages
=========
Parsley proceeds as a tree of transformations starting with the parse tree
of the grammar (`.parsley`) file and ending with an XSD and various C++ source
files.

Lang:0
------
This is the input grammar language, e.g.:
```
Path  ::=  (schema:IDENTIFIER ":" "//"?)? terms:Term ("/" terms:Term)*

Term  ::=  identifier:IDENTIFIER
        |  wildcard:"*"
        |  function:Function
        |  selection:Selection

Function  ::=  
    name:IDENTIFIER 
    ("(" (args:Argument (SEPARATOR args:Argument)*)? ")")

Argument  ::= identifier:IDENTIFIER
            |  stringLiteral:STRING
            |  integer:INTEGER

Selection  ::=  array:IDENTIFIER "[" index:INTEGER "]"

IDENTIFIER  ::=  /[a-zA-Z_][0-9a-zA-Z_]*/

integer INTEGER  ::=  /0|[1-9][0-9]*/

STRING  ::=  /"([^\\]|\\.)*"/

SEPARATOR  ::=  /\s*,\s*/
```

Lang:1
------
This is the parse tree of the input grammar, e.g.
```racket
'(Grammar
  (Rule
   Path
   (Concatenation
    (Question (Concatenation (Bound schema IDENTIFIER) ":" (Question "//")))
    (Bound terms Term)
    (Star (Concatenation "/" (Bound terms Term)))))
  (Rule
   Term
   (Alternation
    (Bound identifier IDENTIFIER)
    (Bound wildcard "*")
    (Bound function Function)
    (Bound selection Selection)))
  (Rule
   Function
   (Concatenation
    (Bound name IDENTIFIER)
    (Concatenation
     "("
     (Question
      (Concatenation
       (Bound args Argument)
       (Star (Concatenation SEPARATOR (Bound args Argument)))))
     ")")))
  (Rule
   Argument
   (Alternation
    (Bound identifier IDENTIFIER)
    (Bound stringLiteral STRING)
    (Bound integer INTEGER)))
  (Rule
   Selection
   (Concatenation (Bound array IDENTIFIER) "[" (Bound index INTEGER) "]"))
  (Rule IDENTIFIER (regex "[a-zA-Z_][0-9a-zA-Z_]*"))
  (Rule integer INTEGER (regex "0|[1-9][0-9]*"))
  (Rule STRING (regex "\"([^\\\\]|\\\\.)*\""))
  (Rule SEPARATOR (regex "\\s*,\\s*")))
```

Lang:2
------
This language is a series of categorized rules, where tree paths have been
calculated for bindings, modifiers (such as "ignore" or "integer") have been
extracted from terminals, but types have not yet been analyzed.
```racket
(list
 (rule/class
  'Path
  '(Concatenation
    (Alternation
     (Concatenation (Bound schema IDENTIFIER) ":" (Question "//"))
     ())
    (Bound terms Term)
    (Star (Concatenation "/" (Bound terms Term))))
  '((schema (0 0 0)) (terms (1)) (terms (2 0 1))))
 (rule/class
  'Term
  '(Alternation
    (Bound identifier IDENTIFIER)
    (Bound wildcard "*")
    (Bound function Function)
    (Bound selection Selection))
  '((identifier (0)) (wildcard (1)) (function (2)) (selection (3))))
 (rule/class
  'Function
  '(Concatenation
    (Bound name IDENTIFIER)
    (Concatenation
     "("
     (Alternation
      (Concatenation
       (Bound args Argument)
       (Star (Concatenation SEPARATOR (Bound args Argument))))
      ())
     ")"))
  '((name (0)) (args (1 1 0 0)) (args (1 1 0 1 0 1))))
 (rule/class
  'Argument
  '(Alternation
    (Bound identifier IDENTIFIER)
    (Bound stringLiteral STRING)
    (Bound integer INTEGER))
  '((identifier (0)) (stringLiteral (1)) (integer (2))))
 (rule/class
  'Selection
  '(Concatenation (Bound array IDENTIFIER) "[" (Bound index INTEGER) "]")
  '((array (0)) (index (2))))
 (rule/terminal '() 'IDENTIFIER '(regex "[a-zA-Z_][0-9a-zA-Z_]*"))
 (rule/terminal '(integer) 'INTEGER '(regex "0|[1-9][0-9]*"))
 (rule/terminal '() 'STRING '(regex "\"([^\\\\]|\\\\.)*\""))
 (rule/terminal '() 'SEPARATOR '(regex "\\s*,\\s*")))
 ```

Lang:3
------
This language separates rules into sections by category: class, terminal,
and other. All production types have been analyzed, including class types
and class member types. Terminals have all been converted into regular
expressions.
```racket
(grammar
    #:classes
    (list
      (sequence 'Path
        #:elements
        (list
          (element 'schema (nullable (basic 'string)))
          (element 'terms (array 'Term)))
        #:rule
        (rule/class 'Path
          ...))
      ...)
    #:terminals
    (list
      (terminal 'IDENTIFIER (basic 'string) "[a-zA-Z_][0-9a-zA-Z_]*")
      (terminal 'INTEGER (basic 'integer) "0|[1-9][0-9]*")
      ...)
    #:others
    (list
        ...))
```

Lang:3/0/0
----------
This language branches off of `Lang:3`. The goal is to produce an XSD. First,
strip out everything but the classes, and convert them into [stag][stag]
`bdlat` `struct`s.
```racket
(list
  (bdlat:sequence "Path"
    '("This is where documentation should go.")
    (list
      (element "schema" (nullable (basic "string"))
        '("Here there could also be documentation"))
      (element "paths" (array "Path")
        '())))
  ...)
```

Lang:3/0/1
----------
This language is the SXML representation of an XSD produced from the `bdlat`
types in `Lang:3/0/0`.
```racket
'(*TOP*
  (@
   (*NAMESPACES*
    (xs "http://www.w3.org/2001/XMLSchema")
    (tns "http://enterprise.com/my-target-namespace")))

  (xs:schema
   (@ (targetNamespace "http://enterprise.com/my-target-namespace"))

   (xs:complexType
    (@ (name "Path"))
    (xs:sequence
     (xs:element (@ (type "xs:string") (name "schema") (minOccurs "0")))
     (xs:element
      (@
       (type "tns:Part")
       (name "parts")
       (minOccurs "0")
       (maxOccurs "unbounded")))))

    ...))
```
Lang:3/0/2
----------
This language is the XML representation of the SXML from the previous language.
This is one of parsley's final languages: this XML is part of the output.
```xml
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema"
           xmlns:tns="http://enterprise.com/my-target-namespace"
           targetNamespace="http://enterprise.com/my-target-namespace">

  <xs:complexType name="Path">
    <xs:sequence>
      <xs:element name="schema" type="xs:string" minOccurs="0"/>
      <xs:element name="parts"  type="tns:Part" 
                  minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <!-- ... other types ... -->
</xs:schema>
```

Lang:3/1/0
----------
This language branches off of `Lang:3`. TODO: This is the branch that will
begin to deduce which "read" functions will appear in the parsing component.

Lang:3/1/1
----------
TODO: "read" functions, "parse" functions, the combined tokens regex, etc.

Lang:3/1/2
----------
TODO: language resembling a restricted C++ AST, or possibly just parameters to
string templates that will produce C++.

Lang:3/1/3
----------
TODO: C++ source code for the lexing and parsing components.

[stag]: https://github.com/dgoffredo/stag