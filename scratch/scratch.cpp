// Let's first try to write a recursive descent parser by hand-using BDE stuff,
// and only then think about writing a parser generator.

namespace Enterprise {
namespace pathstuff {

struct ParserUtil {
    // TODO: documentation

    static int parse(Path                     *object, 
                     const bslstl::StringRef&  input,
                     bsl::ostream&             errors);

    static int parse(Term                     *object,
                     const bslstl::StringRef&  input,
                     bsl::ostream&             errors);

    static int parse(Function                 *object,
                     const bslstl::StringRef&  input,
                     bsl::ostream&             errors);

    static int parse(Argument                 *object,
                     const bslstl::StringRef&  input,
                     bsl::ostream&             errors);
};

namespace {

namespace tokens {
    enum Kind { 
        e_FORWARD_SLASH // ...
    };
}  // close tokens namespace

struct Token {
    tokens::Kind      kind;
    bslstl::StringRef value;
};

int nextToken(Token *output, bslstl::StringRef *remaining);
    // ...

class TokenIterator {
    // ...
};

TokenIterator lex(const bslstl::StringRef& input);
    // ...

int read(tokens::Kind   tokenKind,
         TokenIterator *tokenIter,
         bsl::ostream  *errors = 0);
    // ...

int read(bsl::string   *tokenValue,
         tokens::Kind   tokenKind,
         TokenIterator *tokenIter,
         bsl::ostream  *errors = 0);

int read(/*OBJECT*/    *object,
         TokenIterator *tokenIter,
         bsl::ostream  *errors = 0);
    // ...

}  // close unnamed namespace

#define IMPLEMENT_PARSE_FUNCTION(TYPE)                       \
    int ParserUtil::parse(TYPE                     *object,  \
                          const bslstl::StringRef&  input,   \
                          bsl::ostream&             errors)  \
    {                                                        \
        BSLS_ASSERT_OPT(object);                             \
                                                             \
        TokenIterator tokenIter = lex(input);                \
        return read(object, &tokenIter, &errors);            \
    }

IMPLEMENT_PARSE_FUNCTION(Path)
IMPLEMENT_PARSE_FUNCTION(Term)
IMPLEMENT_PARSE_FUNCTION(Function)
IMPLEMENT_PARSE_FUNCTION(Argument)

#undef IMPLEMENT_PARSE_FUNCTION

namespace {

// maybe TokenCursor := (TokenIterator, end TokenIterator, atEnd() method)

int read(Path          *object,
         TokenIterator *tokenIter,
         bsl::ostream  *errors = 0)
{
    BSLS_ASSERT_OPT(object);
    BSLS_ASSERT_OPT(tokenIter);

    const char k_RULE[]  = "Path  ::=  terms:(Term (\"/\" Term)*)"
                           "\n";

    // Path  ::=  terms:(Term ("/" Term)*)
    Term term;
    // (Term ("/" Term)*)
    if (read(&term, tokenIter)) {
        if (errors) {
            *errors << "Unable to read first Term in rule:\n" << k_RULE;
        }
        return 1;
    }

    object->terms.push_back(term);

    // ("/" Term)*
    for (;;) {
        // "/"
        if (read(tokens::e_FORWARD_SLASH, tokenIter)) {
            break;
        }
        // Term
        if (read(&term, tokenIter)) {
            break;
        }
        object->terms.push_back(item);
    }

    return 0;
}

int read(Term          *object,
         TokenIterator *tokenIter,
         bsl::ostream  *errors = 0)
{
    BSLS_ASSERT_OPT(object);
    BSLS_ASSERT_OPT(tokenIter);

    const char k_RULE[]  = "Term  ::=  identifier:IDENTIFIER\n"
                           "       |   wildcard:\"*\"\n"
                           "       |   function:Function\n"
                           "       |   selection:Selection"
                           "\n";

    //  Term  ::=  identifier:IDENTIFIER
    //         |   wildcard:"*"
    //         |   function:Function
    //         |   selection:Selection
    
    // identifier:IDENTIFIER
    object->makeIdentifierValue();
    if (!read(&object->identifier(), tokenIter)) {
        return 0;
    }

    // wildcard:"*"
    object->makeWildcardValue();
    if (!read(&object->wildcard(), tokenIter)) {
        return 0;
    }

    // function:Function
    object->makeFunctionValue();
    if (!read(&object->function(), tokenIter)) {
        return 0;
    }

    // selection:Selection
    object->makeSelectionValue();
    if (!read(&object->selection(), tokenIter)) {
        return 0;
    }

    if (errors) {
        *errors << "Input didn't match any pattern in:\n" << k_RULE;
    }

    return 1;
}

int readFunctionArgs(bsl::vector<Argument> *arguments, 
                     TokenIterator         *tokenIter,
                     bsl::ostream          *errors = 0)
{
    const char k_PATTERN[] = "Argument (SEPARATOR Argument)*";

    // Argument (SEPARATOR Argument)*

    // Argument
    Argument argument;
    if (read(&argument, tokenIter, errors)) {
        if (errors) {
            *errors << "Unable to read first Argument in pattern:"
                    << k_PATTERN;
        }

        return 2;
    }
    
    arguments->push_back(argument);

    // (SEPARATOR Argument)*
    for (;;) {
        if (read(tokens::e_SEPARATOR, tokenIter)) {
            break;
        }

        if (read(&argument, tokenIter)) {
            break;
        }

        arguments->push_back(argument);
    }

    return 0;
}

int read(Function      *object,
         TokenIterator *tokenIter,
         bsl::ostream  *errors = 0)
{
    BSLS_ASSERT_OPT(object);
    BSLS_ASSERT_OPT(tokenIter);

    const char k_RULE[]  = 
        "Function  ::=  name:IDENTIFIER\n"
        "               args:(\"(\" (Argument (SEPARATOR Argument)*)? \")\")"
        "\n";

    //  Function  ::=  
    //      name:IDENTIFIER 
    //      args:("(" (Argument (SEPARATOR Argument)*)? ")")

    // name:IDENTIFIER
    if (read(&object->name(), tokens::e_IDENTIFIER, tokenIter, errors)) {
        if (errors) {
            *errors << "Unable to read name in rule:\n" << k_RULE;
        }

        return 1;
    }

    // args:("(" (Argument (SEPARATOR Argument)*)? ")")

    // "("
    if (read(tokens::e_LPAREN, tokenIter, errors)) {
        if (errors) {
            *errors << "Unable to read first \"(\" in rule:\n" << k_RULE;
            return 2;
        }
    }

    // (Argument (SEPARATOR Argument)*)? 
    readFunctionArgs(&object->args(), tokenIter);

    if (read(tokens::e_RPAREN, tokenIter, errors)) {
        if (errors) {
            *errors << "Unable to read closing \")\" for rule:\n" << k_RULE;
        }

        return 3;
    }

    return 0;
}

//  IDENTIFIER  ::=  /[a-zA-Z_][0-9a-zA-Z_]*/
//
//  INTEGER  ::=  /0|[1-9][0-9]*/
//
//  STRING  ::=  /"([^\\"]|\\.)*"/
//
//  SEPARATOR  ::=  /\s*,\s*/

// TODO: How do you know which one worked?
const char k_TOKEN_PATTERN =
    "^(?:"
          "(?P<IDENTIFIER>[a-zA-Z_][0-9a-zA-Z_]*)"
      "|" "(?P<INTEGER>0|[1-9][0-9]*)"
      "|" "(?P<STRING>\"([^\\\\\"]|\\\\.)*\")"
      "|" "(?P<SEPARATOR>\\s*,\\s*)"
     ")";

}  // close unnamed namespace