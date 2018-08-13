#lang racket

(struct op:*   ... #:transparent)
(struct op:+   ... #:transparent)
(struct op:?   ... #:transparent)
(struct op:cat ... #:transparent)
(struct op:alt ... #:transparent)

; How do I get to this code?

#|
// Rule  ::=  ignore:("ignore"?) name:IDENTIFIER ("::="|":") pattern:Pattern
//
int read(Rule          *object,
         TokenIterator *token,
         TokenIterator  end,
         bsl::ostream  *errors)
{
    read(&rule.ignore(), Tokens::e_IGNORE, token, end);

    if (read(&rule.name(), Tokens::e_IDENTIFIER, token, end, errors)) {
        if (errors) {
            // TODO: report error
        }
        return 1;
    }

    if (read(Tokens::e_RULE_ANON_1, token, end, errors)) {
        if (errors) {
            // TODO: report error
        }
        return 2;
    }

    if (read(object->pattern(), token, end, errors)) {
        if (errors) {
            // TODO: report error
        }
        return 3;
    }

    return 0;
}


// Function  ::=  
//     name:IDENTIFIER 
//     args:("(" (Argument (SEPARATOR Argument)*)? ")")
//
int readFunction(Function      *object,
                 TokenIterator *token,
                 TokenIterator  end,
                 bsl::ostream  *errors)
{
    if (read(&object->name(), Tokens::e_IDENTIFIER, token, end, errors)) {
        if (errors) {
            // TODO: report error
        }
        return 1;
    }

    if (readFunctionArgs(&object->args(), token, end, errors)) {
        if (errors) {
            // TODO: report error
        }
        return 2;
    }

    return 0;
}

// from another grammar...
//
// QuantifiedPatternTerm  ::=  star:(UnquantifiedPatternTerm "*")
//                         |   plus:(UnquantifiedPatternTerm "+")
//                         |   question:(UnquantifiedPatternTerm "?")
//                         |   term:UnquantifiedPatternTerm
//
int read(QuantifiedPatternTerm *object,
         TokenIterator         *token,
         TokenIterator          end,
         bsl::ostream          *errors)
{
    TokenIterator fromToken = *token;

    // TODO: Could the generator optimize this?

    if (!read(&object->makeStartValue(), &fromToken, end) &&
        !read(Tokens::e_STAR))
    {
        *token = fromToken;
        return 0;
    }

    fromToken = *token;
    if (!read(&object->makePlusValue(), &fromToken, end) && 
        !read(Tokens::e_PLUS)) 
    {
        *token = fromToken;
        return 0;
    }

    fromToken = *token;
    if (!read(&object->makeQuestionValue(), &fromToken, end) &&
        !read(Tokens::e_QUESTION))
    {
        *token = fromToken;
        return 0;
    }

    fromToken = *token;
    if (!read(&object->makeTermValue(), &fromToken, end))
    {
        *token = fromToken;
        return 0;
    }

    if (errors) {
        // TODO: report errors
    }

    return 1;
}

|#