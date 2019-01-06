#include <snazzy_parseutil.h>
#include <snazzy_lexer.h>
#include <snazzy_messages.h>

#include <bdlb_literalutil.h>
#include <bdlb_numericparseutil.h>
#include <bsls_assert.h>
#include <bsls_nameof.h>

#include <bsl_ostream.h>
#include <bsl_vector.h>

// clang-format off
//
// This component parses the productions from the following Parsley grammar:
/*
# e.g. 3*(2 + 4 / 2 + 6) - 1
# ... which is 29
Expression  ::=  sumOrDiff:SumOrDiff
             |   prodOrQuot:ProdOrQuot
             |   atom:Atom

Atom  ::=  identifier:IDENTIFIER
       |   number:NUMBER
       |   "(" expression:Expression ")"

SumOrDiff  ::=  left:SumOrDiffTerm (op:PlusOrMinus right:SumOrDiffTerm)+

SumOrDiffTerm  ::=  prodOrQuot:ProdOrQuot
                |   atom:Atom

ProdOrQuot  ::=  left:Atom (op:TimesOrDividedBy right:Atom)+

enumeration PlusOrMinus  ::=  "+" | "-"

enumeration TimesOrDividedBy  ::=  "*" | "/"

decimal NUMBER  ::=  /(0|[1-9][0-9]*)(\.[0-9]+)?/

IDENTIFIER  ::=  /[^0-9][a-zA-Z0-9_\-!@#$%^&*+=:\/?~]*\/
*/
// clang-format on

namespace BloombergLP {
namespace snazzy {
namespace {

typedef bsl::vector<LexerToken>::const_iterator TokenIter;

int read(Expression        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(Atom        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(SumOrDiff        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(SumOrDiffTerm        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(ProdOrQuot        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(PlusOrMinus::Value        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(TimesOrDividedBy::Value        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);
    // Parse a value from the beginning of the token sequence indicated by the
    // specified 'token' and the specified 'endTokens'. Return zero on success
    // or a nonzero value otherwise. Additionally on success, load the
    // advanced token iterator into the specified 'token', and if the specified
    // 'output' is not zero, load the parsed value into 'output'. Additionally
    // on failure, if the specified 'errors' is not zero, print a diagnostic
    // to 'errors' describing how parsing failed.

// TODO: declarations of generated read functions

int read(bsl::string      *output,
         LexerToken::Kind  tokenKind,
         TokenIter        *token,
         TokenIter         endTokens,
         bsl::ostream     *errors);
    // If the specified 'token' refers to a token having the specified
    // 'tokenKind', then increment 'token', and if additionally 'output' is not
    // zero, load the value of the matching token into 'output'. Return zero on
    // success or a nonzero value if 'token' does not refer to a token of kind
    // 'tokenKind' or is equal to the specified 'endTokens'. On failure, if the
    // specified 'errors' is not zero, write a diagnostic to 'errors'
    // describing how reading failed.

int read(double            *output,
         LexerToken::Kind  tokenKind,
         TokenIter            *token,
         TokenIter             endTokens,
         bsl::ostream         *errors);
    // If the specified 'token' refers to a token having the specified
    // 'tokenKind', then increment the token referred to by 'token', and if
    // additionally 'output' is not zero, convert the matching token into the
    // appropriate type and load the resulting value into 'output'. Return zero
    // on success or a nonzero value if: 'token' does not refer to a token of
    // kind 'tokenKind', 'token' is equal to the specified 'endTokens', or the
    // token is not convertible to the type of 'output'. On failure, if the
    // specified 'errors' is not zero, write a diagnostic to 'errors'
    // describing the failure.

bool ignore(LexerToken::Kind tokenKind)
    // Return whether the specified 'tokenkind' ought to be ignored by the
    // parser.
{
    switch (tokenKind) {

      default:
        return false;
    }
}

bsl::string quoted(const bslstl::StringRef& input)
{
    bsl::string result;
    bdlb::LiteralUtil::createQuotedEscapedCString(&result, input);
    return result;
}

int tokenize(bsl::vector<LexerToken>       *tokens,
             const bslstl::StringRef&      input,
             bsl::ostream&                      errorStream,
             Lexer                      *lexer)
{
    return (lexer ? *lexer : Lexer())(&tokens, input, errorStream);
}

template <typename Object>
int genericParse(Object                        *output,
                 const bslstl::StringRef&  input,
                 bsl::ostream&                  errorStream,
                 Lexer                  *lexer)
{
    bsl::vector<LexerToken> tokens;
    if (const int rc = tokenize(&tokens, input, errorStream, lexer)) {
        const char *const name = bsls::NameOf<Object>::name();
        errorStream << "Unable to parse a " << name << "; wasn't able to lex "
                       "all of the input.";
        return rc;                                                    // RETURN
    }

    TokenIter tokenIter = tokens.begin();

    if (int rc = read(output, &tokenIter, tokens.end(), &errorStream))
    {
        const char *const name = bsls::NameOf<Object>::name();
        errorStream << "Unable to parse a " << name << "; wasn't able to read "
                       "one from the input.";
        return rc;                                                    // RETURN
    }

    if (tokenIter != tokens.end())
    {
        const char *const name = bsls::NameOf<Object>::name();
        errorStream << "Parsed a " << name << " but didn't exhaust the input.";
        return 3;                                                     // RETURN
    }

    return 0;
}

template <typename Member>
Member *getMemberPtr(Member& member)
    // Return a pointer to the specified 'member' of some class parsed by
    // this component.
{
    return &member;
}

template <typename Member>
Member *getMemberPtr(bsl::vector<Member>& member)
    // Return a pointer to a default-constructed object appended to the end of
    // the specified 'member' of some class parsed by this component.
{
    member.emplace_back();
    return &member.back();
}

template <typename Member>
void resetMember(Member&)
    // Non-nullable, non-array members need no resetting.
{
}

template <typename Member>
void resetMember(bsl::vector<Member>& member)
    // Erase the last element of the specified array-valued 'member' of some
    // class parsed by this component. This undoes the operation performed by
    // 'getMemberPtr'.
{
    BSLS_ASSERT(!member.empty());
    member.pop_back();
}

#define MEMBER(OBJECT, ACCESSOR) \
    OBJECT ? getMemberPtr(OBJECT->ACCESSOR()) : 0

#define RESET(OBJECT, ACCESSOR)              \
    do {                                     \
        if (OBJECT) {                        \
            resetMember(OBJECT->ACCESSOR()); \
        }                                    \
    } while (false)

// TODO: generated reader function definitions here

#undef RESET
#undef MEMBER

int read(bsl::string          *output,
         LexerToken::Kind  tokenKind,
         TokenIter            *tokenIterPtr,
         TokenIter             endTokens,
         bsl::ostream         *errors)
{
    // Parsing functions won't ask for an ignored token kind.
    BSLS_ASSERT(!ignored(tokenKind));

    BSLS_ASSERT(tokenIterPtr);
    TokenIter tokenIter = *tokenIterPtr;  // a copy

    // Skip ignored tokens.
    for (;; ++tokenIter)
    {
        if (tokenIter == endTokens)
        {
            if (errors) {
                *errors << "Unable to read a token of kind " << tokenKind
                        << " because there are no more tokens.\n";
            }
            return 1;                                                 // RETURN
        }

        if (!ignored(tokenIter->d_kind))
        {
            break;
        }
    }

    if (tokenIter->d_kind != tokenKind)
    {
        if (errors) {
            *errors << "Current token is not of the expected kind "
                    << tokenKind << " but is instead of the kind "
                    << tokenIter->d_kind << " with the value "
                    << quoted(tokenIter->d_value) << '\n';
        }
        return 2;                                                     // RETURN
    }

    if (output) {
        *output = tokenIter->d_value;
    }

    ++tokenIter;
    *tokenIterPtr = tokenIter;
    return 0;
}
template <typename Number, typename NumberParser>
int readNumeric(Number               *output,
                LexerToken::Kind  tokenKind,
                TokenIter            *tokenIterPtr,
                TokenIter             endTokens,
                bsl::ostream         *errors
                NumberParser          parseNumber)
{
    BSLS_ASSERT(tokenIterPtr);

    TokenIter         tokenIter(*tokenIterPtr);
    bsl::string       value;
    if (const int rc =
            read(&value, tokenKind, &tokenIter, endTokens, errors)) {
        if (errors) {
            const char *const name = bsls::NameOf<Number>::name();
            *errors << "Unable to read a " << name << " from a token of the "
                       " kind " << tokenKind << " because reading the token "
                       "itself failed.\n";
        }
        return rc;                                                    // RETURN
    }

    // If the end had been reached, then 'read' would have failed above.
    BSLS_ASSERT(*tokenIterPtr != endTokens);

    const LexerToken&  token = **tokenIterPtr;
    Number                 parsedValue;
    bslstl::StringRef remainderOrErrorLocation;
    if (const int rc = parseNumber(&parsedValue,
                                   &remainderOrErrorLocation,
                                   token.d_value))
    {
        if (errors) {
            const char *const name = bsls::NameOf<Number>::name();
            *errors << "Unable to convert " << quoted(token.d_value)
                    << " to a " << name << ". Error occurred beginning at "
                    << quoted(remainderOrErrorLocation) << '\n';
        }
        return rc;                                                    // RETURN
    }

    if (!remainderOrErrorLocation.empty()) {
        if (errors) {
            const char *const name = bsls::NameOf<Number>::name();
            *errors << "Parsed a " << name << ' ' << parsedValue << " from "
                    << quoted(token.d_value)
                    << " without consuming the entire string. "
                    << quoted(remainderOrErrorLocation) << " remains.\n";
        }
        return 1;                                                     // RETURN
    }

    if (output) {
        *output = parsedValue;
    }

    *tokenIterPtr = tokenIter;
    return 0;
}

#define DEFINE_NUMERIC_READ(TYPE, FUNC)                                       \
    int FUNC(TYPE                          *output,                           \
             bslstl::StringRef        *remainderOrErrorLocation,         \
             const bslstl::StringRef&  input)                            \
    {                                                                         \
        /* This wrapper function prevents overload ambiguity. */              \
        return bdlb::NumericParseUtil::FUNC(output,                      \
                                                 remainderOrErrorLocation,    \
                                                 input);                      \
    }                                                                         \
                                                                              \
    int read(TYPE                 *output,                                    \
             LexerToken::Kind  tokenKind,                                 \
             TokenIter            *tokenIterPtr,                              \
             TokenIter             endTokens,                                 \
             bsl::ostream         *errors)                                    \
    {                                                                         \
        return readNumeric(output,                                            \
                           tokenKind,                                         \
                           tokenIterPtr,                                      \
                           endTokens,                                         \
                           errors,                                            \
                           &FUNC);                                            \
    }

DEFINE_NUMERIC_READ(double, parseDouble)

#undef DEFINE_NUMERIC_READER

}  // close unnamed namespace

                            // ----------------
                            // struct ParseUtil
                            // ----------------

int ParseUtil::parse(Expression *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(Atom *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(SumOrDiff *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(SumOrDiffTerm *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(ProdOrQuot *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(PlusOrMinus::Value *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(TimesOrDividedBy::Value *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

}  // close package namespace
}  // close enterprise namespace