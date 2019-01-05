
#include <package_parserutil.h>
#include <package_lexer.h>

#include <bdlb_literalutil.h>
#include <bdlb_numericparseutil.h>  // only if there are typed tokens

#include <bdlf_bind.h>
#include <bdlf_placeholder.h>

#include <bsls_nameof.h>

#include <bsl_vector.h>

namespace Enterprise {
namespace package {
namespace {

typedef bsl::vector<LexerToken>::const_iterator TokenIter;

int read(Foo          *output, 
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(Bar          *output, 
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

int read(int              *output,
         LexerToken::Kind  tokenKind,
         TokenIter        *token,
         TokenIter         endTokens,
         bsl::ostream     *errors);
    // If the specified 'token' refers to a token having the specified
    // 'tokenKind', then increment 'token', and if additionally 'output' is not
    // zero, convert the matching token into the appropriate type and load the
    // resulting value into 'output'. Return zero on success or a nonzero value
    // if: 'token' does not refer to a token of kind 'tokenKind', 'token' is
    // equal to the specified 'endTokens', or the token is not convertible to
    // the type of 'output'. On failure, if the specified 'errors' is not zero,
    // write a diagnostic to 'errors' describing the failure.

bool ignore(LexerToken::Kind tokenKind)
    // Return whether the specified 'tokenkind' ought to be ignored by the
    // parser.
{
    switch (tokenKind) {
      case LexerToken::e_COMMENT: return true;                        // RETURN
      case LexerToken::e_WS_LEFT: return true;                        // RETURN
      case LexerToken::e_WS_END: return true;                         // RETURN
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

int tokenize(bsl::vector<LexerToken>  *tokens,
             const bslstl::StringRef&  input,
             bsl::ostream&             errorStream,
             Lexer                    *lexer)
{
    return (lexer ? *lexer : Lexer())(&tokens, input, errorStream);
}

template <typename OBJECT>
int genericParse(OBJECT                   *output,
                 const bslstl::StringRef&  input,
                 bsl::ostream&             errorStream,
                 Lexer                    *lexer)
{
    bsl::vector<LexerToken> tokens;
    if (const int rc = tokenize(&tokens, input, errorStream, lexer)) {
        return rc;                                                    // RETURN
    }

    TokenIter begin = tokens.begin();

    return read(output, &begin, tokens.end(), &errorStream);
}

template <typename Member>
Member *getMemberPtr(Member& member)
    // Return a pointer to the specified 'member' of some class parsed by
    // this component.
{
    return &member;
}

template <typename Member>
Member *getMemberPtr(bdlb::NullableValue<Member>& member)
    // Return a pointer to a default-constructed object within the specified
    // 'member' of some class parsed by this component.
{
    return &member.makeValueInplace();
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
void resetMember(bdlb::NullableValue<Member>& member)
    // Load a null value into the specified nullable 'member' of some class
    // parsed by this component. This undoes the operation performed by
    // 'getMemberPtr'.
{
    member.reset();
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
    while (false)

// "," bars:Bar
int read(Foo          *output, 
         TokenIter    *tokenIterPtr,
         TokenIter     endTokens,
         bsl::ostream *errors)
{
    BSLS_ASSERT(tokenIterPtr);
    TokenIter tokenIter = *tokenIterPtr;  // a copy

    // ","
    if (const int rc =
            read(0, Token::e_TOKEN_2, &tokenIter, endTokens, errors))
    {
        if (errors) {
            *errors << "Unable to read... TODO";
        }
        return 1;                                                     // RETURN
    }

    // bars:Bar
    if (const int rc =
            read(MEMBER(output, bars), &tokenIter, endTokens, errors))
    {
        if (errors) {
            *errors << "Unable to read a Foo because... TODO";
        }
        RESET(output, bars);
        return rc;                                                    // RETURN
    }

    *tokenIterPtr = tokenIter;
    return 0;
}

// Foo  ::=  "say"? bars:Bar ("," bars:Bar)*
int read(Foo          *output, 
         TokenIter    *tokenIterPtr,
         TokenIter     endTokens,
         bsl::ostream *errors)
{
    BSLS_ASSERT(tokenIterPtr);
    TokenIter tokenIter = *tokenIterPtr;  // a copy

    // "say"?
    read(0, Token::e_TOKEN_3, &tokenIter, endTokens, 0);

    // bars:Bar
    if (const int rc =
            read(MEMBER(output, bars), &tokenIter, endTokens, errors))
    {
        if (errors) {
            *errors << "Unable to read a Foo because... TODO";
        }
        RESET(output, bars);
        return rc;                                                    // RETURN
    }

    // ("," bars:Bar)*
    while (readFoo_1(output, &tokenIter, endTokens, errors) == 0)
        ;

    *tokenIterPtr = tokenIter;
    return 0;
}

// Bar  ::=  number:/0|[1-9][0-9]*/ | fish:"fish" | chicken:"chicken"
int read(Bar          *output, 
         TokenIter    *tokenIterPtr,
         TokenIter     endTokens,
         bsl::ostream *errors)
{
    BSLS_ASSERT(tokenIterPtr);
    TokenIter tokenIter = *tokenIterPtr;

    if (read(MEMBER(output, makeNumber), 
             LexerToken::e_TOKEN_1, 
             &tokenIter, 
             endTokens,
             0) == 0)
    {
        *tokenIterPtr = tokenIter;
        return 0;                                                     // RETURN
    }
    RESET(output, makeNumber);

    if (read(MEMBER(output, makeFish),
             LexerToken::e_TOKEN_4,
             &tokenIter,
             endTokens,
             0) == 0)
    {
        *tokenIterPtr = tokenIter;
        return 0;                                                     // RETURN
    }
    RESET(output, makeFish);

    if (read(MEMBER(output, makeChicken),
             LexerToken::e_TOKEN_5,
             &tokenIter,
             endTokens,
             0))
    {
        *tokenIterPtr = tokenIter;
        return 0;                                                     // RETURN
    }
    RESET(output, makeChicken);

    if (errors)
    {
        *errors << "None of the possibilities in the following alternation "
                   "could be read: number:/0|[1-9][0-9]*/ | fish:\"fish\" | "
                   "chicken:\"chicken\"\n";
    }

    return 1;
}

int read(bsl::string      *output,
         LexerToken::Kind  tokenKind,
         TokenIter        *tokenIterPtr,
         TokenIter         endTokens,
         bsl::ostream     *errors)
{
    BSLS_ASSERT(tokenIterPtr);

    TokenIter& tokenIter = *tokenIterPtr;

    if (tokenIter == endTokens) {
        if (errors) {
            *errors << "Reached the end of tokens. Unable to read token of "
                       "kind " << tokenKind << '\n';
        }
        return 1;                                                      // RETURN
    }

    if (tokenIter->d_kind != tokenKind)
    {
        if (errors) {
            *errors << "Current token is not of the expected kind "
                    << tokenKind << " but is instead of the kind "
                    << tokenIter->d_kind << " with the value "
                    << quoted(tokenIter->d_value) << '\n';
        }
        return 2;                                                      // RETURN
    }

    if (output) {
        *output = tokenIter->d_value;
    }

    ++tokenIter;
    return 0;
}

template <typename Number, typename NumberParser>
int readNumeric(Number           *output,
                LexerToken::Kind  tokenKind,
                TokenIter        *tokenIterPtr,
                TokenIter         endTokens,
                bsl::ostream     *errors
                NumberParser      parseNumber)
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
        return rc;
    }

    // If we had started at the end, reading above would have failed.
    BSLS_ASSERT(*tokenIterPtr != endTokens);

    const Token&      token = **tokenIterPtr;
    Number            parsedValue;
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
        return rc;
    }

    if (!remainderOrErrorLocation.empty()) {
        if (errors) {
            const char *const name = bsls::NameOf<Number>::name();
            *errors << "Parsed a " << name << ' ' << parsedValue << " from "
                    << quoted(token.d_value)
                    << " without consuming the entire string. "
                    << quoted(remainderOrErrorLocation) << " remains.\n";
        }
        return 1;
    }

    if (output) {
        *output = parsedValue;
    }

    *tokenIterPtr = tokenIter;
    return 0;
}

#define DEFINE_NUMERIC_READ(TYPE, FUNC)                                       \
    int FUNC(TYPE                     *result,                                \
             bslstl::StringRef        *remainder,                             \
             const bslstl::StringRef&  input)                                 \
    {                                                                         \
        /* This wrapper function exists to avoid overload ambiguity. */       \
        return bdlb::NumericParserUtil::FUNC(result, remainder, input);       \
    }                                                                         \
                                                                              \
    int read(TYPE             *output,                                        \
             LexerToken::Kind  tokenKind,                                     \
             TokenIter        *tokenIterPtr,                                  \
             TokenIter         endTokens,                                     \
             bsl::ostream     *errors)                                        \
    {                                                                         \
        using namespace bdlf::PlaceHolders;                                   \
                                                                              \
        return readNumeric(                                                   \
                   output, tokenKind, tokenIterPtr, endTokens, errors, &FUNC);\
    }

DEFINE_NUMERIC_READ(int, parseInt)

}  // close unnamed namespace

                            // -----------------
                            // struct ParserUtil
                            // -----------------

int ParserUtil::parse(Foo                      *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&             errorStream,
                      Lexer                    *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParserUtil::parse(Bar                      *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&             errorStream,
                      Lexer                    *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

}  // close package namespace
}  // close enterprise namespace
