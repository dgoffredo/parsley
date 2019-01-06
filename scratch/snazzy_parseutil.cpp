#include <snazzy_parseutil.h>
#include <snazzy_lexer.h>
#include <snazzy_messages.h>

#include <bdlb_literalutil.h>

#include <bsls_assert.h>
#include <bsls_nameof.h>

#include <bsl_ostream.h>
#include <bsl_vector.h>

// clang-format off
//
// This component parses the productions from the following Parsley grammar:
/*
Grammar  ::=  rules:Rule (BLANK_LINE+ rules:Rule)* BLANK_LINE*

Rule  ::=  ignore:"ignore"? name:IDENTIFIER ("::="|":") pattern:Pattern

Pattern  ::=  alternation:Alternation      # one from multiple options
          |   concatenation:Concatenation  # one or more subpatterns

Alternation  ::=  patterns:PatternTerm ("|" patterns:PatternTerm)+

Concatenation  ::=  patterns:PatternTerm+

PatternTerm  ::=  bound:BoundPatternTerm
              |   unbound:QuantifiedPatternTerm

BoundPatternTerm  ::=  name:IDENTIFIER ":" term:QuantifiedPatternTerm

QuantifiedPatternTerm  ::=  star:UnquantifiedPatternTerm "*"
                        |   plus:UnquantifiedPatternTerm "+"
                        |   question:UnquantifiedPatternTerm "?"
                        |   term:UnquantifiedPatternTerm

UnquantifiedPatternTerm  ::=  literal:STRING
                          |   regex:REGEX
                          |   rule:IDENTIFIER
                          |   empty:EMPTY
                          |   "(" pattern:Pattern ")"

IDENTIFIER  ::=  /[a-zA-Z_][0-9a-zA-Z_]*\/

STRING  ::=  /"([^\\"]|\\.)*"/

REGEX  ::=  /\/([^\\\/]|\\.)*\//

EMPTY  ::=  /\(\)/

ignore COMMENT  ::=  /\(\*([^*]|\*[^)])*\*\)/

BLANK_LINE  ::=  /\s*\n\s*\n\s*\/

ignore WS_LEFT  ::=  /\s+(?=\S)/

ignore WS_END  ::=  /\s+$/
*/
// clang-format on

namespace BloombergLP {
namespace snazzy {
namespace {

typedef bsl::vector<LexerToken>::const_iterator TokenIter;

int read(Grammar        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(Rule        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(Pattern        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(Alternation        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(Concatenation        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(PatternTerm        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(BoundPatternTerm        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(QuantifiedPatternTerm        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);

int read(UnquantifiedPatternTerm        *output,
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

bool ignore(LexerToken::Kind tokenKind)
    // Return whether the specified 'tokenkind' ought to be ignored by the
    // parser.
{
    switch (tokenKind) {
      case LexerToken::e_COMMENT: return true;
      case LexerToken::e_WS_LEFT: return true;
      case LexerToken::e_WS_END: return true;
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

}  // close unnamed namespace

                            // ----------------
                            // struct ParseUtil
                            // ----------------

int ParseUtil::parse(Grammar *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(Rule *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(Pattern *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(Alternation *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(Concatenation *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(PatternTerm *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(BoundPatternTerm *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(QuantifiedPatternTerm *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

int ParseUtil::parse(UnquantifiedPatternTerm *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      Lexer                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}

}  // close package namespace
}  // close enterprise namespace