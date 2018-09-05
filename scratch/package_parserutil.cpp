
#include <package_parserutil.h>
#include <package_lexer.h>

#include <bsl_vector.h>

namespace Enterprise {
namespace package {
namespace {

typedef bsl::vector<LexerToken>::const_iterator TokenIter;

// TODO: Declare all the read functions

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

// TODO: Define all the read functions

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
