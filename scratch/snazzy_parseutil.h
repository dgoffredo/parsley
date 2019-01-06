#ifndef INCLUDED_SNAZZY_PARSEUTIL
#define INCLUDED_SNAZZY_PARSEUTIL

#include <snazzy_messages.h>

#include <bsl_iosfwd.h>
#include <bsl_string.h>

namespace BloombergLP {
namespace snazzy {
class Lexer;

struct ParseUtil {
    // This 'struct' provides a namespace of functions used to parse instances
    // of generated types from text.

    static int parse(Expression      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(Atom      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(SumOrDiff      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(SumOrDiffTerm      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(ProdOrQuot      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(PlusOrMinus::Value      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(TimesOrDividedBy::Value      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);
        // Load into the specified 'output' a value parsed from the specified
        // 'input'. If an error occurs, write diagnostics to the specified
        // 'errorStream'. Use the optionally specified 'lexer' to read tokens.
        // If 'lexer' is zero, then a temporary 'Lexer' instance will be
        // used instead. Return zero on success or a nonzero value if an error
        // occurs.
};

}  // close package namespace
}  // close enterprise namespace

#endif