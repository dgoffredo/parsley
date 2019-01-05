#ifndef INCLUDED_SNAZZY_PARSER
#define INCLUDED_SNAZZY_PARSER

#include <bsl_iosfwd.h>
#include <bsl_string.h>

namespace BloombergLP {
namespace snazzy {
class Alternation;
class BoundPatternTerm;
class Concatenation;
class Grammar;
class Lexer;
class Pattern;
class PatternTerm;
class QuantifiedPatternTerm;
class Rule;
class UnquantifiedPatternTerm;

struct Parser {
    // This 'struct' provides a namespace of functions used to parse instances
    // of generated types from text.

    static int parse(Grammar                 *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(Rule                 *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(Pattern                 *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(Alternation                 *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(Concatenation                 *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(PatternTerm                 *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(BoundPatternTerm                 *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(QuantifiedPatternTerm                 *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&                  errorStream,
                     Lexer                  *lexer = 0);

    static int parse(UnquantifiedPatternTerm                 *output,
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