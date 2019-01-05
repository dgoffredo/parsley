#ifndef INCLUDED_PACKAGE_PARSERUTIL
#define INCLUDED_PACKAGE_PARSERUTIL

#include <bsl_iosfwd.h>
#include <bsl_string.h>

namespace Enterprise {
namespace package {
class Bar;
class Foo;
class Lexer;

struct ParserUtil {
    // This 'struct' provides a namespace of functions used to parse instances
    // of generated types from text.

    static int parse(Foo                      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&             errorStream,
                     Lexer                    *lexer = 0);

    static int parse(Bar                      *output,
                     const bslstl::StringRef&  input,
                     bsl::ostream&             errorStream,
                     Lexer                    *lexer = 0);
        // Load into the specified 'output' a value parsed from the specified
        // 'input'. If an error occurs, write diagnostics to the specified
        // 'errorStream'. Use the optionally specified 'lexer' to read tokens.
        // If 'lexer' is zero, then a temporary 'Lexer' instance will be used
        // instead. Return zero on success or a nonzero value if an error
        // occurs.
};

}  // close package namespace
}  // close enterprise namespace

#endif
