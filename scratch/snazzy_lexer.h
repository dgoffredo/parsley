#ifndef INCLUDED_SNAZZY_LEXER
#define INCLUDED_SNAZZY_LEXER

#include <bdlb_literalutil.h>

#include <bdlpcre_regex.h>

#include <bslma_usesbslmaallocator.h>

#include <bslmf_nestedtraitdeclaration.h>

#include <bsl_cstddef.h>
#include <bsl_iosfwd.h>
#include <bsl_string.h>
#include <bsl_utility.h>
#include <bsl_vector.h>

namespace BloombergLP {
namespace bslma { class Allocator; }
namespace snazzy {

                            // =================
                            // struct LexerToken
                            // =================

struct LexerToken {
    // This 'struct' represents a lexer token parsed from some input. Each
    // 'LexerToken' instance has a "kind," which describes the category of
    // token, and a "value," which is the relevant portion of the input
    // corresponding to the token.

    // TYPES
    enum Kind { e_TOKEN_6, e_TOKEN_5, e_TOKEN_4, e_TOKEN_3, e_TOKEN_2, e_TOKEN_1, e_NUMBER, e_IDENTIFIER };

    // PUBLIC DATA
    Kind                   d_kind;
    bslstl::StringRef d_value;

    // CREATORS
    LexerToken(Kind kind, const bslstl::StringRef& value);
        // Create a 'LexerToken' object having the specified 'kind' and the
        // specified 'value'.
};

// FREE FUNCTIONS
bsl::ostream& operator<<(bsl::ostream& stream, LexerToken::Kind tokenKind);
    // Insert into the specified 'stream' a description of the specified
    // 'tokenKind'. Return a reference providing modifiable access to 'stream'.
    // Note that the output format is meant for human-readable diagnostics
    // only, and so is subject to change.

                        // =============================
                        // struct Lexer_SubpatternRecord
                        // =============================

struct Lexer_SubpatternRecord {
    // This component-private 'class' represents a particular token "kind" and
    // the corresponding index into a 'Lexer' subpattern matches vector
    // where the match of that token, if any, can be found.

    // PUBLIC DATA
    bsl::size_t          d_index;
    LexerToken::Kind d_kind;

    // CREATORS
    Lexer_SubpatternRecord(bsl::size_t valueIndex, LexerToken::Kind kind);
        // Create a 'Lexer_SubpatternRecord' object having the specified
        // 'valueIndex' and the specified 'kind'.
};

                              // ===========
                              // class Lexer
                              // ===========

class Lexer {
    // This 'class' defines a function-like object that divides a specified
    // input string into a sequence of tokens. Although the function is
    // stateless, separate invocations of the same object share data structures
    // in order to reduce the overhead of regular expression compilation and
    // memory allocation.

    // DATA
    bdlpcre::RegEx                               d_regex;
    bsl::vector<bsl::pair<bsl::size_t, bsl::size_t> > d_matches;
    bsl::vector<Lexer_SubpatternRecord>           d_subpatterns;  // const

  public:
    // TRAITS
    BSLMF_NESTED_TRAIT_DECLARATION(Lexer, bslma::UsesBslmaAllocator)

    // CREATORS
    explicit Lexer(bslma::Allocator *basicAllocator = 0);
        // Create a new 'Lexer' object. Use the optionally specified
        // 'basicAllocator' to supply memory. If 'basicAllocator' is zero, the
        // currently installed default allocator will be used instead.

    // MANIPULATORS
    int operator()(bsl::vector<LexerToken>   *output,
                   const bslstl::StringRef&  input,
                   bsl::ostream&                  errorStream);
        // Load into the specified 'output' tokens read from the specified
        // 'input'. If an error occurs, print a diagnostic to the specified
        // 'errorStream'. Return zero on success or a nonzero value if an error
        // occurs. Note that this function may be called multiple times.
};

}  // close package namespace
}  // close enterprise namespace

#endif
