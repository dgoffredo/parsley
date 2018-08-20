#ifndef INCLUDED_LAZY_TOKENITERATOR
#define INCLUDED_LAZY_TOKENITERATOR

#include <lazy_lexer.h>

#include <bsl_cstddef.h>
#include <bsl_iterator.h>
#include <bsl_memory.h>

namespace Enterprise {
namespace lazy {

struct TokenIterator_Cell {
    LexerToken                          d_token;
    bsl::shared_ptr<TokenIterator_Cell> d_next;
};

class TokenIterator {
    Lexer                               *d_lexer_p;
    bsl::shared_ptr<TokenIterator_Cell>  d_cell_sp;

  public:
    // TRAITS
    typedef bsl::forward_iterator_tag  iterator_category;
    typedef LexerToken                 value_type;
    typedef bsl::ptrdiff_t             difference_type;
    typedef const LexerToken          *pointer;
    typedef const LexerToken&          reference;

    // CREATORS
    TokenIterator();
        // Create a 'TokenIterator' object referring to the end of any sequence
        // of token read by any 'Lexer' instance.

    TokenIterator(Lexer *lexer);
        // Create a 'TokenIterator' object referring to the beginning of the
        // sequence of tokens to be read by the specified 'lexer'. The behavior
        // is undefined if 'next' has been called on 'lexer'.

    // MANIPULATORS
    TokenIterator& operator++();
        // TODO

    TokenIterator operator++(int):
        // TODO

    reference operator*() const;
        // TODO
};

// FREE OPERATORS
bool operator==(const TokenIterator& left, const TokenIterator& right);
    // TODO

bool operator!=(const TokenIterator& left, const TokenIterator& right);
    // TODO

}  // close package namespace
}  // close enterprise namespace

#endif
