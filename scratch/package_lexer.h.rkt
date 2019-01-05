#lang at-exp racket

(provide lexer-header)

; This is the wrong version of the header to templatify, but I was just playing
; with the reader anyway.

(require threading)

(define (lexer-header std enterprise-namespace names)
  @~a{
#ifndef INCLUDED_LAZY_LEXER
#define INCLUDED_LAZY_LEXER

namespace @enterprise-namespace {
namespace lazy {

                            // ================
                            // class LexerToken
                            // ================

struct LexerToken {
    // This 'struct' represents a lexer token parsed from some input. Each
    // 'LexerToken' instance has a "kind," which describes the category of
    // token, and a "value," which is the relevant portion of the input
    // corresponding to the token.

    // TYPES
    enum Kind { @(~>> names
                   (map ~a)
                   (map (lambda (name) (~a "e_" (string-upcase name))))
                   (string-join _ ", ")) };

    // PUBLIC DATA
    Kind        d_kind;
    @|std|string d_value;
};

                        // ============================
                        // class Lexer_SubpatternRecord
                        // ============================

struct Lexer_SubpatternRecord {
    // This component-private 'class' represents a particular token "kind" and
    // the corresponding index into a 'Lexer' subpattern matches vector where
    // the match of that token, if any, can be found.

    // PUBLIC DATA
    @|std|size_t      d_valueIndex;
    LexerToken::Kind d_kind;

    // CREATORS
    Lexer_SubpatternRecord(@|std|size_t valueIndex, LexerToken::Kind kind);
        // Create a 'Lexer_SubpatternRecord' object having the specified
        // 'valueIndex' and the specified 'kind'.
};

                            // ===========
                            // class Lexer
                            // ===========

class Lexer {
    // TODO

    // DATA
    @|std|streambuf                                    *d_streambuf_p;
    bdlpcre::RegEx                                     d_regex;
    @|std|vector<bsl::pair<bsl::size_t, bsl::size_t> >  d_matches;
    @|std|vector<Lexer_SubpatternRecord>                d_subpatterns;  // const

  public:
    // TRAITS
    BSLMF_NESTED_TRAIT_DECLARATION(Lexer, bslma::UsesBslmaAllocator)

    // CREATORS
    explicit Lexer(@|std|streambuf   *streambuf,
                   bslma::Allocator *basicAllocator = 0);
        // Create a new 'Lexer' object that reads token from the specified
        // 'streambuf'. Use the optionally specified 'basicAllocator' to supply
        // memory. If 'basicAllocator' is zero, the currently installed default
        // allocator will be used instead.

    // MANIPULATORS
    int next(LexerToken *token, @|std|ostream *errorStream = 0);
        // Load into the specified 'token' the next token read from the
        // streambuf held by this object. Return zero on success or a nonzero
        // value if a token could not be read. Return '-1' if the streambuf
        // end of file was reached before any input could be read. If an error
        // occurs and the optionally specified 'errorStream' is nonzero, write
        // a diagnostic to 'errorStream'.

    void reset(@|std|streambuf *streambuf);
        // Use the specified 'streambuf' to supply characters in subsequent
        // calls to 'next'.
};

}  // close enterprise namespace
}  // close package namespace
})