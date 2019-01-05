#lang at-exp racket

(provide generate-lexer lexer-header lexer-source)

(require "names.rkt"        ; e.g. "fooBar" -> "e_FOO_BAR"
         "types.rkt"        ; struct token
         "codegen-util.rkt" ; string-join* etc.
         threading)         ; ~> and similar macros

(define (generate-lexer tokens
                        package-name
                        [class-name "Lexer"]
                        [enterprise-namespace "BloombergLP"]
                        [output-directory (current-directory)])
  "Write a C++ source and header file to the optionally specified
   @var{output-directory}. The two resulting files define a component that
   provides a lexer that scans text for the specified @var{tokens}."
  (let* ([component (~a package-name "_" (string-downcase class-name))]
         [header (build-path output-directory (~a component ".h"))]
         [source (build-path output-directory (~a component ".cpp"))])
    (call-with-output-file* header #:exists 'truncate
      (lambda (out)
        (displayln (lexer-header (map token-name tokens)
                                 enterprise-namespace
                                 package-name
                                 class-name)
                    out)))
    (call-with-output-file* source #:exists 'truncate
      (lambda (out)
        (displayln (lexer-source tokens
                                 enterprise-namespace
                                 package-name 
                                 class-name)
                    out)))))

(define (lexer-header enum-value-names
                      enterprise-namespace
                      package-name
                      class-name)
  "Return a string containing C++ source code for a header file definining a
   lexer."
  (let* ([ns (calculate-bloomberg-prefix enterprise-namespace)]
         [guard-macro (~>> (list "INCLUDED" package-name class-name)
                           (map string-upcase)
                           (string-join* "_"))]
         [token-class (~a class-name "Token")]
         [token-decl (~a "struct " token-class)]
         [token-border (make-string (string-length token-decl) #\=)]
         [subpattern-record-class (~a class-name "_SubpatternRecord")]
         [subpattern-record-decl (~a "struct " subpattern-record-class)]
         [subpattern-record-border
          (make-string (string-length subpattern-record-decl) #\=)]
         [class-decl (~a "class " class-name)]
         [class-border (make-string (string-length  class-decl) #\=)])
    @~a{
#ifndef @guard-macro
#define @guard-macro

#include <bdlb_literalutil.h>

#include <bdlpcre_regex.h>

#include <bslma_usesbslmaallocator.h>

#include <bslmf_nestedtraitdeclaration.h>

#include <bsl_cstddef.h>
#include <bsl_iosfwd.h>
#include <bsl_string.h>
#include <bsl_utility.h>
#include <bsl_vector.h>
@(if (not (equal? enterprise-namespace "BloombergLP"))
    ; If the enterprise namespace is different from BloombergLP, then the
    ; forward declaration of BloombergLP::bslma::Allocator has to be outside.
    (~a "
namespace BloombergLP {
namespace bslma { class Allocator; }
}")
    "")
namespace @enterprise-namespace {
@(if (equal? enterprise-namespace "BloombergLP")
    ; If the enterprise namespace is BloombergLP, then the forward declaration
    ; for bslma::Allocator is inside.
    "namespace bslma { class Allocator; }\n"
    "")namespace @package-name {

                            // @token-border
                            // @token-decl
                            // @token-border

@token-decl {
    // This 'struct' represents a lexer token parsed from some input. Each
    // '@token-class' instance has a "kind," which describes the category of
    // token, and a "value," which is the relevant portion of the input
    // corresponding to the token.

    // TYPES
    enum Kind { @(~>> enum-value-names
                   (map bde-enum-value-case)
                   (string-join* ", ")) };

    // PUBLIC DATA
    Kind                   d_kind;
    @|ns|bslstl::StringRef d_value;

    // CREATORS
    @token-class(Kind kind, const @|ns|bslstl::StringRef& value);
        // Create a '@token-class' object having the specified 'kind' and the
        // specified 'value'.
};

// FREE FUNCTIONS
bsl::ostream& operator<<(bsl::ostream& stream, @|token-class|::Kind tokenKind);
    // Insert into the specified 'stream' a description of the specified
    // 'tokenKind'. Return a reference providing modifiable access to 'stream'.
    // Note that the output format is meant for human-readable diagnostics
    // only, and so is subject to change.

                        // @subpattern-record-border
                        // @subpattern-record-decl
                        // @subpattern-record-border

@subpattern-record-decl {
    // This component-private 'class' represents a particular token "kind" and
    // the corresponding index into a '@class-name' subpattern matches vector
    // where the match of that token, if any, can be found.

    // PUBLIC DATA
    bsl::size_t          d_index;
    @|token-class|::Kind d_kind;

    // CREATORS
    @subpattern-record-class(bsl::size_t valueIndex, LexerToken::Kind kind);
        // Create a '@subpattern-record-class' object having the specified
        // 'valueIndex' and the specified 'kind'.
};

                              // @class-border
                              // @class-decl
                              // @class-border

class @class-name {
    // This 'class' defines a function-like object that divides a specified
    // input string into a sequence of tokens. Although the function is
    // stateless, separate invocations of the same object share data structures
    // in order to reduce the overhead of regular expression compilation and
    // memory allocation.

    // DATA
    @|ns|bdlpcre::RegEx                               d_regex;
    bsl::vector<bsl::pair<bsl::size_t, bsl::size_t> > d_matches;
    bsl::vector<@|subpattern-record-class|>           d_subpatterns;  // const

  public:
    // TRAITS
    BSLMF_NESTED_TRAIT_DECLARATION(@class-name, @|ns|bslma::UsesBslmaAllocator)

    // CREATORS
    explicit @class-name(@|ns|bslma::Allocator *basicAllocator = 0);
        // Create a new '@class-name' object. Use the optionally specified
        // 'basicAllocator' to supply memory. If 'basicAllocator' is zero, the
        // currently installed default allocator will be used instead.

    // MANIPULATORS
    int operator()(bsl::vector<@|token-class|>   *output,
                   const @|ns|bslstl::StringRef&  input,
                   bsl::ostream&                  errorStream);
        // Load into the specified 'output' tokens read from the specified
        // 'input'. If an error occurs, print a diagnostic to the specified
        // 'errorStream'. Return zero on success or a nonzero value if an error
        // occurs. Note that this function may be called multiple times.
};

}  // close package namespace
}  // close enterprise namespace

#endif
}))

(define (name->value-name token-name)
  "Return the name of the regex pattern subgroup of the \"inner value\" of the
   token with the specified @var{token-name}. The idea is that a token named
   FOO could have within its regex pattern a subgroup named FOO_VALUE, whose
   contents would be extracted as the value of the token instead of the full
   match for FOO. Similarly, fooValue for foo."
  (if (all-capitals? (~a token-name))
    (~a token-name "_VALUE")
    (~a token-name "Value")))

(define (regex-pattern tokens)
  "Return a string containing a Perl-compatible regular expression that
   matches any of the specified @var{tokens}, where each token pattern is a
   named capture group within the total pattern, where the names are the
   same as the token names."
  (define (subpattern tok)
    (match tok
      [(token name pattern _ _)
       (~a "(?P<" name ">" pattern ")")]))

  (~a "(?:" (~>> tokens (map subpattern) (string-join* "|")) ")"))

(define (lexer-source tokens
                      enterprise-namespace
                      package-name
                      class-name)
  "Return a string containing C++ source code for a source file implementing
   a lexer."
  (let* ([ns (if (equal? enterprise-namespace "BloombergLP")
               "" 
               "BloombergLP::")]
         [header (~a package-name "_" (string-downcase class-name) ".h")]
         [token-class (~a class-name "Token")]
         [token-decl (~a "struct " token-class)]
         [token-border (make-string (string-length token-decl) #\-)]
         [subpattern-record-class (~a class-name "_SubpatternRecord")]
         [subpattern-record-decl (~a "struct " token-class)]
         [subpattern-record-border
          (make-string (string-length subpattern-record-decl) #\-)]
         [class-decl (~a "class " class-name)]
         [class-border (make-string (string-length  class-decl) #\-)])
    @~a{
#include <@|header|>

#include <bdlb_arrayutil.h>

#include <bsl_algorithm.h>
#include <bsl_iostream.h>
#include <bsl_ostream.h>

namespace @enterprise-namespace {
namespace @package-name {
namespace {

struct Subpattern {
    const char           *d_name;
    const char           *d_valueName;
    const char           *d_pattern;
    @|token-class|::Kind  d_kind;
};

const Subpattern k_SUBPATTERNS[] = {
    @(~>> tokens
        ; e.g. {"LPAREN", "LPAREN_VALUE", "\\(", LexerToken::e_FOO},
        (map (match-lambda [(token name pattern _ _)
                (~a "{" (~s (~a name)) ", "
                (~s (name->value-name name)) ", "
                (~s pattern) ", "
                token-class "::" (bde-enum-value-case name)
                "}")]))
        (string-join* ",\n    "))
};

const Subpattern *const k_END = @|ns|bdlb::ArrayUtil::end(k_SUBPATTERNS);

void addSubpattern(bsl::string *output, const Subpattern& subpattern)
{
    BSLS_ASSERT(output);

    *output += "(?P<";  // python-style named subpattern
    *output += subpattern.d_name;
    *output += '>';
    *output += subpattern.d_pattern;
    *output += ')';
}

const Subpattern& subpatternByKind(@|token-class|::Kind tokenKind)
{
    const int index = tokenKind;
    BSLS_ASSERT(index >= 0);
    BSLS_ASSERT(index < @|ns|bdlb::ArrayUtil::size(k_SUBPATTERNS));

    const Subpattern& subpattern = k_SUBPATTERNS[index];
    BSLS_ASSERT(subpattern.d_kind == tokenKind);
    return subpattern;
}

bsl::string getPattern()
    // Return a bdlpcre-compatible regular expression pattern that is an
    // alternation among the patterns of the tokens within the grammar. Each
    // token pattern is a named subpattern.
{
    bsl::string result("(?:");  // Don't capture the alternation itself
    const Subpattern *it = k_SUBPATTERNS;
    
    BSLS_ASSERT(it != k_END);
    addSubpattern(&result, *it);

    for (++it; it != k_END; ++it)
    {
        result += '|';
        addSubpattern(&result, *it);
    }    

    result += ')';
    return result;
}

void prepare(@|ns|bdlpcre::RegEx *regex)
{
    BSLS_ASSERT(regex);

    bsl::string error;
    bsl::size_t errorOffset;

    const bsl::string pattern = getPattern();
    const int flags           = @|ns|bdlpcre::RegEx::k_FLAG_UTF8;

    const int rc = 
        regex->prepare(&error, &errorOffset, pattern.c_str(), flags);

    if (rc) {
        bsl::cerr << "Fatal programming error: bdlpcre::RegEx::prepare "
                     "returned error code " << rc << " with the message: "
                  << error << "\nError occurred at offset " << errorOffset
                  << " of the pattern: " << pattern << bsl::endl;
    }

    BSLS_ASSERT_OPT(rc == 0);
}

void enumerateSubpatterns(bsl::vector<@|subpattern-record-class|> *subpatterns,
                          const @|ns|bdlpcre::RegEx&               regex)
{ 
    BSLS_ASSERT(subpatterns);
    BSLS_ASSERT(regex.isPrepared());

    for (const Subpattern *iter = k_SUBPATTERNS; iter != k_END; ++iter)
    {
        const Subpattern& subpattern = *iter;
        const int nameIndex = regex.subpatternIndex(subpattern.d_name);
        BSLS_ASSERT(nameIndex > 0);
        
        const int valueIndex = regex.subpatternIndex(subpattern.d_valueName);
        BSLS_ASSERT(valueIndex == -1 || valueIndex > 0);

        subpatterns->emplace_back(valueIndex == -1 ? nameIndex : valueIndex,
                                  subpattern.d_kind);
    }
}

class IsMatch {
    // This 'class' defines a function-like object intended to be used as a
    // predicate in 'bsl::find_if'. It returns whether a specified subpattern
    // record is matched in a bound vector of subpattern match intervals.

    // DATA
    const bsl::vector<bsl::pair<bsl::size_t, bsl::size_t> > *d_matches_p;

  public:
    // CREATORS
    explicit IsMatch(
        const bsl::vector<bsl::pair<bsl::size_t, bsl::size_t> >& matches)
    : d_matches_p(&matches)
    {
    }

    // MANIPULATORS
    bool operator()(const @|subpattern-record-class|& subpattern) const
    {
        BSLS_ASSERT(d_matches_p);
        BSLS_ASSERT(subpattern.d_index < d_matches_p->size());

        return (*d_matches_p)[subpattern.d_index].first != bsl::size_t(-1);
    }
};

bsl::string quoted(const bslstl::StringRef& input)
{
    bsl::string result;
    @|ns|bdlb::LiteralUtil::createQuotedEscapedCString(&result, input);
    return result;
}

}  // close unnamed namespace

                            // @token-border
                            // @token-decl
                            // @token-border

// CREATORS
@|token-class|::@token-class(LexerToken::Kind              kind,
                             const @|ns|bslstl::StringRef& value)
: d_kind(kind)
, d_value(value)
{
}

// FREE FUNCTIONS
bsl::ostream& operator<<(bsl::ostream& stream, @|token-class|::Kind tokenKind)
{
    const Subpattern& subpattern = subpatternByKind(tokenKind);

    // e.g. <DIGITS /[1-9][0-9]+/>
    return stream << '<' << subpattern.d_name << " /" << subpattern.d_pattern
                  << "/>";
}

                        // @subpattern-record-border
                        // @subpattern-record-decl
                        // @subpattern-record-border

// CREATORS
@|subpattern-record-class|::@subpattern-record-class(
    bsl::size_t      valueIndex,
    LexerToken::Kind kind)
: d_index(valueIndex)
, d_kind(kind)
{
}

                              // @class-border
                              // @class-decl
                              // @class-border

// CREATORS
@|class-name|::@class-name(@|ns|bslma::Allocator *basicAllocator)
: d_regex(basicAllocator)
, d_matches(basicAllocator)
, d_subpatterns(basicAllocator)
{
    prepare(&d_regex);
    enumerateSubpatterns(&d_subpatterns, d_regex);
}

// MANIPULATORS
int @|class-name|::operator()(bsl::vector<@|token-class|>   *output,
                              const @|ns|bslstl::StringRef&  input,
                              bsl::ostream&                  errorStream)
{
    BSLS_ASSERT(output);

    output->clear();

    for (bsl::size_t offset = 0; offset < input.size();)
    {
        const int rc =
            d_regex.match(&d_matches, input.data(), input.size(), offset);

        switch (rc) {
          case 0:
            break;  // match successful
          case 1:
            errorStream << "Regular expression depth limit exceeded.\n";
            // fall through
          default:
            errorStream << "Failed to match the text: "
                        << quoted(@|ns|bslstl::StringRef(
                                      input.begin() + offset, input.end()))
                        << "\nagainst the pattern: " << d_regex.pattern()
                        << "\nerror code: " << rc << '\n';
            return rc;  // match failure
        }

        BSLS_ASSERT(!d_matches.empty());

        // Update the offset from the subject, for the next go-round.
        const bsl::pair<bsl::size_t, bsl::size_t>& totalMatch = 
            d_matches.front();

        offset = totalMatch.first + totalMatch.second;

        // Find which subpattern matched.
        const bsl::vector<@|subpattern-record-class|>::const_iterator found =
            bsl::find_if(d_subpatterns.begin(),
                         d_subpatterns.end(), 
                         IsMatch(d_matches));

        BSLS_ASSERT(found != d_subpatterns.end());

        const bsl::pair<bsl::size_t, bsl::size_t>& match =
            d_matches[found->d_index];

        output->emplace_back(found->d_kind,
                             @|ns|bslstl::StringRef(input.data() + match.first,
                                                    match.second));
    }

    return 0;
}

}  // close package namespace
}  // close enterprise namespace
}))
