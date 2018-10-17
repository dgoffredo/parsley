#include <shibby_lexer.h>

#include <bdlb_arrayutil.h>

#include <bsl_algorithm.h>
#include <bsl_iostream.h>
#include <bsl_ostream.h>

namespace BloombergLP {
namespace shibby {
namespace {

struct Subpattern {
    const char           *d_name;
    const char           *d_valueName;
    const char           *d_pattern;
    LexerToken::Kind  d_kind;
};

const Subpattern k_SUBPATTERNS[] = {
    {"TOKEN_8", "TOKEN_8_VALUE", "\\]", LexerToken::e_TOKEN_8},
    {"TOKEN_7", "TOKEN_7_VALUE", "\\[", LexerToken::e_TOKEN_7},
    {"TOKEN_6", "TOKEN_6_VALUE", "\\)", LexerToken::e_TOKEN_6},
    {"TOKEN_5", "TOKEN_5_VALUE", "\\(", LexerToken::e_TOKEN_5},
    {"TOKEN_4", "TOKEN_4_VALUE", "\\*", LexerToken::e_TOKEN_4},
    {"TOKEN_3", "TOKEN_3_VALUE", "/", LexerToken::e_TOKEN_3},
    {"TOKEN_2", "TOKEN_2_VALUE", "//", LexerToken::e_TOKEN_2},
    {"TOKEN_1", "TOKEN_1_VALUE", ":", LexerToken::e_TOKEN_1},
    {"IDENTIFIER", "IDENTIFIER_VALUE", "[a-zA-Z_][0-9a-zA-Z_]*", LexerToken::e_IDENTIFIER},
    {"INTEGER", "INTEGER_VALUE", "0|[1-9][0-9]*", LexerToken::e_INTEGER},
    {"STRING", "STRING_VALUE", "\"(?P<STRING_VALUE>([^\\\\]|\\\\.)*)\"", LexerToken::e_STRING},
    {"SEPARATOR", "SEPARATOR_VALUE", "\\s*,\\s*", LexerToken::e_SEPARATOR}
};

const Subpattern *const k_END = bdlb::ArrayUtil::end(k_SUBPATTERNS);

void addSubpattern(bsl::string *output, const Subpattern& subpattern)
{
    BSLS_ASSERT(output);

    *output += "(?P<";  // python-style named subpattern
    *output += subpattern.d_name;
    *output += '>';
    *output += subpattern.d_pattern;
    *output += ')';
}

const Subpattern& subpatternByKind(LexerToken::Kind tokenKind)
{
    const int index = tokenKind;
    BSLS_ASSERT(index >= 0);
    BSLS_ASSERT(index < bdlb::ArrayUtil::size(k_SUBPATTERNS));

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

void prepare(bdlpcre::RegEx *regex)
{
    BSLS_ASSERT(regex);

    bsl::string error;
    bsl::size_t errorOffset;

    const bsl::string pattern = getPattern();
    const int flags           = bdlpcre::RegEx::k_FLAG_UTF8;

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

void enumerateSubpatterns(bsl::vector<Lexer_SubpatternRecord> *subpatterns,
                          const bdlpcre::RegEx&               regex)
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
    bool operator()(const Lexer_SubpatternRecord& subpattern) const
    {
        BSLS_ASSERT(d_matches_p);
        BSLS_ASSERT(subpattern.d_index < d_matches_p->size());

        return (*d_matches_p)[subpattern.d_index].first != bsl::size_t(-1);
    }
};

bsl::string quoted(const bslstl::StringRef& input)
{
    bsl::string result;
    bdlb::LiteralUtil::createQuotedEscapedCString(&result, input);
    return result;
}

}  // close unnamed namespace

                            // -----------------
                            // struct LexerToken
                            // -----------------

// CREATORS
LexerToken::LexerToken(LexerToken::Kind              kind,
                             const bslstl::StringRef& value)
: d_kind(kind)
, d_value(value)
{
}

// FREE FUNCTIONS
bsl::ostream& operator<<(bsl::ostream& stream, LexerToken::Kind tokenKind)
{
    const Subpattern& subpattern = subpatternByKind(tokenKind);

    // e.g. <DIGITS /[1-9][0-9]+/>
    return stream << '<' << subpattern.d_name << " /" << subpattern.d_pattern
                  << "/>";
}

                        // -----------------
                        // struct LexerToken
                        // -----------------

// CREATORS
Lexer_SubpatternRecord::Lexer_SubpatternRecord(
    bsl::size_t      valueIndex,
    LexerToken::Kind kind)
: d_index(valueIndex)
, d_kind(kind)
{
}

                              // -----------
                              // class Lexer
                              // -----------

// CREATORS
Lexer::Lexer(bslma::Allocator *basicAllocator)
: d_regex(basicAllocator)
, d_matches(basicAllocator)
, d_subpatterns(basicAllocator)
{
    prepare(&d_regex);
    enumerateSubpatterns(&d_subpatterns, d_regex);
}

// MANIPULATORS
int Lexer::operator()(bsl::vector<LexerToken>   *output,
                              const bslstl::StringRef&  input,
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
                        << quoted(bslstl::StringRef(
                                      input.begin() + offset, input.end()))
                        << "\nagainst the pattern: " << d_regex.pattern()
                        << "\nerror code: " << rc;
            return rc;  // match failure
        }

        BSLS_ASSERT(!d_matches.empty());

        // Update the offset from the subject, for the next go-round.
        const bsl::pair<bsl::size_t, bsl::size_t>& totalMatch =
            d_matches.front();

        offset = totalMatch.first + totalMatch.second;

        // Find which subpattern matched.
        const bsl::vector<Lexer_SubpatternRecord>::const_iterator found =
            bsl::find_if(d_subpatterns.begin(),
                         d_subpatterns.end(),
                         IsMatch(d_matches));

        BSLS_ASSERT(found != d_subpatterns.end());

        const bsl::pair<bsl::size_t, bsl::size_t>& match =
            d_matches[found->d_index];

        output->emplace_back(found->d_kind,
                             bslstl::StringRef(input.data() + match.first,
                                                    match.second));
    }

    return 0;
}

}  // close package namespace
}  // close enterprise namespace
