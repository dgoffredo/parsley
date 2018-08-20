#include <package_lexer.h>

#include <bdlb_arrayutil.h>

#include <bsl_iostream.h>
#include <bsl_ostream.h>

namespace BloombergLP {
namespace package {
namespace {

void prepare(bdlpcre::RegEx *regex)
{
    BSLS_ASSERT(regex);

    bsl::string error;
    bsl::size_t errorOffset;

    static const char k_PATTERN[] = "(?P<STRING>...)|(blah...)|(blah...)";
    static const int  k_FLAGS     = bdlpcre::RegEx::k_FLAG_UTF8;

    const int rc = 
        regex->prepare(&errorMessage, &errorOffset, k_PATTERN, k_FLAGS);

    if (rc) {
        bsl::cerr << "bdlpcre::RegEx::prepare returned error code " << rc
                  << " with the message: " << error
                  << "\nError occurred at offset " << errorOffset
                  << " of the pattern: " << k_PATTERN << '\n';
    }

    BSLS_ASSERT(rc == 0);
}

void enumerationSubpatterns(bsl::vector<Lexer_SubpatternRecord> *subpatterns,
                            const bdlpcre::RegEx&                regex)
{ 
    BSLS_ASSERT(subpatterns);

    struct Subpattern {
        const char       *d_name;
        const char       *d_valueName;
        LexerToken::Kind  d_kind;
    };

    static const Subpattern k_SUBPATTERNS[] = {
        {"STRING", "STRING_VALUE", LexerToken::e_STRING},
        {"LPAREN", "LPAREN_VALUE", LexerToken::e_FOO},
        {"thing",  "thingValue",   LexerToken::e_BAR}
    };

    static const Subpattern *const k_END = bdlb::ArrayUtil::end(k_SUBPATTERNS);

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
    const bsl::vector<bsl::pair<bsl::size_t, bsl::size_t> > *d_matches;

  public:
    // CREATORS
    explicit IsMatch(
        const bsl::vector<bsl::pair<bsl::size_t, bsl::size_t> >& matches)
    : d_matches(&matches)
    {
    }

    // MANIPULATORS
    bool operator()(const Lexer_SubpatternRecord& subpattern) const
    {
        BSLS_ASSERT(d_matches);
        BSLS_ASSERT(subpattern.d_index < d_matches->size());

        return d_matches[subpattern.d_index].first != bsl::size_t(-1);
    }
};

}  // close unnamed namespace

                            // ----------------
                            // class LexerToken
                            // ----------------

// CREATORS
LexerToken::LexerToken(LexerToken::Kind kind, const bslstl::StringRef& value)
: d_kind(kind)
, d_value(value)
{
}

                        // ----------------------------
                        // class Lexer_SubpatternRecord
                        // ----------------------------

// CREATORS
Lexer_SubpatternRecord::Lexer_SubpatternRecord(bsl::size_t      valueIndex,
                                               LexerToken::Kind kind)
: d_valueIndex(valueIndex)
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
, d_generation(0)
{
    prepare(&d_regex);
    enumerateSubpatterns(&d_subpatterns);
}

// MANIPULATORS
int Lexer::operator()(bsl::vector<Token>       *output,
                      const bslstl::StringRef&  input,
                      bsl::ostream&             errorStream)
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
            errorStream << "Depth limit exceeded.\n";
            // fall through
          default:
            errorStream << "Failed to match the text: " << input 
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
        const bsl::vector<Lexer_SubpatternRecord::const_iterator found =
            bsl::find_if(d_subpatterns.begin(),
                         d_subpatterns.end(), 
                         IsMatch(d_matches));

        BSLS_ASSERT(found != d_subpatterns.end());

        const bsl::pair<bsl::size_t, bsl::size_t>& match =
            d_matches[found->d_index];

        output->emplace_back(found->kind,
                             bslstl::StringRef(input.data() + match.first,
                                               match.second));
    }

    return 0;
}

}  // close package namespace
}  // close enterprise namespace
