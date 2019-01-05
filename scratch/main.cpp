#include <shibby_lexer.h>

#include <bdlb_stringrefutil.h>

#include <bsl_iostream.h>
#include <bsl_sstream.h>
#include <bsl_string.h>
#include <bsl_vector.h>

using namespace BloombergLP;

int main()
{
    bsl::ostringstream oss;
    oss << bsl::cin.rdbuf();
    const bsl::string input(oss.str());

    bsl::vector<shibby::LexerToken> tokens;
    shibby::Lexer lexer;
    const int rc = lexer(&tokens, bdlb::StringRefUtil::trim(input), bsl::cerr);

    if (rc) {
        return rc;
    }

    for (const auto& token : tokens) {
        bsl::cout << "kind: " << token.d_kind
                  << " value: " << token.d_value << '\n';
    }
}
