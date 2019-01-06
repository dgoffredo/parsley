#lang at-exp racket

(provide (all-defined-out)) ; TODO debugging

(require "productions.rkt"
         "types.rkt"
         "names.rkt"
         "codegen-util.rkt"
         "applicable-dict.rkt"
         "debug.rkt"
         "parser-functions.rkt"
         threading
         srfi/1)  ; list procedures, e.g. `any`

(define (generate-parser . TODO)
  'TODO)

(define (reader-function-declare function #:semicolon? [semicolon? #t])
  (match function
    [(reader-function name output-type _)
     @~a{int @name(@(if output-type ; output parameter if output-type is not #f
                        (~a output-type " *output,") 
                        "")
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors)@(if semicolon? ";" "")}]))

(define (reader-function-define function)
  (match function
    [(reader-function name output-type reader)
     @~a{@(reader-function-declare function #:semicolon? #f)
{
    // TODO: code here
}
}]))

(define (declare-parse-function output-class lexer-class ns)
  (let ([margin (make-string 4 #\space)])
    @~a{@""
@|margin|static int parse(@output-class                 *output,
@|margin|                 const @|ns|bslstl::StringRef&  input,
@|margin|                 bsl::ostream&                  errorStream,
@|margin|                 @lexer-class                  *lexer = 0);}))

(define (token-ignore-definition tokens token-class)
    @~a{bool ignore(@|token-class|::Kind tokenKind)
    // Return whether the specified 'tokenkind' ought to be ignored by the 
    // parser.
{
    switch (tokenKind) {
@(let ([case-line (lambda (enum)
                    (~a "      case " token-class "::" enum 
                      ": return true;"))])
   (~>> tokens 
     (filter token-ignore?) 
     (map token-name) 
     (map bde-enum-value-case)
     (map case-line)
     (string-join* "\n")))
      default:
        return false;
    }
}})

(define (numeric-read-declaration cpp-type token-class)
  @~a{int read(@cpp-type            *output,
               @|token-class|::Kind  tokenKind,
               TokenIter            *token,
               TokenIter             endTokens,
               bsl::ostream         *errors);})

(define numeric-read-contract
  "
    // If the specified 'token' refers to a token having the specified
    // 'tokenKind', then increment the token referred to by 'token', and if
    // additionally 'output' is not zero, convert the matching token into the
    // appropriate type and load the resulting value into 'output'. Return zero
    // on success or a nonzero value if: 'token' does not refer to a token of
    // kind 'tokenKind', 'token' is equal to the specified 'endTokens', or the
    // token is not convertible to the type of 'output'. On failure, if the
    // specified 'errors' is not zero, write a diagnostic to 'errors'
    // describing the failure.
")

(define (numeric-read-declarations cpp-types token-class)
  (~a
    (string-join* "\n\n" 
      (for/list ([type cpp-types])
        (numeric-read-declaration type token-class)))
    numeric-read-contract))

(define (numeric-read-define-template token-class ns)
  @~a{
template <typename Number, typename NumberParser>
int readNumeric(Number               *output,
                @|token-class|::Kind  tokenKind,
                TokenIter            *tokenIterPtr,
                TokenIter             endTokens,
                bsl::ostream         *errors
                NumberParser          parseNumber)
{
    BSLS_ASSERT(tokenIterPtr);

    TokenIter         tokenIter(*tokenIterPtr);
    bsl::string       value;
    if (const int rc =
            read(&value, tokenKind, &tokenIter, endTokens, errors)) {
        if (errors) {
            const char *const name = @|ns|bsls::NameOf<Number>::name();
            *errors << "Unable to read a " << name << " from a token of the "
                       " kind " << tokenKind << " because reading the token "
                       "itself failed.\n";
        }
        return rc;                                                     // RETURN
    }

    // If the end had been reached, then 'read' would have failed above.
    BSLS_ASSERT(*tokenIterPtr != endTokens);

    const @|token-class|&  token = **tokenIterPtr;
    Number                 parsedValue;
    @|ns|bslstl::StringRef remainderOrErrorLocation;
    if (const int rc = parseNumber(&parsedValue,
                                   &remainderOrErrorLocation,
                                   token.d_value))
    {
        if (errors) {
            const char *const name = @|ns|bsls::NameOf<Number>::name();
            *errors << "Unable to convert " << quoted(token.d_value)
                    << " to a " << name << ". Error occurred beginning at "
                    << quoted(remainderOrErrorLocation) << '\n';
        }
        return rc;                                                     // RETURN
    }

    if (!remainderOrErrorLocation.empty()) {
        if (errors) {
            const char *const name = @|ns|bsls::NameOf<Number>::name();
            *errors << "Parsed a " << name << ' ' << parsedValue << " from "
                    << quoted(token.d_value)
                    << " without consuming the entire string. "
                    << quoted(remainderOrErrorLocation) << " remains.\n";
        }
        return 1;                                                      // RETURN
    }

    if (output) {
        *output = parsedValue;
    }

    *tokenIterPtr = tokenIter;
    return 0;
}
})

(define (numeric-read-define-macro token-class ns)
  @~a{
#define DEFINE_NUMERIC_READ(TYPE, FUNC)                                        \
    int FUNC(TYPE                          *output,                            \
             @|ns|bslstl::StringRef        *remainderOrErrorLocation,          \
             const @|ns|bslstl::StringRef&  input)                             \
    {                                                                          \
        /* This wrapper function prevents overload ambiguity. */               \
        return @|ns|bdlb::NumericParseUtil::FUNC(output,                       \
                                                 remainderOrErrorLocation,     \
                                                 input);                       \
    }                                                                          \
                                                                               \
    int read(TYPE                 *output,                                     \
             @|token-class|::Kind  tokenKind,                                  \
             TokenIter            *tokenIterPtr,                               \
             TokenIter             endTokens,                                  \
             bsl::ostream         *errors)                                     \
    {                                                                          \
        return readNumeric(output,                                             \
                           tokenKind,                                          \
                           tokenIterPtr,                                       \
                           endTokens,                                          \
                           errors,                                             \
                           &FUNC);                                             \
    }
})

(define (numeric-read-undefine-macro)
  "\n#undef DEFINE_NUMERIC_READER\n")

(define (numeric-read-definition cpp-type parser-func-name)
    @~a{DEFINE_NUMERIC_READ(@cpp-type, @parser-func-name)
})

(define type->func-name
  (applicable-hash "double"             "parseDouble"
                   "long long"          "parseInt64"
                   "int"                "parseInt"
                   "short"              "parseShort"
                   "long long"          "parseSignedInteger"
                   "unsigned long long" "parseUint64"
                   "unsigned"           "parseUint"
                   "unsigned long long" "parseUnsignedInteger"))

(define (numeric-read-definitions cpp-types token-class ns)
  (~a
    (numeric-read-define-template token-class ns)
    "\n\n"
    (numeric-read-define-macro token-class ns)
    "\n\n"
    (string-join* "\n"
      (for/list ([type cpp-types])
        (numeric-read-definition type (type->func-name type))))
    "\n"
    (numeric-read-undefine-macro)))

(define (parser-header class-names
                       package-name
                       enterprise-namespace 
                       parser-class 
                       lexer-class)
  (let* ([ns (calculate-bloomberg-prefix enterprise-namespace)]
         [guard-macro (~>> (list "INCLUDED" package-name parser-class)
                           (map string-upcase)
                           (string-join* "_"))])
    @~a{
#ifndef @guard-macro
#define @guard-macro

#include <bsl_iosfwd.h>
#include <bsl_string.h>

namespace @enterprise-namespace {
namespace @package-name {
@(~>> ; forward declarations of generated classes and the lexer
   (cons lexer-class class-names)
   (map ~a)
   (sort _ string<?)
   (map (lambda (name) (~a "class " name ";")))
   (string-join* "\n"))

struct @parser-class {
    // This 'struct' provides a namespace of functions used to parse instances
    // of generated types from text.
@(~>> ; declare an overload of "parse" for each class
   class-names
   (map (lambda (name) (declare-parse-function name lexer-class ns)))
   (string-join* "\n"))
        // Load into the specified 'output' a value parsed from the specified
        // 'input'. If an error occurs, write diagnostics to the specified
        // 'errorStream'. Use the optionally specified 'lexer' to read tokens.
        // If 'lexer' is zero, then a temporary '@lexer-class' instance will be
        // used instead. Return zero on success or a nonzero value if an error
        // occurs.
};

}  // close package namespace
}  // close enterprise namespace

#endif
}))

(define schema-token-type->cpp-type
  (applicable-hash
    'decimal            "double"
    'int                "int"
    'integer            "long long"
    'long               "long long"
    'negativeInteger    "long long"
    'nonNegativeInteger "long long"
    'nonPositiveInteger "long long"
    'positiveInteger    "long long"
    'short              "short"
    'unsignedLong       "unsigned long long"
    'unsignedInt        "unsigned int"
    'unsignedShort      "unsigned short"))

(define-syntax-rule (only-when when? body ...)
  ; Since `(void)` prints as `#<void>`, `only-when` is used as an alternative to
  ; `when`."
  (if when?
    (begin body ...)
    ""))

; The following five expanders, macros, and procedures are used to decide
; which overloads of the `getMemberPtr` and `resetMember` templates will
; appear in the output. Each template has an overload for `vector`,
; `NullableValue`, and other. If there are no array members in the generated
; types, though, then the `vector` overload is not needed, and etc. for the
; other categories.

(define-match-expander sequence-or-choice
  ; This `match` syntax matches either of `schema/sequence` or `schema/choice`,
  ; with the specified arguments.
  (syntax-rules ()
    [(_ args ...) (or (schema/sequence args ...) (schema/choice args ...))]))

(define-syntax-rule (type-category-finder category)
  ; This syntax expands to a lambda that will check a `schema/*` type's members
  ; for a specified occurence category (`array`, `nullable`, `scalar`). The
  ; lambda will return whether one is found.
 (...  ; treat "..." literally
   (match-lambda
     [(sequence-or-choice _ _ (list (list _ types) ...))
      (any
        (match-lambda [(category _) #t] [_ #f])
        types)]
     [_ #f])))

(define (any-types-contain-array? types)
  (any (type-category-finder array) types))

(define (any-types-contain-nullable? types)
  (any (type-category-finder nullable) types))

(define (any-types-contain-scalar? types)
  (any (type-category-finder scalar) types))

(define (parser-source classes
                       tokens
                       parser-class
                       lexer-class
                       package-name
                       enterprise-namespace)
  (let* ([ns (calculate-bloomberg-prefix enterprise-namespace)]
         [class-names (map schema/base-name classes)]
         [parser-decl (~a "struct " parser-class)]
         [parser-border (make-string (string-length parser-decl) #\-)]
         [token-class (~a lexer-class "Token")]
         [component-of (lambda (class-name)
                         (~>> (list package-name class-name)
                           (map string-downcase)
                           (string-join* "_")))]
         [parser-component (component-of parser-class)]
         [lexer-component (component-of lexer-class)]
         [messages-component (component-of "messages")]  ; TODO?
         [contains-array? (any-types-contain-array? classes)]
         [contains-nullable? (any-types-contain-nullable? classes)]
         [contains-scalar? (any-types-contain-scalar? classes)]
         [numeric-token-cpp-types
          (~>> tokens
               (map (match-lambda [(token _ _ (basic type) _) type]))
               (filter (lambda (type) (not (equal? type 'string))))
               remove-duplicates
               (map schema-token-type->cpp-type))])
    @~a{
#include <@|parser-component|.h>
@(~>> (list lexer-component messages-component)
      (sort _ string<?)
      (map (lambda (component) (~a "#include <" component ".h>\n")))
      (string-join* ""))
#include <bdlb_literalutil.h>
@(if (empty? numeric-token-cpp-types)
   ""
   "#include <bdlb_numericparseutil.h>")
#include <bsls_assert.h>
#include <bsls_nameof.h>

#include <bsl_ostream.h>
#include <bsl_vector.h>

namespace @enterprise-namespace {
namespace @package-name {
namespace {

typedef bsl::vector<@|token-class|>::const_iterator TokenIter;

@(string-join* "\n\n"
  (for/list ([name class-names])
    @~a{
int read(@name        *output,
         TokenIter    *token,
         TokenIter     endTokens,
         bsl::ostream *errors);}))
    // Parse a value from the beginning of the token sequence indicated by the
    // specified 'token' and the specified 'endTokens'. Return zero on success
    // or a nonzero value otherwise. Additionally on success, load the
    // advanced token iterator into the specified 'token', and if the specified
    // 'output' is not zero, load the parsed value into 'output'. Additionally
    // on failure, if the specified 'errors' is not zero, print a diagnostic
    // to 'errors' describing how parsing failed.

// TODO: declarations of generated read functions

int read(bsl::string      *output,
         LexerToken::Kind  tokenKind,
         TokenIter        *token,
         TokenIter         endTokens,
         bsl::ostream     *errors);
    // If the specified 'token' refers to a token having the specified
    // 'tokenKind', then increment 'token', and if additionally 'output' is not
    // zero, load the value of the matching token into 'output'. Return zero on
    // success or a nonzero value if 'token' does not refer to a token of kind
    // 'tokenKind' or is equal to the specified 'endTokens'. On failure, if the
    // specified 'errors' is not zero, write a diagnostic to 'errors'
    // describing how reading failed.

@(only-when (not (empty? numeric-token-cpp-types))
   @~a{
    @(numeric-read-declarations class-names token-class)
    // If the specified 'token' refers to a token having the specified
    // 'tokenKind', then increment 'token', and if additionally 'output' is not
    // zero, convert the matching token into the appropriate type and load the
    // resulting value into 'output'. Return zero on success or a nonzero value
    // if: 'token' does not refer to a token of kind 'tokenKind', 'token' is
    // equal to the specified 'endTokens', or the token is not convertible to
    // the type of 'output'. On failure, if the specified 'errors' is not zero,
    // write a diagnostic to 'errors' describing the failure.
})
@(token-ignore-definition tokens token-class)

bsl::string quoted(const @|ns|bslstl::StringRef& input)
{
    bsl::string result;
    @|ns|bdlb::LiteralUtil::createQuotedEscapedCString(&result, input);
    return result;
}

int tokenize(bsl::vector<@|token-class|>       *tokens,
             const @|ns|bslstl::StringRef&      input,
             bsl::ostream&                      errorStream,
             @lexer-class                      *lexer)
{
    return (lexer ? *lexer : @lexer-class())(&tokens, input, errorStream);
}

template <typename Object>
int genericParse(Object                        *output,
                 const @|ns|bslstl::StringRef&  input,
                 bsl::ostream&                  errorStream,
                 @lexer-class                  *lexer)
{
    bsl::vector<@|token-class|> tokens;
    if (const int rc = tokenize(&tokens, input, errorStream, lexer)) {
        return rc;                                                    // RETURN
    }

    TokenIter tokenIter = tokens.begin();

    if (int rc = read(output, &tokenIter, tokens.end(), &errorStream))
    {
        // TODO
    }

    if (tokenIter != tokens.end())
    {
        // TODO
    }

    return 0;
}@(only-when contains-scalar?
   @~a{


template <typename Member>
Member *getMemberPtr(Member& member)
    // Return a pointer to the specified 'member' of some class parsed by
    // this component.
{
    return &member;
}})@(only-when contains-nullable?
   @~a{


template <typename Member>
Member *getMemberPtr(@|ns|bdlb::NullableValue<Member>& member)
    // Return a pointer to a default-constructed object within the specified
    // 'member' of some class parsed by this component.
{
    return &member.makeValueInplace();
}})@(only-when contains-array?
   @~a{


template <typename Member>
Member *getMemberPtr(bsl::vector<Member>& member)
    // Return a pointer to a default-constructed object appended to the end of
    // the specified 'member' of some class parsed by this component.
{
    member.emplace_back();
    return &member.back();
}})@(only-when contains-scalar?
   @~a{


template <typename Member>
void resetMember(Member&)
    // Non-nullable, non-array members need no resetting.
{
}})@(only-when contains-nullable?
   @~a{


template <typename Member>
void resetMember(@|ns|bdlb::NullableValue<Member>& member)
    // Load a null value into the specified nullable 'member' of some class
    // parsed by this component. This undoes the operation performed by
    // 'getMemberPtr'.
{
    member.reset();
}})@(only-when contains-array?
   @~a{


template <typename Member>
void resetMember(bsl::vector<Member>& member)
    // Erase the last element of the specified array-valued 'member' of some
    // class parsed by this component. This undoes the operation performed by
    // 'getMemberPtr'.
{
    BSLS_ASSERT(!member.empty());
    member.pop_back();
}})

#define MEMBER(OBJECT, ACCESSOR) \
    OBJECT ? getMemberPtr(OBJECT->ACCESSOR()) : 0

#define RESET(OBJECT, ACCESSOR)              \
    do {                                     \
        if (OBJECT) {                        \
            resetMember(OBJECT->ACCESSOR()); \
        }                                    \
    } while (false)

// TODO: generated reader function definitions here

#undef RESET
#undef MEMBER

int read(bsl::string          *output,
         @|token-class|::Kind  tokenKind,
         TokenIter            *tokenIterPtr,
         TokenIter             endTokens,
         bsl::ostream         *errors)
{
    // Parsing functions won't ask for an ignored token kind.
    BSLS_ASSERT(!ignored(tokenKind));

    BSLS_ASSERT(tokenIterPtr);
    TokenIter tokenIter = *tokenIterPtr;  // a copy

    // Skip ignored tokens.
    for (;; ++tokenIter)
    {
        if (tokenIter == endTokens)
        {
            if (errors) {
                *errors << "Unable to read a token of kind " << tokenKind
                        << " because there are no more tokens.\n";
            }
            return 1;                                                 // RETURN
        }

        if (!ignored(tokenIter->d_kind))
        {
            break;
        }
    }

    if (tokenIter->d_kind != tokenKind)
    {
        if (errors) {
            *errors << "Current token is not of the expected kind "
                    << tokenKind << " but is instead of the kind "
                    << tokenIter->d_kind << " with the value "
                    << quoted(tokenIter->d_value) << '\n';
        }
        return 2;                                                     // RETURN
    }

    if (output) {
        *output = tokenIter->d_value;
    }

    ++tokenIter;
    *tokenIterPtr = tokenIter;
    return 0;
}
@(if (empty? numeric-token-cpp-types)
   ""
   (numeric-read-definitions numeric-token-cpp-types token-class ns))
}  // close unnamed namespace

                            // @parser-border
                            // @parser-decl
                            // @parser-border

@(string-join* "\n\n"
   (for/list ([name class-names])
     @~a{int @|parser-class|::parse(@name *output,
                      const @|ns|bslstl::StringRef&  input,
                      bsl::ostream&                  errorStream,
                      @lexer-class                  *lexer)
{
    return genericParse(output, input, errorStream, lexer);
}}))

}  // close package namespace
}  // close enterprise namespace
}))