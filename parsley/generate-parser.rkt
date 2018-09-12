#lang at-exp racket

(provide reader-functions ; TODO debugging
         parser-header)

(require "productions.rkt"
         "types.rkt"
         "names.rkt"
         "counter.rkt"
         "codegen-util.rkt"
         "applicable-dict.rkt"
         threading
         srfi/1
         racket/generator
         racket/hash)

(define-for-syntax debugging? #t)

(define-syntax (debug stx)
  (syntax-case stx ()
    [(debug args ...)
     (if debugging?
       #'(displayln (string-join (map ~a (list args ...)))
                    (current-error-port))
       #'(void))]))

; This struct describes a C++ function that will appear in the anonymous
; namespace of the parser's implementation. Each reader-function reads either
; a class or some part of a class.
(struct reader-function (name output-type reader) #:prefab)

; These structs describe parts of a reader-function. Each node in a production
; rule pattern corresponds to a strategy for reading that production from
; input, e.g. (Concatenation A B?) produces a concatenation-reader whose
; readers are a term-reader and a question-reader, respectively.
(struct reader/base          (pattern)             #:prefab)
(struct reader/compound      reader/base (readers) #:prefab)
(struct reader/concatenation reader/compound ()    #:prefab)
(struct reader/alternation   reader/compound ()    #:prefab)
(struct reader/star          reader/base (term)    #:prefab)
(struct reader/question      reader/base (term)    #:prefab)
; The term-output field of reader/term can be any of:
; - 'forward, indicating that the output parameter of the calling function
;   is forwarded to it, or
; - a member-accessor object, indicating that the output goes to a member of
;   the output parameter of the calling function, or
; - #f, indicating no output (pass a null pointer for the output parameter).
; The function field of reader/term can be any of:
; - a string naming the function, or
; - a reader/token object indicating the token to be read.
(struct reader/term        reader/base (term-output function) #:prefab)
(struct reader/token       reader/base (name)                 #:prefab)
(struct reader/enumeration reader/base (class-name function)  #:prefab)
(struct member-accessor    (name)                 #:prefab)

(define (alternation/concatenation? symbol)
  (member symbol '(Alternation Concatenation)))

(define (star/question? symbol)
  (member symbol '(Star Question)))

(define (contains-bindings? pattern)
  (match pattern
    [(list 'Bound _ _) #t]
    [(list _ args ...) (any contains-bindings? args)]
    [_ #f]))

(define (pattern->output-type pattern output-type)
  (debug "in" (~s (list 'pattern->output-type pattern output-type)))
  (match output-type
    [(occurrence (basic _)) output-type] ; basic -> keep (e.g. for enum values)
    [#f #f]                              ; none -> none
    [_                                   ; otherwise, only if has bindings
     (if (contains-bindings? pattern)
       output-type
       #f)]))

; TODO: The arithmetic grammar makes me realize that an optimization is
;       available after this step: Find functions that are the same, and pick
;       just one of them, renaming as necessary.
(define (pattern->reader pattern 
                         output-type 
                         element->accessor-name
                         name->production
                         get-reader-function-name)
  (let recur ([pattern (restore-question pattern)]
              [output-type output-type] 
              [element->accessor-name element->accessor-name]
              [name->production name->production]
              [get-reader-function-name get-reader-function-name])
    (match pattern ; TODO: 
                   ; Probably want the restore-question step to be more
                   ; visible from the outside, but then need a replace-rule
                   ; procedure for schema/* types analogous to replace-pattern
                   ; for rule/* types.
      ; TODO: This case and the symbol case share some code. Figure out how to
      ;       refactor the common stuff.
      [(list 'Bound element-name binding-pattern)
       (debug "pattern->reader Bound case:" element-name binding-pattern)
       (reader/term
         ; pattern
         pattern
         ; term-output
         (if output-type
           (member-accessor (dict-ref element->accessor-name element-name))
           #f)
         ; function
         (match (dict-ref name->production binding-pattern)
           ; classes get read by their "read" function overload
           [(schema/base name _) "read"]
           ; tokens get read by a call to a read overload with a token kind
           [(token name pattern _ _) (reader/token pattern name)]))]
      [(? symbol? name)
       (debug "pattern->reader symbol case:" name)
       (let ([output (if output-type 'forward #f)])
         (match (dict-ref name->production name)
           ; classes get read by their "read" function overload
           [(schema/base name _) (reader/term name output "read")]
           ; tokens get read by a call to a read overload with a token kind
           [(token name pattern _ _)
            (reader/term pattern output (reader/token pattern name))]))]
      ; TODO: This case and the star/question case share a lot of code. Figure
      ;       out how to refactor the common stuff.
      [(list (? alternation/concatenation? which) patterns ...)
       (debug "pattern->reader alt/cat case:" which patterns)
       (let ([reader-constructor
              (match which
                ['Concatenation reader/concatenation]
                ['Alternation reader/alternation])])
         (reader-constructor
           pattern
           (for/list ([pattern patterns])
             (let* ([output-type (pattern->output-type pattern output-type)]
                    [reader
                     (recur pattern
                            output-type
                            element->accessor-name
                            name->production
                            get-reader-function-name)])
                (debug "in pattern->reader alt/cat with output-type:"
                       output-type)
                (match reader
                  [(struct reader/compound _)
                   (let ([name (get-reader-function-name output-type reader)])
                     (debug "in pattern->reader alt/cat about to emit term "
                            "for output-type:" output-type 
                            " and reader/compound function name:" name)
                     (reader/term pattern (if output-type 'forward #f) name))]
                  [_ 
                   (debug "in pattern->reader alt/cat about to emit term for "
                          "output-type:" output-type 
                          " and reader:" reader)
                   reader])))))]
      [(list (? star/question? which) term-pattern)
       (debug "pattern->reader star/question case:" which term-pattern)
       (let* ([output-type (pattern->output-type term-pattern output-type)]
              [reader
               (recur term-pattern
                      output-type
                      element->accessor-name
                      name->production
                      get-reader-function-name)]
              [reader-constructor
               (match which 
                 ['Star reader/star]
                 ['Question reader/question])])
           (reader-constructor
             pattern
             (match reader
               [(struct reader/compound _)
                (let ([name (get-reader-function-name output-type reader)])
                  (reader/term term-pattern 
                               (if output-type 'forward #f)
                               name))]
               [_ reader])))])))

(define (productions-by-name types-and-tokens)
  (match types-and-tokens
    [(productions types tokens)
     (hash-union
       (for/hash ([type types])
         (values (schema/base-name type) type))
       (for/hash ([token tokens])
         (values (token-name token) token)))]))

(define (sequence/choice-reader-function class-name
                                         pattern
                                         element-types
                                         name->production
                                         name-casing)
  (let ([output-type  (scalar (complex class-name))])
    (reader-function
      ; name
      "read"
      ; output type
      output-type
      ; reader
      (pattern->reader
        ; pattern
        pattern
        ; output type
        output-type
        ; element->accessor-name
        (for/hash ([elem-type element-types])
          (match elem-type
            [(list elem-name _)
              (values elem-name (name-casing elem-name))]))
        ; hash-table mapping productions by name
        name->production
        ; get-reader-function-name (to create child readers)
        (let ([tick (make-counter 1)])
          (lambda (output-type reader)
            (debug "in" (~s (list 'get-reader-function-name output-type reader)))
            (let ([name (~a "read" class-name "_" (tick))])
              (yield (reader-function name output-type reader))
              name)))))))

(define (reader-functions types-and-tokens)
  (let ([name->production (productions-by-name types-and-tokens)])
    (match types-and-tokens
      [(productions types tokens)
       (sequence->list
         (in-generator
           (for ([type types])
             (match type
               [(schema/sequence name rule element-types)
                (yield
                  (sequence/choice-reader-function
                    (class-case name)        ; class-name
                    (rule/base-pattern rule) ; pattern
                    element-types
                    name->production
                    attribute-case))] ; name-casing (different in choice)
               [(schema/choice name rule element-types)
                (yield
                  (sequence/choice-reader-function
                    (class-case name)        ; class-name
                    (rule/base-pattern rule) ; pattern
                    element-types
                    name->production
                    maker-case))] ; name-casing (different in sequence)
               [(schema/enumeration name rule values)
                ; An enumeration class "Foo" yields two functions:
                ; first "readFooValue", which outputs to a string and is an
                ; alternation reader among the tokens matching the values of
                ; Foo, and second "read" overloaded for "Foo::Value", which
                ; calls "readFooValue" and then converts the resulting string
                ; into a "Foo::Value" using the conversion routine in "Foo".
                (let ([read-value-name (~a "read" (class-case name) "Value")]
                      [string-type (scalar (basic 'string))])
                  ; the function that outputs the string value of the enum.
                  (yield
                    (reader-function
                      ; name
                      read-value-name
                      ; output type
                      string-type
                      ; reader
                      (pattern->reader
                        ; pattern
                        (rule/base-pattern rule)
                        ; output type
                        string-type
                        ; element->accessor-name (not used in this case)
                        #f
                        ; hash-table mapping productions by name
                        name->production
                        ; get-reader-function-name (not used in this case)
                        #f)))
                  ; the function that outputs the enum value
                  (yield
                    (reader-function
                    ; name
                    "read"
                    ; output type (the name of the enum type within the class)
                    (scalar (complex (~a (class-case name) "::Value")))
                    ; reader
                    (reader/enumeration
                      ; pattern
                      (rule/base-pattern rule)
                      ; class name
                      (class-case name)
                      ; (value reader) function
                      read-value-name))))]))))])))

(define (generate-parser . TODO)
  'TODO)

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

; TODO: This will be able to appear inline in the .cpp template.
(define (token-read-declaration token-class)
  @~a{int read(bsl::string      *output,
         @|token-class|::Kind  tokenKind,
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
})

(define (numeric-read-declaration cpp-type token-class)
  @~a{int read(@cpp-type              *output,
         @|token-class|::Kind  tokenKind,
         TokenIter            *token,
         TokenIter             endTokens,
         bsl::ostream         *errors);})

(define numeric-read-contract
  @~a{
    // If the specified 'token' refers to a token having the specified
    // 'tokenKind', then increment the token referred to by 'token', and if
    // additionally 'output' is not zero, convert the matching token into the
    // appropriate type and load the resulting value into 'output'. Return zero
    // on success or a nonzero value if: 'token' does not refer to a token of
    // kind 'tokenKind', 'token' is equal to the specified 'endTokens', or the
    // token is not convertible to the type of 'output'. On failure, if the
    // specified 'errors' is not zero, write a diagnostic to 'errors'
    // describing the failure.
})

(define (numeric-read-declarations cpp-types token-class)
  (~a
    (string-join* "\n\n" 
      (for/list ([type cpp-types])
        (numeric-read-declaration type token-class)))
    "\n    "
    numeric-read-contract))

(define (numeric-read-macro token-class ns)
  @~a{
#define DEFINE_NUMERIC_READER(TYPE, UTIL_FUNC)                                \
int read(TYPE                 *output,                                        \
         @|token-class|::Kind  tokenKind,                                     \
         TokenIter            *tokenIterPtr,                                  \
         TokenIter             endTokens,                                     \
         bsl::ostream         *errors)                                        \
{                                                                             \
    BSLS_ASSERT(tokenIterPtr);                                                \
                                                                              \
    const char *const name = bsls::NameOf<TYPE>::name();                      \
    TokenIter         tokenIter(*tokenIterPtr);                               \
    bsl::string       value;                                                  \
    if (const int rc =                                                        \
            read(&value, tokenKind, &tokenIter, endTokens, errors)) {         \
        if (errors) {                                                         \
            *errors << "Unable to read a " << name << " from a token of the " \
                       " kind " << tokenKind << " because reading the token " \
                       "itself failed.\n";                                    \
        }                                                                     \
        return rc;                                                            \
    }                                                                         \
                                                                              \
    BSLS_ASSERT(*tokenIterPtr != endTokens);                                  \
                                                                              \
    const Token&           token = **tokenIterPtr;                            \
    TYPE                   parsedValue;                                       \
    @|ns|bslstl::StringRef remainderOrErrorLocation;                          \
    if (const int rc = @|ns|bdlb::NumericParseUtil::UTIL_FUNC(                \
                           &parsedValue,                                      \
                           &remainderOrErrorLocation,                         \
                           token.d_value))                                    \
    {                                                                         \
        if (errors) {                                                         \
            *errors << "Unable to convert " << quoted(token.d_value)          \
                    << " to a " << name << ". Error occurred beginning at "   \
                    << quoted(remainderOrErrorLocation) << '\n';              \
        }                                                                     \
        return rc;                                                            \
    }                                                                         \
                                                                              \
    if (!remainderOrErrorLocation.empty()) {                                  \
        if (errors) {                                                         \
            *errors << "Parsed a " << name << ' ' << parsedValue << " from "  \
                    << quoted(token.d_value)                                  \
                    << " without consuming the entire string. "               \
                    << quoted(remainderOrErrorLocation) << " remains.\n";     \
        }                                                                     \
        return 1;                                                             \
    }                                                                         \
                                                                              \
    if (output) {                                                             \
        *output = parsedValue;                                                \
    }                                                                         \
                                                                              \
    *tokenIterPtr = tokenIter;                                                \
    return 0;                                                                 \
}
})

(define (numeric-read-definition cpp-type parser-func-name)
    @~a{DEFINE_NUMERIC_READER(@cpp-type, @parser-func-name)
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
    (numeric-read-macro token-class ns)
    "\n"
    (string-join* "\n"
      (for/list ([type cpp-types])
        (numeric-read-definition type (type->func-name type))))
    "\n#undef DEFINE_NUMERIC_READER\n"))

(define (restore-question pattern)
  "Replace occurrences of alternations involving nil instead with a question
   mark operator, e.g. transform
       foo | ()
   into
       foo?
   Question mark operators are removed during previous analysis of the rules,
   but they are useful to recognize for parser generation (the resulting code
   makes more sense)."
  (match pattern
    [(list 'Alternation subpattern '())
     (debug "restore-question Alternation:" pattern)
     (list 'Question (restore-question subpattern))]

    [(list operation args ...)
     (debug "restore-question recur:" pattern)
     (cons operation (map restore-question args))]

    [otherwise
     (debug "restore-question otherwise:" pattern)
     otherwise]))

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
         [numeric-token-cpp-types
          (~>> tokens
               (map (match-lambda [(token _ _ (basic type) _) type]))
               (filter (lambda (type) (not (equal? type 'string))))
               remove-duplicates
               (map schema-token-type->cpp-type))])
    @~a{
#include <@|parser-component|.h>
#include <@|lexer-component|.h>

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
int read(@name        *output
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

@(if (empty? numeric-token-cpp-types)
   ""
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

    TokenIter begin = tokens.begin();

    return read(output, &begin, tokens.end(), &errorStream);
}

template <typename Member>
Member *getMemberPtr(Member& member)
    // Return a pointer to the specified 'member' of some class parsed by
    // this component.
{
    return &member;
}

template <typename Member>
Member *getMemberPtr(@|ns|bdlb::NullableValue<Member>& member)
    // Return a pointer to a default-constructed object within the specified
    // 'member' of some class parsed by this component.
{
    return &member.makeValueInplace();
}

template <typename Member>
Member *getMemberPtr(bsl::vector<Member>& member)
    // Return a pointer to a default-constructed object appended to the end of
    // the specified 'member' of some class parsed by this component.
{
    member.emplace_back();
    return &member.back();
}

template <typename Member>
void resetMember(Member&)
    // Non-nullable, non-array members need no resetting.
{
}

template <typename Member>
void resetMember(@|ns|bdlb::NullableValue<Member>& member)
    // Load a null value into the specified nullable 'member' of some class
    // parsed by this component. This undoes the operation performed by
    // 'getMemberPtr'.
{
    member.reset();
}

template <typename Member>
void resetMember(bsl::vector<Member>& member)
    // Erase the last element of the specified array-valued 'member' of some
    // class parsed by this component. This undoes the operation performed by
    // 'getMemberPtr'.
{
    BSLS_ASSERT(!member.empty());
    member.pop_back();
}

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