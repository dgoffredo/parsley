(struct reader-function (name output-type reader) #:prefab)
; Will produce one of:
;
;     int @name(@output-type *output,
;               TokenIter    *tokenIterPtr,
;               TokenIter     endTokens,
;               bsl::ostream *errors)
;     {
;         BSLS_ASSERT(tokenIterPtr);
;         TokenIter tokenIter = *tokenIterPtr;
;         ...
;     }
;
; or
;
;     int @name(TokenIter    *tokenIterPtr,
;               TokenIter     endTokens,
;               bsl::ostream *errors)
;     {
;         BSLS_ASSERT(tokenIterPtr);
;         TokenIter tokenIter = *tokenIterPtr;
;         ...
;     }
;
(struct reader/base          (pattern)                          #:prefab)
(struct reader/compound      reader/base (readers)              #:prefab)
(struct reader/concatenation reader/compound ()                 #:prefab)
; For each reader,
; If it's a term or a token or a token in a term,
;     // <excerpt from the grammar...>
;     if (const int rc = @read(@output?, &tokenIter, tokensEnd, errors))
;     {
;         if (errors) {
;             *errors << @diagnostic << '\n';
;         }
;         @reset(@output);
;         return rc;                                                  // RETURN
;     }
;
; If it's a star,
;     
;     // ("," bars:Bar)*
;     while (!readFoo_1(output, &tokenIter, endTokens, errors))
;         ;
;
; but if the "output" in the while loop is a MEMBER, then the loop must be
; followed by a RESET, e.g.
;
;     // bars:Bar*
;     while (!read(MEMBER(output, bars), &tokenIter, endTokens, errors))
;         ;
;     RESET(output, bars);
;
; If it's a question,
;
;     // "say"?
;     read(0, Token::e_TOKEN_3, &tokenIter, endTokens, 0);
;
; but what if it's a MEMBER?
;
;     // constant:"say"?
;     if (read(MEMBER(output, constant),
;              Token::e_TOKEN_3,
;              &tokenIter,
;              endTokens, 0))
;     {
;         RESET(output, constant);
;     }
;
(struct reader/alternation   reader/compound ()                 #:prefab)
; Try each reader, and continue only if the reader fails. If all of the readers
; fail, then the whole fails. None of the readers will be questions or stars,
; except for one special case: It might be that the last reader is starred. In
; this case loop over that reader, consuming all successes, and consider the
; whole a success (since "zero or more ____" is one of the alternatives, so
; matching nothing is success -- zero of whatever).
;
;     // Bar  ::=  number:/0|[1-9][0-9]*/ | fish:"fish" | chicken:"chicken"
;     int read(Bar          *output, 
;              TokenIter    *tokenIterPtr,
;              TokenIter     endTokens,
;              bsl::ostream *errors)
;     {
;         BSLS_ASSERT(tokenIterPtr);
;         TokenIter tokenIter = *tokenIterPtr;
;     
;         // number:/0|[1-9][0-9]*/
;         if (read(MEMBER(output, makeNumber), 
;                  LexerToken::e_TOKEN_1, 
;                  &tokenIter, 
;                  endTokens,
;                  0) == 0)
;         {
;             *tokenIterPtr = tokenIter;
;             return 0;                                               // RETURN
;         }
;         RESET(output, makeNumber);
;     
;         // fish:"fish" 
;         if (read(MEMBER(output, makeFish),
;                  LexerToken::e_TOKEN_4,
;                  &tokenIter,
;                  endTokens,
;                  0) == 0)
;         {
;             *tokenIterPtr = tokenIter;
;             return 0;                                               // RETURN
;         }
;         RESET(output, makeFish);
;     
;         // chicken:"chicken"
;         if (read(MEMBER(output, makeChicken),
;                  LexerToken::e_TOKEN_5,
;                  &tokenIter,
;                  endTokens,
;                  0))
;         {
;             *tokenIterPtr = tokenIter;
;             return 0;                                               // RETURN
;         }
;         RESET(output, makeChicken);
;     
;         if (errors)
;         {
;             *errors << 
;                 "None of the possibilities in the following alternation "
;                 "could be read: number:/0|[1-9][0-9]*/ | fish:\"fish\" | "
;                 "chicken:\"chicken\"\n";
;         }
;     
;         return 1;
;     }
;
; Here's what the last section would look like were the chicken starred:
;     ...
;         // "chicken"*
;         while (!read(0,
;                      LexerToken::e_TOKEN_5,
;                      &tokenIter,
;                      endTokens,
;                      0))
;             ;
;         *tokenIterPtr = tokenIter;
;         return 0;
;     }
;
; A choice type can't have an array in it, but a non-choice alternation could
; end in a star.
(struct reader/star          reader/base (term)                 #:prefab)
; As seen above,
;
;     while (read(...))
;         ;
;     RESET(...);  // maybe
;
(struct reader/question      reader/base (term)                 #:prefab)
; Either
;
;     read(...);
; 
; or
;
;     if (read(...)) {
;         RESET(...);
;     }
;
(struct reader/term          reader/base (term-output function) #:prefab)
; This is tells you how to form the "read" call (or "readSomething_N" call),
; including what to output (which, in turn, tells you whether you need RESET).
; There's an annoying difference between passing zero as output and not having
; an output parameter at all. To tell the difference, inspect `function`.
(struct reader/token         reader/base (name)                 #:prefab)
; There's a set of overloads of `read` that take a token `Kind` as the second
; argument. The enumeration value name can be derived from `name` (e.g. "foo"
; -> "e_FOO").
(struct reader/enumeration   reader/base (class-name function)  #:prefab)
; This
;
;     (reader-function
;       "read"
;       ((scalar occurrence 1) ((complex kind 1) "TimesOrDividedBy::Value"))
;       (reader/enumeration "TimesOrDividedBy" "readTimesOrDividedByValue")))
;
; produces
;
;     int read(TimesOrDividedBy::Value *output,
;              TokenIter               *tokenIterPtr,
;              TokenIter                endTokens,
;              bsl::ostream            *errors)
;     {
;         BSLS_ASSERT(tokenIterPtr);
; 
;         TokenIter   tokenIter = *tokenIterPtr;
;         bsl::string value;
;         if (const int rc = readTimesOrDividedByValue(&value,
;                                                      &tokenIter,
;                                                      endTokens,
;                                                      errors))
;         {
;             if (errors) {
;                 *errors << "Unable to read a TimeOrDividedBy::Value because "
;                            "unable to read the underlying string value.\n";
;             }
;             return rc;                                              // RETURN
;         }
;
;         *tokenIterPtr = tokenIter;
;         return 0;
;     }