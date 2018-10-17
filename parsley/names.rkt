#lang racket

(provide class-case
         attribute-case
         maker-case
         enum-value-case
         bde-enum-value-case
         all-capitals?)

(require threading) ; ~>> and similar macros

(define (all-capitals? string)
  "Return whether no unicode character in the specified @var{string} is lower
   case"
  (not (regexp-match #px"\\p{Ll}" string)))

(define split-name
  (let* ([clauses '("\\s+"                                       ; whitespace
                    "\\p{P}+"                                    ; punctuation
                    "(?<=[[:upper:]])(?=[[:upper:]][[:lower:]])" ; THISCase
                    "(?<=[[:lower:]])(?=[[:upper:]])")]          ; thisCase
         [pattern-string (string-join clauses "|")]
         [separator-regexp (pregexp pattern-string)])
    (lambda (name)
    ; Divide the specified string into a list of parts, where each part was
    ; separated from its adjacent parts by any of:
    ; - white space
    ; - punctuation
    ; - an upper case to lower case transition
    ; - a lower case to upper case transition
    ;
    ; For example:
    ; "IANATimeZone" -> ("IANA" "Time" "Zone")
    ; "wakka/wakka.txt" -> ("wakka" "wakka" "txt")
    ; "this_-oneIS   contrived-true" -> ("this" "one" "IS" "contrived" "true")
    ; "THISIsAHARDOne" -> ("THIS" "Is" "AHARD" "One")
    ; "BSaaS" -> ("B" "Saa" "S")
      (regexp-split separator-regexp name))))

(define (capitalize-first text)
  ; "hello" -> "Hello"
  ; "hello there" -> "Hello there"
  ; "12345" -> "12345"
  (match (string->list text)
    [(cons first-char others)
     (list->string (cons (char-upcase first-char) others))]
    [_ text]))

(define (class-case name)
  ; Classes use CapitalizedCamelCasing.
  (~>> name
    ~a
    split-name
    (map string-downcase)
    (map capitalize-first)
    (string-join _ "")))

(define (attribute-case name)
  ; Attributes (class member accessors) use mostlyCapitalizedCamelCasing.
  (~> name
    ~a
    split-name
    (map string-downcase _)
    (match [(cons head tail) (cons head (map capitalize-first tail))])
    (string-join "")))

(define (maker-case name)
  ; Manipulators that set a choice selection use makeSomethingCase.
  (~>> name 
    ~a 
    class-case
    (string-append "make")))

(define (enum-value-case name)
  ; Enumeration values use UNDERSCORE_SEPARATED_UPPER_CASING.
  (~>> name
    ~a
    split-name
    (map string-upcase)
    (string-join _ "_")))

(define (bde-enum-value-case name)
  ; BDE code prefixes enum values with "e_".
  (string-append "e_" (enum-value-case name)))
