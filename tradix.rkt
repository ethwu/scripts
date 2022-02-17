#! /usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/contract
         racket/list
         racket/port
         racket/string
         raco/command-name)

(provide number->digits
         digits->string)

(define radix/c (integer-in 2 #f))
(define digits/c (listof natural-number/c))
(define alphabet/c (procedure-arity-includes/c 2))

; Convert n to a list of digits in the given radix.
(define/contract (number->digits num [radix 10])
  ((natural-number/c) (radix/c) . ->* . digits/c)
  (let loop ([num num] [acc '()])
    (cond
      [(zero? num) (if (empty? acc) '(0) acc)]
      [else
       (define-values (q r) (quotient/remainder num radix))
       (loop q (cons r acc))])))

; Build an alphabet. Returns a lambda with two arguments, `output?` and `digit`.
; If `output?` is true, then the lambda will look up the string corresponding to
; the passed digit, or "?" if the digit is not available in the alphabet.
; If `outupt?` is false, then the lambda will take the passed digit string and
; find the corresponding integer.
(define/contract (make-alphabet available)
  (list? . -> . alphabet/c)
  (lambda (output? digit)
    (if output?
        (if ((integer-in 0 (length available)) digit) (list-ref available digit) #\?)
        (index-of available digit))))

; Cast a string of single-character digits to an alphabet. The strings ".", "(", and ")" are reserved,
; and will be replaced with "?". You should not use "?" in an alphabet.
(define/contract (string->alphabet str)
  (string? . -> . alphabet/c)
  (make-alphabet (map (lambda (c)
                        (case c
                          [(#\. #\( #\)) #\?]
                          [else c]))
                      (string->list str))))

; The default alphabet.
(define default-alphabet
  (string->alphabet "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

; Dozenal alphabet using the Unicode Pitman numerals for ten and eleven.
(define pitman (make-alphabet '(0 1 2 3 4 5 6 7 8 9 ↊ ↋)))
; Dozenal alphabet using lowercase delta and epsilon for ten and eleven.
(define delta-epsilon (make-alphabet '(0 1 2 3 4 5 6 7 8 9 δ ε)))
; Dozenal alphabet using lowercase tau and epsilon for ten and eleven.
(define tau-epsilon (make-alphabet '(0 1 2 3 4 5 6 7 8 9 τ ε)))

; The alphabet used in Base64.
(define base64 (string->alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))

; Get alphabet according to its specifier, or define a new alphabet.
(define/contract (get-alphabet specifier)
  ((or/c alphabet/c string?) . -> . alphabet/c)
  (if (procedure? specifier)
      specifier
      (case specifier
        [("default") default-alphabet]
        [("pitman") pitman]
        [("delta-epsilon") delta-epsilon]
        [("tau-epsilon") tau-epsilon]
        [("base64") base64]
        [else (string->alphabet specifier)])))

; Convert a list of digits to a string using the given alphabet.
(define/contract (digits->string digits [alphabet default-alphabet])
  ((digits/c) (alphabet/c) . ->* . string?)
  (let loop ([digits digits] [acc ""])
    (cond
      [(empty? digits) acc]
      [else (loop (cdr digits) (format "~a~a" acc (alphabet #t (car digits))))])))

(module+ main
  ; The input radix. Defaults to ten.
  (define input-radix (make-parameter 10))
  ; The output radix. Defaults to ten.
  (define output-radix (make-parameter 10))

  ; The input alphabet.
  (define input-alphabet (make-parameter default-alphabet))

  ; The output format. Defaults to num.
  (define output-format (make-parameter "num"))
  ; The output alphabet.
  (define output-alphabet (make-parameter default-alphabet))

  (command-line
   #:program (short-program+command-name)
   #:usage-help ;
   "Convert between radices. Flags beginning with capital letters correspond to"
   "input, and flags beginning with lowercase letters correspond to output. Both"
   "input and output default to decimal. The default alphabet extends the Hindu–"
   "Arabic numeral system with the lowercase Latin letters for 10 through 35 and"
   "the uppercase Latin letters for 36 through 61. If no number is given for"
   "conversion, input will be taken from STDIN."
   #:ps "\nAll numerals in this help text are in decimal (base 10)."
   #:help-labels ;
   "=============================== Radix Options ================================="
   "Specify the input and output radices. Both default to decimal (base 10). While"
   "shortcuts are provided for several common radices, you can also specify a radix"
   "in decimal using the --input-radix and --output-radix flags."
   ""
   #:once-any [("-i" "-I" "--input-radix")
               input_radix
               "Specify the input radix. Must be between 2 and 16."
               (input-radix (string->number input_radix))]
   [("-B" "--Binary")                             "Interpret as binary.      (Base 2)" (input-radix 2)]
   [("-S" "--Senary")                             "Interpret as senary.      (Base 6)" (input-radix 6)]
   [("--Octal")                                   "Interpret as octal.       (Base 8)" (input-radix 8)]
   [("-D" "--Dozenal" "--Duodecimal" "--Uncial")  "Interpret as dozenal.     (Base 12)" (input-radix 12)]
   [("-X" "--Hexadecimal")                        "Interpret as hexadecimal. (Base 16)" (input-radix 16)]
   #:help-labels ""
   #:once-any [("-o" "--output-radix")
               output_radix
               "Specify the output radix. Must be at least 2."
               (output-radix (string->number output_radix))]
   [("-b" "--binary")                             "Print in binary.          (Base 2)" (output-radix 2)]
   [("-s" "--senary")                             "Print in senary.          (Base 6)" (output-radix 6)]
   [("--octal")                                   "Print in octal.           (Base 8)" (output-radix 8)]
   [("-d" "--dozenal" "--duodecimal" "--uncial")  "Print in dozenal.         (Base 12)" (output-radix 12)]
   [("-x" "--hexadecimal")                        "Print in hexadecimal.     (Base 16)" (output-radix 16)]
   [("--vigesimal")                               "Print in vigesimal.       (Base 20)" (output-radix 20)]
   [("--sexagesimal" "--sexagenary")              "Print in sexagesimal.     (Base 60)" (output-radix 60)]
   #:help-labels ""
   "============================== Alphabet Options ==============================="
   "Specify the alphabets used for interpreting and printing. The --input-alphabet"
   "and --output-alphabet flags may take either a string of one-character digits"
   "that form an alphabet (i.e., '0123456789abcdef') or one of the following"
   "specifiers:"
   ""
   "  * default       An alphabet that extends the Hindu–Arabic numeral system with"
   "                  lowercase Latin letters for 10 through 35 and uppercase Latin"
   "                  letters for 36 through 61. Supports any radix up to 62."
   "  * pitman        An alphabet that uses Isaac Pitman’s dozenal numerals for ten"
   "                  and eleven, '↊' and '↋'. Supports any radix up to 12."
   "  * delta-epsilon An alphabet that uses lowercase delta 'δ' and epsilon 'ε' for"
   "                  ten and eleven. Supports any radix up to 12."
   "  * tau-epsilon   An alphabet that uses lowercase tau 'τ' and epsilon 'ε' for"
   "                  ten and eleven. Supports any radix up to 12."
   "  * base64        The alphabet used by the Base64 encoding scheme. Does not"
   "                  pad. Supports any radix up to 64."
   ""
   #:once-any
   [("-A" "--input-alphabet") alphabet "Specify the input alphabet." (input-alphabet alphabet)]
   [("--Pitman")
    "Interpret Isaac Pitman’s dozenal numerals.        Implies --Dozenal."
    (input-radix 12)
    (input-alphabet pitman)]
   [("--Delta-Epsilon")
    "Interpret delta–epsilon-style dozenal numerals.   Implies --Dozenal."
    (input-radix 12)
    (input-alphabet delta-epsilon)]
   [("--Tau-Epsilon")
    "Interpret tau–epsilon-style dozenal numerals.     Implies --Dozenal."
    (input-radix 12)
    (input-alphabet tau-epsilon)]
   [("--Base64")
    "Interpret using Base64.                           Implies --input-radix 64."
    (input-radix 64)
    (input-alphabet base64)]
   #:help-labels ""
   #:once-any
   [("-a" "--output-alphabet") alphabet "Specify the output alphabet. " (output-alphabet alphabet)]
   [("--pitman")
    "Print using Isaac Pitman’s dozenal numerals.      Implies --dozenal."
    (output-radix 12)
    (output-alphabet pitman)]
   [("--delta-epsilon")
    "Print using delta–epsilon-style dozenal numerals. Implies --dozenal."
    (output-radix 12)
    (output-alphabet delta-epsilon)]
   [("--tau-epsilon")
    "Print using tau–epsilon-style dozenal numerals.   Implies --dozenal."
    (output-radix 12)
    (output-alphabet tau-epsilon)]
   [("--base64")
    "Print using using Base64.                         Implies --output-radix 64."
    (output-radix 64)
    (output-alphabet base64)]
   #:help-labels ""
   #:once-any
   [("-l" "--list") "Print the converted number as a list of place values." (output-format "list")]
   #:help-labels ""
   #:args ([number #f])
   (let* ([number (string->number (string-trim (if number number (port->string (current-input-port))))
                                  (input-radix))]
          [digits (number->digits number (output-radix))]
          [input-alphabet (get-alphabet (input-alphabet))]
          [output-alphabet (get-alphabet (output-alphabet))])
     (displayln (case (output-format)
                  [("num") (digits->string digits output-alphabet)]
                  [else digits])))
   (unless number
     (close-input-port (current-input-port)))))

(module+ test
  (require rackunit)

  (test-case "number->digits should convert integers to a list of place values."
             (check-equal? (number->digits 78 2) '(1 0 0 1 1 1 0))
             (check-equal? (number->digits 766 8) '(1 3 7 6))
             (check-equal? (number->digits 962 10) '(9 6 2))
             (check-equal? (number->digits 363 16) '(1 6 11))
             (check-equal? (number->digits 655 16) '(2 8 15)))

  (test-case "digits->string should convert a list of place values to a string."
             (check-equal? (digits->string '(1 0 0 1 1 1 0)) "1001110")
             (check-equal? (digits->string '(1 3 7 6)) "1376")
             (check-equal? (digits->string '(1 6 11)) "16b")
             (check-equal? (digits->string '(2 8 15)) "28f"))

  (test-case "digits->string should the specified alphabet."
             (check-equal? (digits->string '(1 6 11) pitman) "16↋")))
