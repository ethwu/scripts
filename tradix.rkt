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

; Convert n to a list of digits in the given radix.
(define/contract (number->digits num [radix 10])
                 ((natural-number/c) (exact-positive-integer?) . ->* . (listof natural-number/c))
                 (let loop ([num num] [acc '()])
                   (cond
                     [(zero? num) (if (empty? acc) '(0) acc)]
                     [else
                      (define-values (q r) (quotient/remainder num radix))
                      (loop q (cons r acc))])))

; Build an alphabet. Returns `?` when the passed index is out of bounds.
(define (make-alphabet available)
  (lambda (digit) (if (< digit (length available)) (list-ref available digit) #\?)))

; The default alphabet.
(define default-alphabet
  (make-alphabet (string->list "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

; Dozenal alphabet using the Unicode Pitman numerals for ten and eleven.
(define dozenal-pitman (make-alphabet '(0 1 2 3 4 5 6 7 8 9 ↊ ↋)))

; Convert a list of digits to a string using the given alphabet.
(define/contract (digits->string digits [alphabet default-alphabet])
                 (((listof natural-number/c)) ((procedure-arity-includes/c 1)) . ->* . string?)
                 (let loop ([digits digits] [acc ""])
                   (cond
                     [(empty? digits) acc]
                     [else (loop (cdr digits) (format "~a~a" acc (alphabet (car digits))))])))

; The input radix. Defaults to ten.
(define input-radix (make-parameter 10))
; The output radix. Defaults to ten.
(define output-radix (make-parameter 10))

; The output format. Defaults to list.
(define output-format (make-parameter "num"))
; The output alphabet.
(define output-alphabet (make-parameter "default"))

(module+ main
  (command-line
   #:program (short-program+command-name)
   #:once-any ;
   [("-i" "--input-radix")
    input_radix ;
    "The input base. [default: 10]"
    (input-radix (string->number input_radix))]
   [("-B" "--Binary") "Binary (base 2)." (input-radix 2)]
   [("-S" "--Senary") "Senary (base 6)." (input-radix 6)]
   [("--Octal") "Octal (base 8)." (input-radix 8)]
   [("-D" "--Dozenal") "Dozenal (base 12)." (input-radix 12)]
   [("-X" "--Hexadecimal") "Hexadecimal (base 16)." (input-radix 16)]
   #:once-any ;
   [("-o" "--output-radix")
    output_radix ;
    "The output base. [default: 10]"
    (output-radix (string->number output_radix))]
   [("-b" "--binary") "Binary (base 2)." (output-radix 2)]
   [("-s" "--senary") "Senary (base 6)." (output-radix 6)]
   [("--octal") "Octal (base 8)." (output-radix 8)]
   [("-d" "--dozenal") "Dozenal (base 12)." (output-radix 12)]
   [("-x" "--hexadecimal") "Hexadecimal (base 16)." (output-radix 16)]
   [("--sexagesimal") "Sexagesimal (base 60)." (output-radix 60)]
   #:once-any ;
   [("-l" "--list") "Output as a list of digit values." (output-format "list")]
   #:once-any ;
   [("-a" "--alphabet")
    alphabet ;
    "Specify the output alphabet."
    (output-alphabet alphabet)]
   [("--pitman")
    "Isaac Pitman's dozenal numerals. Implies --dozenal."
    (output-radix 12)
    (output-alphabet "dozenal-pitman")]
   #:args ([number #f])
   (let* ([number (string->number (string-trim (if number number (port->string (current-input-port))))
                                  (input-radix))]
          [digits (number->digits number (output-radix))]
          [alphabet (case (output-alphabet)
                      [("default") default-alphabet]
                      [("dozenal-pitman") dozenal-pitman]
                      [else (make-alphabet (string->list (output-alphabet)))])])
     (printf "~a~n"
             (case (output-format)
               [("num") (digits->string digits alphabet)]
               [else digits])))
   (unless number
     (close-input-port (current-input-port)))))
