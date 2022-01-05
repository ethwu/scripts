#! /usr/bin/env racket
#lang racket/base
(require hyphenate
         racket/cmdline
         racket/port)

; Joiner used to hyphenate.
(define joiner (make-parameter (integer->char 173)))
; Whether to hyphenate.
(define do-hyphenation (make-parameter #true))
; List of exceptions.
(define exceptions (make-parameter null))

(define parser
  (command-line
   #:usage-help "Hyphenate text using the Knuth–Liang hyphenation algorithm."
   #:once-each
   [("-j" "--joiner") JOINER "Specify joiner. Defaults to U+00AD Soft Hyphen." (joiner JOINER)]
   [("-u" "--unhyphenate") "Remove joiners." (do-hyphenation #false)]
   #:multi [("-s" "--specify")
            HYPHENATION
            "Specify hyphenation for a given string."
            (exceptions (cons HYPHENATION (exceptions)))]))

(let* ([stdin (port->string (current-input-port))] [stdout (current-output-port)] [joiner (joiner)])
  (display
   (if (do-hyphenation) (hyphenate #:exceptions (exceptions) stdin joiner) (unhyphenate stdin joiner))
   stdout))
