#lang scribble/text
#lang racket/base

(provide (all-defined-out))

(define desc "@getenv{digivice-desc}")

{module @getenv{digivice-name} racket/base
  (require syntax/location)
  (require racket/cmdline)
  (require racket/path)
  
  (command-line
   #:program (file-name-from-path (cadr (quote-module-path)))
   #:once-each
   [("-v" "--version") "Print version information"
                       (displayln "version: 0.618.")]
   #:args rest-argv (displayln rest-argv))}
