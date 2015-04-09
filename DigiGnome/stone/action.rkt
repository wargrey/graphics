#lang scribble/text
#lang racket/base

(provide (all-defined-out))

(define desc "@getenv{digivice-desc}")

{module @getenv{digivice-name} racket/base
  (require racket/cmdline)
  (require racket/path)
  
  (command-line
   #:program (file-name-from-path (syntax-source #'action))
   #:once-each
   [("-v" "--version") "Print version information"
                       (displayln "version: 0.618.")]
   #:args rest-argv (displayln rest-argv))}
