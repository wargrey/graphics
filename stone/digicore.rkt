#lang scribble/text

@(require "../digitama/digicore.rkt")

#lang typed/racket

(require "@(getenv "digicore.rkt")")

(provide (all-defined-out))
(provide (all-from-out "@(getenv "digicore.rkt")"))

(current-digimon "@(current-digimon)")
