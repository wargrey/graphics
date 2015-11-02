#lang scribble/text

@(require "../digitama/digicore.rkt")

#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out "@(getenv "digicore.rkt")"))

(require "@(getenv "digicore.rkt")")

(current-digimon "@(current-digimon)")
