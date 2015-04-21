#lang scribble/text

@(require "../digitama/digicore.rkt")

#lang typed/racket

(require "@(path->string (path-replace-suffix (getenv "digicore.rkt") ".typed.rkt"))")

(provide (all-defined-out))
(provide (all-from-out "@(path->string (path-replace-suffix (getenv "digicore.rkt") ".typed.rkt"))"))

(current-digimon "@(current-digimon)")
