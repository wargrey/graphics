#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out bitmap/digitama/digicore))

(require bitmap/digitama/digicore)

(define-predicate racket-font-smoothing? Font-Smoothing)
(define-predicate racket-font-hinting? Font-Hinting)
