#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out bitmap/digitama/digicore))
(provide (all-from-out typed/images/icons))

(require bitmap/digitama/digicore)
(require typed/images/icons)

(define-predicate racket-font-smoothing? Font-Smoothing)
(define-predicate racket-font-hinting? Font-Hinting)
