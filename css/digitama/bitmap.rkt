#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out bitmap/digitama/bitmap))
(provide (all-from-out typed/images/icons))

(require bitmap/digitama/bitmap)
(require typed/images/icons)

(define-predicate racket-font-smoothing? Font-Smoothing)
(define-predicate racket-font-hinting? Font-Hinting)

(define default-css-invalid-image : (Parameterof Bitmap) (make-parameter (x-icon)))
