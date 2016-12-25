#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out typed/racket/draw typed/racket/class))
(provide (all-from-out racket/fixnum racket/flonum racket/math))

(require typed/racket/draw)
(require typed/racket/class)

(require racket/fixnum)
(require racket/flonum)
(require racket/math)

(require digimon/cheat)

(define-type Color (Instance Color%))
(define-type Color+sRGB (U Index Symbol String Color))
(define-type Bitmap (Instance Bitmap%))
(define-type Font (Instance Font%))

(define-cheat-opaque color%? #:is-a? Color% color%)
(define-cheat-opaque bitmap%? #:is-a? Bitmap% bitmap%)
(define-cheat-opaque font%? #:is-a? Font% font%)

(define the-dc (make-object bitmap-dc% (make-object bitmap% 1 1)))
(define the-invalid-image : Bitmap (read-bitmap (open-input-bytes #"placeholder")))
(define default-css-font : (Parameterof Font) (make-parameter (make-font)))

(define smart-font-size : (-> Font Nonnegative-Flonum)
  (let ([macosx? (eq? (system-type 'os) 'macosx)])
    (lambda [font]
      (define size : Nonnegative-Flonum (real->double-flonum (send font get-size)))
      (case (if (or macosx? (send font get-size-in-pixels)) 'px 'pt)
        [(pt) (fl* size (fl/ 96.0 72.0))]
        [else size]))))
