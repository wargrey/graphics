#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out typed/racket/draw))
(provide (all-from-out racket/fixnum racket/flonum racket/math))

(require typed/racket/draw)

(require racket/fixnum)
(require racket/flonum)
(require racket/math)

(require "cheat.rkt")

(define-type Color+sRGB (U String Symbol Integer (Instance Color%)))
(define-type Bitmap (Instance Bitmap%))

(define-cheat-opaque color%? #:is-a? Color% color%)
(define-cheat-opaque bitmap%? #:is-a? Bitmap% bitmap%)
(define-cheat-opaque font%? #:is-a? Font% font%)

(define default-bitmap-density : (Parameterof Positive-Flonum) (make-parameter 2.0))
(define default-bitmap-icon-height : (Parameterof Nonnegative-Flonum) (make-parameter 24.0))

(define-type Brush+Color (U Color+sRGB (Pairof Color+sRGB Brush-Style) (Instance Brush%)))
(define-type Pen+Color (U Color+sRGB (Pairof Color+sRGB Pen-Style) (Pairof Color+sRGB Nonnegative-Real)
                          (Pairof Color+sRGB (Pairof Nonnegative-Real Pen-Style)) (Instance Pen%)))

(define os : Symbol (system-type 'os))
(define the-dc : (Instance Bitmap-DC%) (make-object bitmap-dc% (make-object bitmap% 1 1)))
(define the-invalid-image : (Instance Bitmap%) (read-bitmap (open-input-bytes #"placeholder")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define smart-font-size : (-> (Instance Font%) Nonnegative-Flonum)
  (let ([macosx? (eq? os 'macosx)])
    (lambda [font]
      (define size : Nonnegative-Flonum (real->double-flonum (send font get-size)))
      (case (if (or macosx? (send font get-size-in-pixels)) 'px 'pt)
        [(pt) (fl* size (fl/ 96.0 72.0))]
        [else size]))))
