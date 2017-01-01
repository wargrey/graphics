#lang typed/racket/base

(provide (all-defined-out) select-color)

(require "digitama/bitmap.rkt")
(require "digitama/color.rkt")
(require "constructor.rkt")

(require typed/images/icons)

(define bitmap-size : (case-> [Bitmap -> (Values Positive-Integer Positive-Integer)]
                              [Bitmap Nonnegative-Real -> (Values Nonnegative-Real Nonnegative-Real)]
                              [Bitmap Nonnegative-Real Nonnegative-Real -> (Values Nonnegative-Real Nonnegative-Real)])
  (case-lambda [(bmp) (values (send bmp get-width) (send bmp get-height))]
               [(bmp ratio) (values (* (send bmp get-width) ratio) (* (send bmp get-height) ratio))]
               [(bmp w-ratio h-ratio) (values (* (send bmp get-width) w-ratio) (* (send bmp get-height) h-ratio))]))

(define bitmap-size+density : (Bitmap -> (Values Positive-Integer Positive-Integer Positive-Real))
  (lambda [bmp]
    (values (send bmp get-width)
            (send bmp get-height)
            (send bmp get-backing-scale))))

(define bitmap-intrinsic-size : (-> Bitmap (Values Positive-Integer Positive-Integer))
  (lambda [bmp]
    (define density : Positive-Real (send bmp get-backing-scale))
    (values (max (exact-ceiling (* (send bmp get-width) density)) 1)
            (max (exact-ceiling (* (send bmp get-height) density)) 1))))

(define bitmap-alter-density : (->* (Bitmap) (Positive-Real) Bitmap)
  (lambda [raw [density (default-icon-backing-scale)]]
    (define ratio : Nonnegative-Real (/ (send raw get-backing-scale) density))
    (cond [(= ratio 1.0) raw]
          [else (let ([bmp (bitmap-blank (* (send raw get-width) ratio) (* (send raw get-height) ratio) density)])
                  ; This algorithm is much faster than the (get/set-argb-pixels) one
                  (define dc : (Instance Bitmap-DC%) (send bmp make-dc))
                  (send dc set-smoothing 'aligned)
                  (send dc set-scale ratio ratio)
                  (send dc draw-bitmap raw 0 0)
                  bmp)])))

(define make-font+ : (->* () (Font #:size Real #:face (Option String) #:size-in-pixels? (U Boolean Symbol) #:family (Option Font-Family)
                                   #:style (Option Font-Style) #:weight (Option Font-Weight) #:hinting (Option Font-Hinting)
                                   #:underlined? (U Boolean Symbol) #:smoothing (Option Font-Smoothing)) Font)
  (lambda [[basefont (default-css-font)] #:size [size +nan.0] #:face [face #false] #:family [family #false]
           #:style [style #false] #:weight [weight #false] #:hinting [hinting #false] #:smoothing [smoothing #false]
           #:underlined? [underlined 'default] #:size-in-pixels? [size-in-pixels? 'default]]
    ;;; NOTE: Racket provides extra term `family` to make the font% cross platform, in practical, these two terminologies are referring
    ;;;        the same thing, thus, ask the basefont for inheriting the `font description string` only when both of them are unset.
    (define ?face : (Option String) (or face (and (not family) (send basefont get-face))))
    (define underlined? : Boolean (if (boolean? underlined) underlined (send basefont get-underlined)))
    (define pixels? : Boolean (if (boolean? size-in-pixels?) size-in-pixels? (send basefont get-size-in-pixels)))
    (define fontsize : Real (min 1024.0 (cond [(positive? size) size]
                                              [(or (zero? size) (nan? size)) (smart-font-size basefont)]
                                              [else (* (- size) (smart-font-size basefont))])))
    (if (string? ?face)
        (send the-font-list find-or-create-font fontsize ?face
              (or family (send basefont get-family)) (or style (send basefont get-style))
              (or weight (send basefont get-weight)) underlined?
              (or smoothing (send basefont get-smoothing)) pixels?
              (or hinting (send basefont get-hinting)))
        (send the-font-list find-or-create-font fontsize
              (or family (send basefont get-family)) (or style (send basefont get-style))
              (or weight (send basefont get-weight)) underlined?
              (or smoothing (send basefont get-smoothing)) pixels?
              (or hinting (send basefont get-hinting))))))
