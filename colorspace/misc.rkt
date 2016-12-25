#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)
(require racket/flonum)
(require racket/math)

(define-type Gamut Nonnegative-Flonum)
(define-type Hue Nonnegative-Flonum)

(define-type HSB->RGB (-> Hue Gamut Gamut (Values Gamut Gamut Gamut)))
(define-type RGB->HSB (-> Gamut Gamut Gamut (Values Hue Gamut Gamut)))

(define gamut->byte : (-> Gamut Byte) (λ [r] (min (exact-round (fl* r 255.0)) #xFF)))
(define real->gamut : (-> Real Gamut) (λ [r] (flmax (flmin (real->double-flonum r) 1.0) 0.0)))

(define real->hue : (-> Real Hue)
  (lambda [hue]
    (cond [(nan? hue) +nan.0]
          [(or (zero? hue) (and (positive? hue) (< hue 360))) (real->double-flonum hue)]
          [else (let ([integer-part (modulo (exact-truncate hue) 360)])
                  (cond [(integer? hue) (real->double-flonum integer-part)]
                        [(positive? hue) (flabs (real->double-flonum (+ integer-part (- hue (truncate hue)))))]
                        [(zero? integer-part) (flabs (fl+ 360.0 (real->double-flonum (- hue (truncate hue)))))]
                        [else (flabs (real->double-flonum (- integer-part (- (truncate hue) hue))))]))])))

(define rgb-bytes->hex : (-> Byte Byte Byte Index)
  (lambda [r g b]
    (fxand #xFFFFFF
           (fxior (fxlshift r 16)
                  (fxior (fxlshift g 8)
                         b)))))

(define hex->rgb-bytes : (-> Index (Values Byte Byte Byte))
  (lambda [rgb]
    (values (fxand (fxrshift rgb 16) #xFF)
            (fxand (fxrshift rgb 8) #xFF)
            (fxand rgb #xFF))))

(define rgb-bytes->hsb : (-> RGB->HSB Byte Byte Byte (Values Hue Gamut Gamut))
  (lambda [rgb->hsb red green blue]
    (rgb->hsb (real->double-flonum (/ red   #xFF))
              (real->double-flonum (/ green #xFF))
              (real->double-flonum (/ blue  #xFF)))))

(define hsb->rgb-bytes : (-> HSB->RGB Real Real Real (Values Byte Byte Byte))
  (lambda [hsb->rgb hue s% b%]
    (define-values (red green blue) (hsb->rgb (real->hue hue) (real->gamut s%) (real->gamut b%)))
    (values (gamut->byte red)
            (gamut->byte green)
            (gamut->byte blue))))

(define rgb-hex->hsb : (-> RGB->HSB Index (Values Hue Gamut Gamut))
  (lambda [rgb->hsb hex]
    (define-values (red green blue) (hex->rgb-bytes hex))
    (rgb-bytes->hsb rgb->hsb red green blue)))

(define hsb->rgb-hex : (-> HSB->RGB Real Real Real Index)
  (lambda [hsb->rgb hue s% b%]
    (define-values (red green blue) (hsb->rgb-bytes hsb->rgb hue s% b%))
    (rgb-bytes->hex red green blue)))
