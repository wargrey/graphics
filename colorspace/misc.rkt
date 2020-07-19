#lang typed/racket/base

(provide (all-defined-out))

(require racket/fixnum)
(require racket/math)

(define-type HSB->RGB (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define-type RGB->HSB (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))

(define gamut->byte : (-> Flonum Byte) (λ [r] (min (max (exact-round (* r 255.0)) 0) #xFF)))
(define byte->gamut : (-> Byte Flonum) (λ [r] (/ (exact->inexact r) 255.0)))
(define real->gamut : (-> Real Flonum) (λ [r] (max (min (real->double-flonum r) 1.0) 0.0)))
(define real->alpha : (-> Real Flonum) (λ [r] (max (min (real->double-flonum r) 1.0) 0.0)))
(define gamut->uint16 : (-> Flonum Index) (λ [r] (assert (min (max (exact-round (* r 65535.0)) 0) 65535) index?)))

(define real->hue : (-> Real Flonum)
  (lambda [hue]
    (cond [(nan? hue) +nan.0]
          [(and (<= 0 hue) (< hue 360)) (real->double-flonum hue)]
          [else (let ([integer-part (modulo (exact-truncate hue) 360)])
                  (cond [(integer? hue) (real->double-flonum integer-part)]
                        [(positive? hue) (real->double-flonum (+ integer-part (- hue (truncate hue))))]
                        [(zero? integer-part) (+ 360.0 (real->double-flonum (- hue (truncate hue))))]
                        [else (real->double-flonum (- integer-part (- (truncate hue) hue)))]))])))

(define rgb-bytes->hex : (-> Byte Byte Byte Index)
  (lambda [r g b]
    (fxand #xFFFFFF
           (fxior (fxlshift r 16)
                  (fxior (fxlshift g 8)
                         b)))))

(define hex->rgb-bytes : (-> Integer (Values Byte Byte Byte))
  (lambda [rgb]
    (values (fxand (fxrshift rgb 16) #xFF)
            (fxand (fxrshift rgb 8) #xFF)
            (fxand rgb #xFF))))

(define rgb-gamuts->hex : (-> Flonum Flonum Flonum Index)
  (lambda [r g b]
    (rgb-bytes->hex (gamut->byte r) (gamut->byte g) (gamut->byte b))))

(define hex->rgb-gamuts : (-> Integer (Values Flonum Flonum Flonum))
  (lambda [rgb]
    (define-values (r g b) (hex->rgb-bytes rgb))
    (values (byte->gamut r) (byte->gamut g) (byte->gamut b))))

(define rgb-bytes->hsb : (-> RGB->HSB Byte Byte Byte (Values Flonum Flonum Flonum))
  (lambda [rgb->hsb red green blue]
    (rgb->hsb (byte->gamut red)
              (byte->gamut green)
              (byte->gamut blue))))

(define hsb->rgb-bytes : (-> HSB->RGB Real Real Real (Values Byte Byte Byte))
  (lambda [hsb->rgb hue s% b%]
    (define-values (red green blue) (hsb->rgb (real->hue hue) (real->gamut s%) (real->gamut b%)))
    (values (gamut->byte red)
            (gamut->byte green)
            (gamut->byte blue))))

(define rgb-hex->hsb : (-> RGB->HSB Integer (Values Flonum Flonum Flonum))
  (lambda [rgb->hsb hex]
    (define-values (red green blue) (hex->rgb-bytes hex))
    (rgb-bytes->hsb rgb->hsb red green blue)))

(define hsb->rgb-hex : (-> HSB->RGB Real Real Real Index)
  (lambda [hsb->rgb hue s% b%]
    (define-values (red green blue) (hsb->rgb-bytes hsb->rgb hue s% b%))
    (rgb-bytes->hex red green blue)))
