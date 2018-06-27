#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require math/flonum)

(require "misc.rkt")
(require "digitama/hsb.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hsv->rgb : HSB->RGB
  (lambda [hue saturation value]
    (define chroma : Flonum (* value saturation))
    (define m : Flonum (- value chroma))
    (hue->rgb hue chroma m)))

(define rgb->hsv : RGB->HSB
  (lambda [red green blue]
    (define-values (M m chroma hue) (rgb->hue red green blue))
    (define value : Flonum M)
    (define saturation : Flonum (if (zero? value) 0.0 (/ chroma value)))
    (values hue saturation value)))

(define hsl->rgb : HSB->RGB
  (lambda [hue saturation lightness]
    (define chroma : Flonum (* (- 1.0 (abs (- (* 2.0 lightness) 1.0))) saturation)) ; C = S(1 - |2L - 1|)
    (define m : Flonum (- lightness (/ chroma 2.0)))
    (hue->rgb hue chroma m)))

(define rgb->hsl : RGB->HSB
  (lambda [red green blue]
    (define-values (M m chroma hue) (rgb->hue red green blue))
    (define lightness : Flonum (* 0.5 (+ M m)))
    (define saturation : Flonum (if (zero? chroma) 0.0 (/ chroma (- 1.0 (abs (- (* 2.0 lightness) 1.0))))))
    (values hue saturation lightness)))

(define hsi->rgb : HSB->RGB
  (lambda [hue saturation intensity]
    (cond [(or (zero? saturation) (nan? hue)) (values intensity intensity intensity)]
          [(< hue 120.0) (hsi-sector->rgb hue saturation intensity 'red)]
          [(< hue 240.0) (hsi-sector->rgb (- hue 120.0) saturation intensity 'green)]
          [else (hsi-sector->rgb (- hue 240.0) saturation intensity 'blue)])))
  
(define rgb->hsi : RGB->HSB
  (lambda [red green blue]
    (define α : Flonum (- red (* 0.5 (+ green blue))))
    (define hue : Flonum
      (let* ([β (flsqrt (+ (flexpt (- red green) 2.0) (* (- red blue) (- green blue))))]
             [h (* (flacos (/ α β)) (/ 180.0 pi))])
        (if (> blue green) (- 360.0 h) h)))
    #;(define hue/dead-code : Flonum ;;; This algorithm is also okay regardless the performance and (almost not less) precision
      (let* ([β (* (flsqrt 0.75) (- green blue))]
             [chroma (flhypot α β)])
        (define hue : Flonum
          (cond [(> α 0.0) (* 2.0 (atan (- chroma α) β))]
                [(not (zero? β)) (* 2.0 (atan β (+ chroma α)))]
                [else (if (zero? α) +nan.0 pi)]))
        (* (/ 180.0 pi) (if (< hue 0.0) (+ (* 2.0 pi) hue) hue))))
    (define intensity : Flonum (/ (+ red green blue) 3.0))
    (define saturation : Flonum (if (zero? intensity) 0.0 (- 1.0 (/ (min red green blue) intensity))))
    (values hue saturation intensity)))

(define hwb->rgb : HSB->RGB
  (lambda [hue w b]
    (define-values (white black)
      (let ([w+b (+ w b)])
        (cond [(<= w+b 1.0) (values w b)]
              [else (let ([ratio (/ 1.0 w+b)])
                      (values (* w ratio) (* b ratio)))])))
    (cond [(= black 1.0) (values 0.0 0.0 0.0)]
          [else (let* ([value (- 1.0 black)]
                       [saturation (- 1.0 (/ white value))])
                  (hsv->rgb hue saturation value))])))
  
(define rgb->hwb : RGB->HSB
  (lambda [red green blue]
    (define-values (hue saturation value) (rgb->hsv red green blue))
    (define white : Flonum (* (- 1.0 saturation) value))
    (define black : Flonum (- 1.0 value))
    (values hue white black)))
