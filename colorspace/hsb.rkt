#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require math/flonum)

(require "misc.rkt")
(require "digitama/hsb.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hsv->rgb : HSB->RGB
  (lambda [hue saturation value]
    (define chroma : Flonum (fl* value saturation))
    (define m : Flonum (fl- value chroma))
    (hue->rgb hue chroma m)))

(define rgb->hsv : RGB->HSB
  (lambda [red green blue]
    (define-values (M m chroma hue) (rgb->hue red green blue))
    (define value : Flonum M)
    (define saturation : Flonum (if (zero? chroma) 0.0 (fl/ chroma M)))
    (values hue saturation value)))

(define hsl->rgb : HSB->RGB
  (lambda [hue saturation lightness]
    (define chroma : Flonum (fl* (fl- 1.0 (flabs (fl- (fl* 2.0 lightness) 1.0))) saturation)) ; C = S(1 - |2L - 1|)
    (define m : Flonum (fl- lightness (fl/ chroma 2.0)))
    (hue->rgb hue chroma m)))

(define rgb->hsl : RGB->HSB
  (lambda [red green blue]
    (define-values (M m chroma hue) (rgb->hue red green blue))
    (define lightness : Flonum (fl* 0.5 (fl+ M m)))
    (define saturation : Flonum (if (zero? chroma) 0.0 (fl/ chroma (fl- 1.0 (flabs (fl- (fl* 2.0 lightness) 1.0))))))
    (values hue saturation lightness)))

(define hsi->rgb : HSB->RGB
  (lambda [hue saturation intensity]
    (cond [(or (zero? saturation) (flnan? hue)) (values intensity intensity intensity)]
          [(fl< hue 120.0) (hsi-sector->rgb hue saturation intensity 'red)]
          [(fl< hue 240.0) (hsi-sector->rgb (fl- hue 120.0) saturation intensity 'green)]
          [else (hsi-sector->rgb (fl- hue 240.0) saturation intensity 'blue)])))
  
(define rgb->hsi : RGB->HSB
  (lambda [red green blue]
    (define α : Flonum (fl- red (fl* 0.5 (fl+ green blue))))
    (define hue : Flonum
      (let* ([β (flsqrt (fl+ (flexpt (fl- red green) 2.0) (fl* (fl- red blue) (fl- green blue))))]
             [h (fl* (flacos (fl/ α β)) (fl/ 180.0 pi))])
        (if (fl> blue green) (fl- 360.0 h) h)))
    (define hue/dead-code : Flonum ;;; This algorithm is also okay regardless the performance and (almost not less) precision
      (let* ([β (fl* (flsqrt 0.75) (fl- green blue))]
             [chroma (flhypot α β)])
        (define hue : Flonum
          (cond [(fl> α 0.0) (fl* 2.0 (atan (fl- chroma α) β))]
                [(not (zero? β)) (fl* 2.0 (atan β (fl+ chroma α)))]
                [else (if (zero? α) +nan.0 pi)]))
        (fl* (fl/ 180.0 pi) (if (fl< hue 0.0) (fl+ (fl* 2.0 pi) hue) hue))))
    (define intensity : Flonum (fl/ (fl+ (fl+ red green) blue) 3.0))
    (define saturation : Flonum (if (zero? intensity) 0.0 (fl- 1.0 (fl/ (min red green blue) intensity))))
    (values hue saturation intensity)))

(define hwb->rgb : HSB->RGB
  (lambda [hue w b]
    (define-values (white black)
      (let ([w+b (fl+ w b)])
        (cond [(fl<= w+b 1.0) (values w b)]
              [else (let ([ratio (fl/ 1.0 w+b)])
                      (values (fl* w ratio) (fl* b ratio)))])))
    (cond [(fl= black 1.0) (values 0.0 0.0 0.0)]
          [else (let* ([value (fl- 1.0 black)]
                       [saturation (fl- 1.0 (fl/ white value))])
                  (hsv->rgb hue saturation value))])))
  
(define rgb->hwb : RGB->HSB
  (lambda [red green blue]
    (define-values (hue saturation value) (rgb->hsv red green blue))
    (define white : Flonum (fl* (fl- 1.0 saturation) value))
    (define black : Flonum (fl- 1.0 value))
    (values hue white black)))
