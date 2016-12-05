#lang typed/racket

(provide (all-defined-out))

(require racket/fixnum)
(require math/flonum)

(define-type Gamut Nonnegative-Flonum)
(define-type Hue Nonnegative-Flonum)

(define-type HSB->RGB (-> Hue Gamut Gamut (Values Gamut Gamut Gamut)))
(define-type RGB->HSB (-> Gamut Gamut Gamut (Values Hue Gamut Gamut)))

(define gamut->byte : (-> Gamut Byte) (λ [r] (min (exact-round (fl* r 255.0)) #xFF)))
(define real->gamut : (-> Real Gamut) (λ [r] (flmax (flmin (fl r) 1.0) 0.0)))

(define real->hue : (-> Real Hue)
  (lambda [hue]
    (cond [(nan? hue) +nan.0]
          [(or (zero? hue) (and (positive? hue) (< hue 360))) (fl hue)]
          [else (let ([integer-part (modulo (exact-truncate hue) 360)])
                  (cond [(integer? hue) (fl integer-part)]
                        [(positive? hue) (flabs (fl (+ integer-part (- hue (truncate hue)))))]
                        [(zero? integer-part) (flabs (fl+ 360.0 (fl (- hue (truncate hue)))))]
                        [else (flabs (fl (- integer-part (- (truncate hue) hue))))]))])))

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
    (rgb->hsb (fl (/ red   #xFF))
              (fl (/ green #xFF))
              (fl (/ blue  #xFF)))))

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
    (values (flabs hue) (flabs saturation) (flabs value))))

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
    (values (flabs hue) (flabs saturation) (flabs lightness))))

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
    (values (flabs hue) (flabs saturation) (flabs intensity))))

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
                  (hsv->rgb hue (flabs saturation) (flabs value)))])))
  
(define rgb->hwb : RGB->HSB
  (lambda [red green blue]
    (define-values (hue saturation value) (rgb->hsv red green blue))
    (define white : Flonum (fl* (fl- 1.0 saturation) value))
    (define black : Flonum (fl- 1.0 value))
    (values hue (flabs white) (flabs black))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rgb->hue : (-> Gamut Gamut Gamut (Values Flonum Flonum Flonum Flonum))
  (lambda [red green blue]
    (define-values (M m) (values (max red green blue) (min red green blue)))
    (define chroma : Flonum (- M m))
    (define hue : Flonum
      (cond [(zero? chroma)   +nan.0]
            [(fl= M green)    (fl* 60.0 (fl+ (fl/ (fl- blue red)   chroma) 2.0))]
            [(fl= M blue)     (fl* 60.0 (fl+ (fl/ (fl- red green)  chroma) 4.0))]
            [(fl< green blue) (fl* 60.0 (fl+ (fl/ (fl- green blue) chroma) 6.0))]
            [else             (fl* 60.0      (fl/ (fl- green blue) chroma))]))
    (values M m chroma hue)))

(define hue->rgb : (-> Hue Flonum Flonum (Values Gamut Gamut Gamut))
  (lambda [hue chroma m]
    (define hue/60 : Flonum (if (flnan? hue) 6.0 (fl/ hue 60.0)))
    (define hue. : Integer (exact-floor hue/60))
    (define x : Flonum (* chroma (- 1 (abs (- (remainder hue. 2) (- hue. hue/60) 1))))) ; X = C(1-|H' mod 2 - 1|)
    (define-values (red green blue)
      (case hue.
        [(0) (values chroma x 0.0)]
        [(1) (values x chroma 0.0)]
        [(2) (values 0.0 chroma x)]
        [(3) (values 0.0 x chroma)]
        [(4) (values x 0.0 chroma)]
        [(5) (values chroma 0.0 x)]
        [else (values 0.0 0.0 0.0)]))
    (values (flabs (fl+ red m)) (flabs (fl+ green m)) (flabs (fl+ blue m)))))

(define hsi-sector->rgb : (-> Flonum Flonum Flonum (U 'red 'green 'blue) (Values Gamut Gamut Gamut))
  (lambda [hue saturation intensity color]
    (define flcosH/cos60-H : Flonum
      (cond [(or (zero? hue) (fl= hue 120.0)) 2.0]
            [else (let ([H (fl* hue (fl/ pi 180.0))])
                    (fl/ (flcos H) (flcos (fl- (fl/ pi 3.0) H))))]))
    (define major : Flonum (fl* intensity (fl+ 1.0 (fl* saturation flcosH/cos60-H))))
    (define midor : Flonum (fl* intensity (fl- 1.0 saturation)))
    (define minor : Flonum (fl- (fl* 3.0 intensity) (fl+ major midor)))
    (case color
      [(red)   (values (flabs major) (flabs minor) (flabs midor))]
      [(green) (values (flabs midor) (flabs major) (flabs minor))]
      [else    (values (flabs minor) (flabs midor) (flabs major))])))
