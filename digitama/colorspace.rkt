#lang typed/racket

(provide (all-defined-out))

(require math/flonum)

(define-type Gamut Nonnegative-Flonum)
(define-type Hue Nonnegative-Flonum)

(define-type HSB->RGB (-> Hue Gamut Gamut (Values Gamut Gamut Gamut)))
(define-type RGB->HSB (-> Gamut Gamut Gamut (Values Hue Gamut Gamut)))

(define rgb-bytes->hsb : (-> RGB->HSB Byte Byte Byte (Values Hue Gamut Gamut))
  (lambda [rgb->hsb red green blue]
    (rgb->hsb (fl (/ red   #xFF))
              (fl (/ green #xFF))
              (fl (/ blue  #xFF)))))

(define hsb->rgb-bytes : (-> HSB->RGB Hue Gamut Gamut (Values Byte Byte Byte))
  (lambda [hsb->rgb hue s% %]
    (define-values (red green blue) (hsb->rgb hue s% %))
    (values (min (exact-round (fl* red   255.0)) #xFF)
            (min (exact-round (fl* green 255.0)) #xFF)
            (min (exact-round (fl* blue  255.0)) #xFF))))

(define hsb-normalize : (-> Real Real Real (Values Hue Gamut Gamut))
  (lambda [hue s% b%]
    (values (cond [(nan? hue) +nan.0]
                  [(or (zero? hue) (and (positive? hue) (< hue 360))) (fl hue)]
                  [else (let ([integer-part (modulo (exact-truncate hue) 360)])
                          (cond [(integer? hue) (fl integer-part)]
                                [(positive? hue) (flabs (fl (+ integer-part (- hue (truncate hue)))))]
                                [(zero? integer-part) (flabs (fl+ 360.0 (fl (- hue (truncate hue)))))]
                                [else (flabs (fl (- integer-part (- (truncate hue) hue))))]))])
            (flmax (flmin (fl s%) 1.0) 0.0)
            (flmax (flmin (fl b%) 1.0) 0.0))))

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
    (define chroma : Flonum 1.0)
    (define m : Flonum (fl* (fl- 1.0 saturation) intensity))
    (hue->rgb hue chroma m)))
  
(define rgb->hsi : RGB->HSB
  (lambda [red green blue]
    (define-values (M m chroma hue) (rgb->hue red green blue))
    (define intensity : Flonum (fl/ (fl+ (fl+ red green) blue) 3.0))
    (define saturation : Flonum (if (zero? chroma) 0.0 (fl- 1.0 (fl/ m intensity))))
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
    (define hue/60 : Flonum (if (nan? hue) 6.0 (fl/ hue 60.0)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (define datum=? : (-> Flonum Flonum * Boolean)
    (lambda [src . res]
      (cond [(ormap nan? (cons src res)) (andmap nan? (cons src res))]
            [else (for/and ([r (in-list res)])
                    (ormap (λ [[m : Flonum]] (eq? (exact-round (fl* src m)) (exact-round (fl* r m))))
                           (list 1000.0 100.0 10.0)))])))

  (define examples : (Listof (List Gamut Gamut Gamut Gamut Gamut Gamut Gamut Gamut Gamut Gamut))
    (list (list 1.000   1.000   1.000   +nan.0   1.000   1.000   1.000   0.000   0.000   0.000)
          (list 0.500   0.500   0.500   +nan.0   0.500   0.500   0.500   0.000   0.000   0.000)
          (list 0.000   0.000   0.000   +nan.0   0.000   0.000   0.000   0.000   0.000   0.000)
          (list 1.000   0.000   0.000   0.0      1.000   0.500   0.333   1.000   1.000   1.000)
          (list 0.750   0.750   0.000   60.0     0.750   0.375   0.500   1.000   1.000   1.000)
          (list 0.000   0.500   0.000   120.0    0.500   0.250   0.167   1.000   1.000   1.000)
          (list 0.500   1.000   1.000   180.0    1.000   0.750   0.833   0.500   1.000   0.400)
          (list 0.500   0.500   1.000   240.0    1.000   0.750   0.667   0.500   1.000   0.250)
          (list 0.750   0.250   0.750   300.0    0.750   0.500   0.583   0.667   0.500   0.571)
          (list 0.628   0.643   0.142   61.8     0.643   0.393   0.471   0.779   0.638   0.699)
          (list 0.255   0.104   0.918   251.1    0.918   0.511   0.426   0.887   0.832   0.756)
          (list 0.116   0.675   0.255   134.9    0.675   0.396   0.349   0.828   0.707   0.667)
          (list 0.941   0.785   0.053   49.5     0.941   0.497   0.593   0.944   0.893   0.911)
          (list 0.704   0.187   0.897   283.7    0.897   0.542   0.596   0.792   0.775   0.686)
          (list 0.931   0.463   0.316   14.3     0.931   0.624   0.570   0.661   0.817   0.446)
          (list 0.998   0.974   0.532   56.9     0.998   0.765   0.835   0.467   0.991   0.363)
          (list 0.099   0.795   0.591   162.4    0.795   0.447   0.495   0.875   0.779   0.800)
          (list 0.211   0.149   0.597   248.3    0.597   0.373   0.319   0.750   0.601   0.533)
          (list 0.495   0.493   0.721   240.5    0.721   0.607   0.570   0.316   0.290   0.135)))
  
  (for/list : (Listof Datum) ([tc (in-list examples)])
    (define-values (R  G  B  H) (values (first tc) (second tc) (third tc) (fourth tc)))
    (define-values (V  L  I)    (values (fifth tc) (sixth tc) (seventh tc)))
    (define-values (Sv Sl Si)   (values (eighth tc) (ninth tc) (tenth tc)))
    (define-values (hv sv v) (rgb->hsv R G B))
    (define-values (hl sl l) (rgb->hsl R G B))
    (define-values (ho si i) (rgb->hsi R G B))
    (define src : (Listof Flonum) (list H  V L I Sv Sl Si))
    (define hsb : (Listof Flonum) (list hv v l i sv sl si))
    (cond [(andmap datum=? src hsb) (cons #true tc)]
          [else (list* #false R G B (map (inst cons Flonum Flonum) src hsb))]))

  (for/list : (Listof Datum) ([tc (in-list examples)])
    (define-values (R  G  B  H) (values (first tc) (second tc) (third tc) (fourth tc)))
    (define-values (V  L  I)    (values (fifth tc) (sixth tc) (seventh tc)))
    (define-values (Sv Sl Si)   (values (eighth tc) (ninth tc) (tenth tc)))
    (define-values (rv gv bv) (hsv->rgb H Sv V))
    (define-values (rl gl bl) (hsl->rgb H Sl L))
    (define-values (ri gi bi) (hsi->rgb H Si I))
    (define src : (Listof Flonum) (list R  G  B))
    (define hsv : (Listof Flonum) (list rv gv bv))
    (define hsl : (Listof Flonum) (list rl gl bl))
    (define hsi : (Listof Flonum) (list ri gi bi))
    (cond [(andmap datum=? src hsv hsl hsi) (list #true R G B H)]
          [else (cons #false (map (inst list Flonum) src hsv hsl hsi))]))

  (for/list : (Listof Datum) ([tc (in-list examples)])
    (define-values (R  G  B  H) (values (first tc) (second tc) (third tc) (fourth tc)))
    (define-values (h w black) (rgb->hwb R G B))
    (define-values (r g b) (hwb->rgb h w black))
    (define src : (Listof Flonum) (list R G B H))
    (define hwb : (Listof Flonum) (list r g b h))
    (cond [(andmap datum=? src hwb) (list #true R G B H)]
          [else (list* #false (map (inst list Flonum) src hwb))])))
