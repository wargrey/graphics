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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (define color->name : (-> Natural Keyword)
    (lambda [color]
      (string->keyword (string-upcase (~r color #:base 16 #:min-width 6 #:pad-string "0")))))

  (define color->flbyte : (-> Flonum Flonum)
    (lambda [color]
      (define flbyte : Flonum (fl* color 255.0))
      (fl/ (round (fl* flbyte 1000.0)) 1000.0)))
  
  (define datum=? : (-> Flonum Flonum * Boolean)
    (lambda [src . res]
      (cond [(ormap flnan? (cons src res)) (andmap flnan? (cons src res))]
            [else (for/and ([r (in-list res)])
                    (ormap (λ [[m : Flonum]] (eq? (exact-round (fl* src m)) (exact-round (fl* r m))))
                           (list 1000.0 100.0 10.0)))])))

  (define examples : (Listof (List Natural Gamut Gamut Gamut Hue Hue Gamut Gamut Gamut Gamut Gamut Gamut Gamut))
    (list (list #xFFFFFF  1.000  1.000  1.000  +nan.0  +nan.0  1.000  1.000  1.000  1.000  0.000  0.000  0.000)
          (list #x808080  0.500  0.500  0.500  +nan.0  +nan.0  0.500  0.500  0.500  0.500  0.000  0.000  0.000)
          (list #x000000  0.000  0.000  0.000  +nan.0  +nan.0  0.000  0.000  0.000  0.000  0.000  0.000  0.000)
          (list #xFF0000  1.000  0.000  0.000  0.0     0.0     1.000  0.500  0.333  0.299  1.000  1.000  1.000)
          (list #xBFBF00  0.750  0.750  0.000  60.0    60.0    0.750  0.375  0.500  0.664  1.000  1.000  1.000)
          (list #x008000  0.000  0.500  0.000  120.0   120.0   0.500  0.250  0.167  0.293  1.000  1.000  1.000)
          (list #x80FFFF  0.500  1.000  1.000  180.0   180.0   1.000  0.750  0.833  0.850  0.500  1.000  0.400)
          (list #x8080FF  0.500  0.500  1.000  240.0   240.0   1.000  0.750  0.667  0.557  0.500  1.000  0.250)
          (list #xBF40BF  0.750  0.250  0.750  300.0   300.0   0.750  0.500  0.583  0.457  0.667  0.500  0.571)
          (list #xA0A424  0.628  0.643  0.142  61.8    61.5    0.643  0.393  0.471  0.581  0.779  0.638  0.699)
          (list #x411BEA  0.255  0.104  0.918  251.1   250.0   0.918  0.511  0.426  0.242  0.887  0.832  0.756)
          (list #x1EAC41  0.116  0.675  0.255  134.9   133.8   0.675  0.396  0.349  0.460  0.828  0.707  0.667)
          (list #xF0C80E  0.941  0.785  0.053  49.5    50.5    0.941  0.497  0.593  0.748  0.944  0.893  0.911)
          (list #xB430E5  0.704  0.187  0.897  283.7   284.8   0.897  0.542  0.596  0.423  0.792  0.775  0.686)
          (list #xED7651  0.931  0.463  0.316  14.3    13.2    0.931  0.624  0.570  0.586  0.661  0.817  0.446)
          (list #xFEF888  0.998  0.974  0.532  56.9    57.4    0.998  0.765  0.835  0.931  0.467  0.991  0.363)
          (list #x19CB97  0.099  0.795  0.591  162.4   163.4   0.795  0.447  0.495  0.564  0.875  0.779  0.800)
          (list #x362698  0.211  0.149  0.597  248.3   247.3   0.597  0.373  0.319  0.219  0.750  0.601  0.533)
          (list #x7E7EB8  0.495  0.493  0.721  240.5   240.4   0.721  0.607  0.570  0.520  0.316  0.290  0.135)))
  
  (for/list : (Listof Datum) ([tc (in-list examples)])
    (define-values (color-name metrics) (values (color->name (first tc)) (take-right (cdr tc) 7)))
    (define-values (R  G  B  H  Hi) (values (second tc) (third tc) (fourth tc) (fifth tc) (sixth tc)))
    (define-values (V  L  I)        (values (first metrics) (second metrics) (third metrics)))
    (define-values (Sv Sl Si)       (values (fifth metrics) (sixth metrics) (seventh metrics)))
    (define-values (hv sv v)        (rgb->hsv R G B))
    (define-values (hl sl l)        (rgb->hsl R G B))
    (define-values (hi si i)        (rgb->hsi R G B))
    (define src : (Listof Flonum)   (list H  Hi  V  L  I  Sv  Sl  Si))
    (define hsb : (Listof Flonum)   (list hv hi  v  l  i  sv  sl  si))
    (cond [(andmap datum=? src hsb) (list (cons 'rgb->hsb color-name) R G B H Hi V L I Sv Sl Si)]
          [else (list* (cons 'rgb->nan color-name) R G B (map (inst cons Flonum Flonum) src hsb))]))

  (for/list : (Listof Datum) ([tc (in-list examples)])
    (define-values (color-name metrics) (values (color->name (first tc)) (take-right (cdr tc) 7)))
    (define-values (R  G  B  H  Hi) (values (second tc) (third tc) (fourth tc) (fifth tc) (sixth tc)))
    (define-values (V  L  I)        (values (first metrics) (second metrics) (third metrics)))
    (define-values (Sv Sl Si)       (values (fifth metrics) (sixth metrics) (seventh metrics)))
    (define-values (rv gv bv)       (hsv->rgb H  Sv  V))
    (define-values (rl gl bl)       (hsl->rgb H  Sl  L))
    (define-values (ri gi bi)       (hsi->rgb Hi Si  I))
    (define-values (rh gh bh)       (hsi->rgb H  Si  I))
    (define src : (Listof Flonum)   (list R  G  B))
    (define hsv : (Listof Flonum)   (list rv gv bv))
    (define hsl : (Listof Flonum)   (list rl gl bl))
    (define hsi : (Listof Flonum)   (list ri gi bi))
    (define hsh : (Listof Flonum)   (list rh gh bh))
    (define diff : (Listof Datum) (map (λ [[i : Flonum] [h : Flonum]] (cons (color->flbyte h) (color->flbyte i))) hsi hsh))
    (cond [(andmap datum=? src hsv hsl hsi) (list* (cons 'hsb->rgb color-name) R G B (cons H Hi) diff)]
          [else (cons (cons 'hsb->nan color-name) (map (inst list Flonum) src hsv hsl hsi))]))

  (for/list : (Listof Datum) ([tc (in-list examples)])
    (define-values (color-name metrics) (values (color->name (first tc)) (take-right (cdr tc) 7)))
    (define-values (R  G  B  H)   (values (second tc) (third tc) (fourth tc) (fifth tc)))
    (define-values (h w black)    (rgb->hwb R G B))
    (define-values (r g b)        (hwb->rgb h w black))
    (define src : (Listof Flonum) (list R G B H))
    (define hwb : (Listof Flonum) (list r g b h))
    (cond [(andmap datum=? src hwb) (list (cons 'rgb<>hwb color-name) R G B H)]
          [else (cons (cons 'hwb<>nan color-name) (map (inst cons Flonum Flonum) src hwb))])))
