#lang typed/racket

(require "../main.rkt")

(require math/flonum)

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
        [else (cons (cons 'hwb<>nan color-name) (map (inst cons Flonum Flonum) src hwb))]))