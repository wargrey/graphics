#lang typed/racket/base

(provide (all-defined-out))

(require digimon/number)

(require math/matrix)
(require math/flonum)

(require racket/string)
(require racket/fixnum)
(require racket/math)

(require "digitama/spectrum.rkt")
(require "correction.rkt")

;;; Resources
; http://cvrl.ioo.ucl.ac.uk/index.htm
; https://www.rit.edu/science/munsell-color-science-lab-educational-resources#useful-color-data

;;; Terminology and Theorem
; RGB, the RGB color mode
; XYZ, an awful-named imaginary 3-primary color mode to ensure that all colors are located in the first quadrant
;        This color mode maps the Spectural Power Distribution(SPD) to XYZ tristimulus values.
;        The matching functions themselves are defined by lookup tables rather than formula that could hardly be
;          calculated exactly. Thus, the integration(∫) of them is actually summation(Σ).
; xyY, a non-linear transformation of XYZ in a 2D plane, as the `Y` is designed to be the luminance of any color.
;        The `x`, `y` are chromaticity values related to `X` and `Y`,
;          and x + y + z = 1.0, x >= 0.0, y >= 0.0, z >= 0.0,
;          hence just a 2D plane.
;        The value of `Y` doesn't affect the resulting chromaticity diagram unless it is a 3D one.
; xbar, ybar, zbar, rbar, gbar, bbar are coefficients of their corresponding color components,
;   and they represent the mixing ratios of certain 3 parimaries;
;   also, they can be calculated with transpose matrices.

; Negative `rbar`, `gbar`, `bbar` means the target color is either out of gamut or cannot be mixed by
;  the 3-primary monochromatic light at certain wavelengths.
; Yes, it's true that not all colors can be produced by additive mixing with R, G, and B. Say,
;     Vibrant BlueGreen       = -38R + 42G + 9B
;  => Vibrant BlueGreen + 38R =        42G + 9B
; That is, in the color matching experiment, Some `R` must be added to the target color to trick human eyes.
; Here the coefficients are irradiance, as tristimulus values.

; Human eyes favor the Green light, hence the basic ratio of RGB are 1.0 : 4.5907 : 0.060 
; That's why the `Y` is designed for the luminance, and `X` and `Z` only contribute to the chroma.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CIE-RGB-Weight-Factors (Matrix Flonum))
(define-type CIE<->RGB (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define-type CIE-xyY->RGB (-> Flonum Flonum (Values Flonum Flonum Flonum Boolean)))
(define-type CIE-RGB->xyY (-> Flonum Flonum Flonum (Values Flonum Flonum)))
(define-type CIE-Filter (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define-type CIE-Ref.White (Immutable-Vector Flonum Flonum Flonum))
(define-type CIE-Illuminant-Type (U 'D65 'D50))

(struct CIE-observer
  ([type : Symbol]
   [λs : FlVector]
   [xbars : FlVector]
   [ybars : FlVector]
   [zbars : FlVector])
  #:type-name CIE-Observer
  #:transparent)

(struct CIE-illuminant
  ([type : Symbol]
   [λs : FlVector]
   [spds : FlVector])
  #:type-name CIE-Illuminant
  #:transparent)

(struct CIE-XYZ-matching-curves
  ([λstart : Flonum]
   [λend : Flonum]
   [count : Index]
   [X : FlVector]
   [Y : FlVector]
   [Z : FlVector])
  #:type-name CIE-XYZ-Matching-Curves
  #:transparent)

(define CIE-primary : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.49000 0.31000 0.20000
                                    0.17697 0.81240 0.01063
                                    0.00000 0.01000 0.99000)))

; For monitor, Daylight 6504K, bluish
(define CIE-sRGB-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.412453 0.357580 0.180423
                                    0.212671 0.715160 0.072169
                                    0.019334 0.119193 0.950227)))

(define CIE-XYZ->LMS-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.8190224379967030 0.3619062600528904 -0.1288737815209879
                                    0.0329836539323885 0.9292868615863434  0.0361446663506424
                                    0.0481771893596242 0.2642395317527308  0.6335478284694309)))

(define CIE-LMS->OKLAB-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.2104542683093140  0.7936177747023054 -0.0040720430116193
                                    1.9779985324311684 -2.4285922420485799  0.4505937096174110
                                    0.0259040424655478  0.7827717124575296 -0.8086757549230774)))

(define CIE-LMS->XYZ-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable  1.2268798758459243 -0.5578149944602171  0.2813910456659647
                                     -0.0405757452148008  1.1122868032803170 -0.0717110580655164
                                     -0.0763729366746601 -0.4214933324022432  1.5869240198367816)))

(define CIE-OKLAB->LMS-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 1.0000000000000000  0.3963377773761749  0.2158037573099136
                                    1.0000000000000000 -0.1055613458156586 -0.0638541728258133
                                    1.0000000000000000 -0.0894841775298119 -1.2914855480194092)))

(define CIE-XYZn-D65 : CIE-Ref.White #(0.9504559270516716 1.0 1.0890577507598784))

; For printed paper, Daylight 5003K, yellowish
(define CIE-sRGB-D50 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.4361 0.3851 0.1431
                                    0.2225 0.7169 0.0606
                                    0.0139 0.0971 0.7141)))

(define CIE-XYZn-D50 : CIE-Ref.White #(0.9642956764295677 1.0 0.8251046025104602))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-load-default-spectrum-samples : (->* () (Flonum Flonum) (Values CIE-Observer (List CIE-Illuminant CIE-Illuminant)))
  (lambda [[λmin 0.0] [λmax 1000.0]]
    (define src (collection-file-path "spectrum_1nm.csv" "colorspace" "stone"))

    (define rsamples : (List (Listof Flonum) (Listof Flonum) (Listof Flonum) (Listof Flonum) (Listof Flonum) (Listof Flonum))
      (call-with-input-file* src
        (λ [[/dev/csvin : Input-Port]]
          (for/fold ([ss : (List (Listof Flonum) (Listof Flonum) (Listof Flonum)
                                 (Listof Flonum) (Listof Flonum) (Listof Flonum))
                         (list null null null null null null)])
                    ([line (in-port read-line /dev/csvin)])
            (define tokens (string-split line ","))
            
            (cond [(> (length tokens) 8)
                   (let ([λself (string->number (car tokens))]
                         [A (string->number (list-ref tokens 1))]
                         [D (string->number (list-ref tokens 2))]
                         [xbar (string->number (list-ref tokens 5))]
                         [ybar (string->number (list-ref tokens 6))]
                         [zbar (string->number (list-ref tokens 7))])
                     (if (and (real? λself) (<= λmin λself λmax)
                              (real? A) (real? D)
                              (real? xbar) (real? ybar) (real? zbar))
                         (list (cons (real->double-flonum λself) (car ss))
                               (cons (real->double-flonum A) (cadr ss))
                               (cons (real->double-flonum D) (caddr ss))
                               (cons (real->double-flonum xbar) (list-ref ss 3))
                               (cons (real->double-flonum ybar) (list-ref ss 4))
                               (cons (real->double-flonum zbar) (list-ref ss 5)))
                         ss))]
                  [else ss])))))
        
    (let ([λs (list->flvector (reverse (car rsamples)))])
      (values (CIE-observer '2deg λs
                            (list->flvector (reverse (list-ref rsamples 3)))
                            (list->flvector (reverse (list-ref rsamples 4)))
                            (list->flvector (reverse (list-ref rsamples 5))))
              (list (CIE-illuminant 'A λs
                                    (list->flvector (reverse (list-ref rsamples 1))))
                    (CIE-illuminant 'D65 λs
                                    (list->flvector (reverse (list-ref rsamples 2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-RGB-normalize : CIE-Filter
  (lambda [r g b]
    ; Yes, negative colors are acceptable
    (define L (max r g b))
    (values (/ r L) (/ g L) (/ b L))))

(define CIE-XYZ-normalize : (-> Flonum Flonum Flonum CIE-Ref.White (Values Flonum Flonum Flonum))
  (lambda [X Y Z XYZn]
    (values (/ X (vector-ref XYZn 0))
            (/ Y (vector-ref XYZn 1))
            (/ Z (vector-ref XYZn 2)))))

;;; the f(t) used in the LAB RGB convertor 
(define CIE-lightness-transformation : (-> Flonum Flonum)
  (let* ([δ      6/29]
         [δ³     (* δ δ δ)]
         [δ⁻²/3  (/ (expt δ -2) 3)]
         [c      16/116])
    (lambda [t]
      (real->double-flonum
       (if (> t δ³)
           (expt t 1/3)
           (+ (* δ⁻²/3 t) c))))))

;;; the f⁻¹(t) used in the LAB RGB convertor
(define CIE-lightness-transformation⁻¹ : (-> Flonum Flonum)
  (let* ([δ   6/29]
         [3δ² (* 3 δ δ)]
         [c   4/29])
    (lambda [t]
      (real->double-flonum
       (if (> t δ)
           (expt t 3)
           (* 3δ² (- t c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-XYZ->xyY : (-> Flonum Flonum Flonum (Values Flonum Flonum))
  (lambda [X Y Z]
    (define L (+ X Y Z))
    
    (values (/ X L)
            (/ Y L))))

(define CIE-xyY->XYZ : (->* (Flonum Flonum) (Flonum) (Values Flonum Flonum Flonum))
  (lambda [x y [luminance 1.0]]
    (define z (- 1.0 x y))
    
    (values (* (/ x y) luminance)
            luminance
            (* (/ z y) luminance))))


(define CIE-LCh->Lab : (-> Flonum Flonum (Values Flonum Flonum))
  (lambda [C h]
    (define polar (make-polar C (degrees->radians h)))
    (values (real-part polar) (imag-part polar))))
            
(define CIE-Lab->LCh : (-> Flonum Flonum Flonum (Values Flonum Flonum))
  (λ [a b epsilon]
    (define polar (make-rectangular a b))
    (define chroma (magnitude polar))
    (define hue
      (let ([deg (radians->degrees (angle polar))])
        (cond [(<= chroma epsilon) +nan.0]
              [(< deg 0.0) (+ deg 360.0)]
              [else deg])))
    
    (values chroma hue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-observer->XYZ-matching-curves : (->* (CIE-Observer) (Flonum Flonum #:λ-span Flonum) CIE-XYZ-Matching-Curves)
  (lambda [spectrum [λstart 380.0] [λend 780.0] #:λ-span [step 5.0]]
    (define fln : Flonum (flfloor (/ (- λend λstart) step)))
    (define n : Index (assert (fl->fx fln) index?))
    (define λs (CIE-observer-λs spectrum))
    (define X (make-flvector n))
    (define Y (make-flvector n))
    (define Z (make-flvector n))

    (for ([i (in-range 0 n)])
      (define λ0 (lerp (real->double-flonum (/ i n))        λstart λend))
      (define λ1 (lerp (real->double-flonum (/ (add1 i) n)) λstart λend))

      (flvector-set! X i (spectrum-sample-average λs (CIE-observer-xbars spectrum) λ0 λ1))
      (flvector-set! Y i (spectrum-sample-average λs (CIE-observer-ybars spectrum) λ0 λ1))
      (flvector-set! Z i (spectrum-sample-average λs (CIE-observer-zbars spectrum) λ0 λ1)))
    
    (CIE-XYZ-matching-curves λstart λend n X Y Z)))

(define CIE-XYZ-matching-curves->dots : (->* (CIE-XYZ-Matching-Curves) (Flonum Flonum #:λ-span Flonum #:vertical-flip? Boolean) (Listof Float-Complex))
  (lambda [curves [λstart 380.0] [λend 780.0] #:λ-span [step 5.0] #:vertical-flip? [yflip? #true]]
    (define fln : Flonum (flfloor (/ (- λend λstart) step)))
    (define n : Index (assert (fl->fx fln) index?))
    (define X (CIE-XYZ-matching-curves-X curves))
    (define Y (CIE-XYZ-matching-curves-Y curves))
    (define Z (CIE-XYZ-matching-curves-Z curves))

    (for/list : (Listof Float-Complex) ([idx (in-range 0 (flvector-length X))])
      (define-values (xbar ybar zbar) (values (flvector-ref X idx) (flvector-ref Y idx) (flvector-ref Z idx)))
      (define-values (x y) (CIE-XYZ->xyY xbar ybar zbar))
      
      (make-rectangular x (if (not yflip?) y (- 1.0 y))))))

(define CIE-illuminant->color-spectral-power-distribution : (->* (CIE-Illuminant) (Flonum Flonum #:λ-span Flonum) FlVector)
  (lambda [illuminant [λstart 380.0] [λend 780.0] #:λ-span [step 5.0]]
    (define fln : Flonum (flfloor (/ (- λend λstart) step)))
    (define n : Fixnum (fl->fx fln))
    (define λs (CIE-illuminant-λs illuminant))
    (define spds (make-flvector n))

    (for ([i (in-range 0 n)])
      (define λ0 (lerp (real->double-flonum (/ i n))        λstart λend))
      (define λ1 (lerp (real->double-flonum (/ (add1 i) n)) λstart λend))

      (flvector-set! spds i (spectrum-sample-average λs (CIE-illuminant-spds illuminant) λ0 λ1)))
    
    spds))

(define CIE-XYZ-scale : (-> CIE-XYZ-Matching-Curves Flonum)
  (lambda [cs]
    (define n : Index (CIE-XYZ-matching-curves-count cs))

    (/ (- (CIE-XYZ-matching-curves-λend cs) (CIE-XYZ-matching-curves-λstart cs))
       (* ∫Yλdλ (->fl n)))))

(define CIE-spectrum-matching-curves->luminance : (-> CIE-XYZ-Matching-Curves FlVector Flonum)
  (lambda [observer illuminant]
    (define k : Flonum (CIE-XYZ-scale observer))
    (define luminance : Flonum
      (for/fold ([l : Flonum 0.0])
                ([y (in-flvector (CIE-XYZ-matching-curves-Y observer))]
                 [i (in-flvector illuminant)])
        (+ l (* y i))))
    
    (values (* luminance k))))

(define CIE-spectrum-matching-curves->XYZ : (-> CIE-XYZ-Matching-Curves FlVector (Values Flonum Flonum Flonum))
  (lambda [observer illuminant]
    (define k : Flonum (CIE-XYZ-scale observer))
    (define-values (X Y Z)
      (for/fold ([X : Flonum 0.0]
                 [Y : Flonum 0.0]
                 [Z : Flonum 0.0])
                ([x (in-flvector (CIE-XYZ-matching-curves-X observer))]
                 [y (in-flvector (CIE-XYZ-matching-curves-Y observer))]
                 [z (in-flvector (CIE-XYZ-matching-curves-Z observer))]
                 [i (in-flvector illuminant)])
        (values (+ X (* x i))
                (+ Y (* y i))
                (+ Z (* z i)))))
    
    (values (* X k) (* Y k) (* Z k))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-make-XYZ-RGB-convertors : (-> (U CIE-RGB-Weight-Factors CIE-Illuminant-Type) [#:rgb-filter CIE-Filter] [#:gamma? Boolean]
                                          (Values CIE<->RGB CIE<->RGB))
  (lambda [m #:rgb-filter [rgb-filter CIE-RGB-normalize] #:gamma? [gamma? #true]]
    (define coRGB (cond [(eq? m 'D65) CIE-sRGB-D65] [(eq? m 'D50) CIE-sRGB-D50] [else m]))
    (define coXYZ (matrix-inverse coRGB))

    (if (not gamma?)
        (values (λ [X Y Z]
                  (define RGB (matrix* coXYZ (col-matrix [X Y Z])))
                  (rgb-filter (matrix-ref RGB 0 0)
                              (matrix-ref RGB 1 0)
                              (matrix-ref RGB 2 0)))
                
                (λ [R G B]
                  (define XYZ (matrix* coRGB (col-matrix [R G B])))
                  (values (matrix-ref XYZ 0 0)
                          (matrix-ref XYZ 1 0)
                          (matrix-ref XYZ 2 0))))
        (values (λ [X Y Z]
                  (define RGB (matrix* coXYZ (col-matrix [X Y Z])))
                  (define-values (r g b)
                    (rgb-filter (matrix-ref RGB 0 0)
                                (matrix-ref RGB 1 0)
                                (matrix-ref RGB 2 0)))
                  
                  (color-gamma-encode r g b))
                
                (λ [R G B]
                  (define-values (r g b) (color-gamma-decode R G B))
                  (define XYZ (matrix* coRGB (col-matrix [r g b])))
                  (values (matrix-ref XYZ 0 0)
                          (matrix-ref XYZ 1 0)
                          (matrix-ref XYZ 2 0)))))))

(define CIE-make-LAB-RGB-convertors : (-> CIE-Illuminant-Type [#:rgb-filter CIE-Filter] (Values CIE<->RGB CIE<->RGB))
  (lambda [m #:rgb-filter [rgb-filter values]]
    (define-values (XYZ->RGB RGB->XYZ) (CIE-make-XYZ-RGB-convertors m #:rgb-filter rgb-filter #:gamma? #true))
    (define XYZn (if (eq? m 'D65) CIE-XYZn-D65 CIE-XYZn-D50))

    (values (λ [L a b]
              (define Y* (/ (+ L 0.16) 1.16))
              (XYZ->RGB (* (vector-ref XYZn 0) (CIE-lightness-transformation⁻¹ (+ Y* (/ a 500.0))))
                        (* (vector-ref XYZn 1) (CIE-lightness-transformation⁻¹ Y*))
                        (* (vector-ref XYZn 2) (CIE-lightness-transformation⁻¹ (- Y* (/ b 200.0))))))
            
            (λ [R G B]
              (define-values (X Y Z) (RGB->XYZ R G B))
              (define-values (X* Y* Z*) (CIE-XYZ-normalize X Y Z XYZn))
              (values (- (* 1.160 (CIE-lightness-transformation Y*)) 0.16)
                      (* 500.0 (- (CIE-lightness-transformation X*) (CIE-lightness-transformation Y*)))
                      (* 200.0 (- (CIE-lightness-transformation Y*) (CIE-lightness-transformation Z*))))))))

(define CIE-make-LCH-RGB-convertors : (-> CIE-Illuminant-Type [#:rgb-filter CIE-Filter] (Values CIE<->RGB CIE<->RGB))
  (lambda [m #:rgb-filter [rgb-filter values]]
    (define-values (Lab->RGB RGB->Lab) (CIE-make-LAB-RGB-convertors m #:rgb-filter rgb-filter))
    (define epsilon 0.0015)

    (values (λ [L C h]
              (define-values (a b) (CIE-LCh->Lab C h))
              (Lab->RGB L a b))
            
            (λ [R G B]
              (define-values (L a b) (RGB->Lab R G B))
              (define-values (chroma hue) (CIE-Lab->LCh a b epsilon))
              (values L chroma hue)))))


(define CIE-make-OKLAB-RGB-convertors : (-> [#:rgb-filter CIE-Filter] (Values CIE<->RGB CIE<->RGB))
  (lambda [#:rgb-filter [rgb-filter values]]
    (define coRGB CIE-sRGB-D65)
    (define coXYZ (matrix-inverse coRGB))

    (values (λ [L a b]
              (define LMS (matrix* CIE-OKLAB->LMS-D65 (col-matrix [L a b])))
              (define XYZ (matrix* CIE-LMS->XYZ-D65 (matrix-map (λ [[c : Flonum]] (* c c c)) LMS)))
              (define RGB (matrix* coXYZ XYZ))
              (define-values (R G B)
                (rgb-filter (matrix-ref RGB 0 0)
                            (matrix-ref RGB 1 0)
                            (matrix-ref RGB 2 0)))

              (color-gamma-encode R G B))
            
            (λ [R G B]
              (define-values (r g b) (color-gamma-decode R G B))
              (define XYZ (matrix* coRGB (col-matrix [r g b])))
              (define LMS (matrix* CIE-XYZ->LMS-D65 XYZ))
              (define Lab (matrix* CIE-LMS->OKLAB-D65 (matrix-map flcbrt LMS)))
              
              (values (matrix-ref Lab 0 0)
                      (matrix-ref Lab 1 0)
                      (matrix-ref Lab 2 0))))))

(define CIE-make-OKLCH-RGB-convertors : (-> [#:rgb-filter CIE-Filter] (Values CIE<->RGB CIE<->RGB))
  (lambda [#:rgb-filter [rgb-filter values]]
    (define-values (OKLab->RGB RGB->OKLab) (CIE-make-OKLAB-RGB-convertors #:rgb-filter rgb-filter))
    (define epsilon 0.000004)

    (values (λ [L C h]
              (define-values (a b) (CIE-LCh->Lab C h))
              (OKLab->RGB L a b))
            
            (λ [R G B]
              (define-values (L a b) (RGB->OKLab R G B))
              (define-values (chroma hue) (CIE-Lab->LCh a b epsilon))
              (values L chroma hue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (lab->rgb rgb->lab) (CIE-make-LAB-RGB-convertors 'D65))
(define-values (lch->rgb rgb->lch) (CIE-make-LCH-RGB-convertors 'D65))
(define-values (oklab->rgb rgb->oklab) (CIE-make-OKLAB-RGB-convertors))
(define-values (oklch->rgb rgb->oklch) (CIE-make-OKLCH-RGB-convertors))
