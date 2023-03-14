#lang typed/racket/base

(provide (all-defined-out))

(require math/matrix)

(require racket/flonum)

;;; Resources
; http://cvrl.ioo.ucl.ac.uk/index.htm
; https://www.rit.edu/science/munsell-color-science-lab-educational-resources#useful-color-data

;;; Terminology and Theory
; RGB, the RGB color mode
; rgb, the transformed color value for RGB
; XYZ, a bad-named imaginary 3-primary color mode to ensure that all colors are located in the first quadrant (non-negative)
; xyY, the projection of XYZ in a 2D plane, as the `Y` are designed to be the luminance of any color.
;        The `x`, `y` are relative values of `X` and `Z`,
;          and x + y + z = 1.0, x > 0.0, z > 0.0,
;          hence just a 2D plane.
;        The value of `Y` doesn't affect the resulting chromaticity diagram unless it is a 3D one.

; Negaive `r`, `g`, `b` means the target color is either out of gamut or cannot be mixed by
;  the 3-primary monochromatic light at certain wavelengths.
; Yes, it's true that not all colors can be produced by additive mixing with R, G, and B. Say,
;     Vibrant BlueGreen       = -38R + 42G + 9B
;  => Vibrant BlueGreen + 38R =        42G + 9B
; That is, in the color matching experiment, Some `R` must be added to the target color to trick human eyes.
; Here the coefficients are irradiance.

; Human eyes favor the Green light,
;   hence the ratio of RGB are 1.0 : 4.5907 : 0.060 
; That's why the `Y` are designed for the luminance,
;   and `X` and `Z` don't affect the luminance.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CIE-RGB-Weight-Factors (Matrix Flonum))
(define-type CIE-Color-Convertor (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))
(define-type CIE-xyY->RGB (-> Flonum Flonum (Values Flonum Flonum Flonum Boolean)))
(define-type CIE-RGB->xyY (-> Flonum Flonum Flonum (Values Flonum Flonum)))
(define-type CIE-Filter (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum)))

(define CIE-primary : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector 0.49000 0.31000 0.20000
                          0.17697 0.81240 0.01063
                          0.00000 0.01000 0.99000)))

; For monitor, Daylight 6500K
(define CIE-sRGB-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector 0.412453 0.357580 0.180423
                          0.212671 0.715160 0.072169
                          0.019334 0.119193 0.950227)))

; For printed paper
(define CIE-sRGB-D50 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector 0.4361 0.3851 0.1431
                          0.2225 0.7169 0.0606
                          0.0139 0.0971 0.7141)))

(define CIE-RGB-gamma-correct-to-XYZ : (-> Flonum Flonum)
  (lambda [c]
    (if (<= c 0.03928)
        (/ c 12.92)
        (flexpt (/ (+ 0.055 c) 1.055) 2.4))))

(define CIE-RGB-gamma-correct-from-XYZ : (-> Flonum Flonum)
  (lambda [c]
    (if (<= c 0.0031308)
        (* c 12.92)
        (- (* (flexpt c (/ 1.0 2.4)) 1.055) 0.055))))

(define CIE-RGB-normalize : CIE-Filter
  (lambda [r g b]
    (define L (max r g b))
    (values (/ r L)
            (/ g L)
            (/ b L))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-make-XYZ-RGB-convertors : (-> CIE-RGB-Weight-Factors (Values CIE-Color-Convertor CIE-Color-Convertor))
  (lambda [m]
    (define coRGB m)
    (define coXYZ (matrix-inverse m))

    (values (λ [X Y Z]
              (define RGB (matrix* coXYZ (col-matrix [X Y Z])))
              (values (matrix-ref RGB 0 0)
                      (matrix-ref RGB 1 0)
                      (matrix-ref RGB 2 0)))

            (λ [R G B]
              (define XYZ (matrix* coRGB (col-matrix [R G B])))
              (values (matrix-ref XYZ 0 0)
                      (matrix-ref XYZ 1 0)
                      (matrix-ref XYZ 2 0))))))

(define CIE-make-xyY-RGB-convertors : (->* (CIE-RGB-Weight-Factors) (Flonum #:filter CIE-Filter) (Values CIE-xyY->RGB CIE-RGB->xyY))
  (lambda [m [luminance 1.0] #:filter [rgb-filter CIE-RGB-normalize]]
    (define-values (XYZ->RGB RGB->XYZ) (CIE-make-XYZ-RGB-convertors m))

    (values (λ [x y]
              (define-values (X Y Z) (CIE-xyY->XYZ x y luminance))
              (define-values (R G B) (XYZ->RGB X Y Z))
              (define okay? (and (>= R 0.0) (>= G 0.0) (>= B 0.0)))

              (if (not okay?)
                  (values R G B okay?)
                  (let-values ([(r g b) (rgb-filter R G B)])
                    (values (CIE-RGB-gamma-correct-from-XYZ r)
                            (CIE-RGB-gamma-correct-from-XYZ g)
                            (CIE-RGB-gamma-correct-from-XYZ b)
                            okay?))))

            (λ [R G B]
              (define r (CIE-RGB-gamma-correct-to-XYZ R))
              (define g (CIE-RGB-gamma-correct-to-XYZ G))
              (define b (CIE-RGB-gamma-correct-to-XYZ B))
              (define-values (X Y Z) (RGB->XYZ r g b))

              (CIE-XYZ->xyY X Y Z)))))
