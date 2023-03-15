#lang typed/racket

(require bitmap/constructor)
(require bitmap/composite)
(require bitmap/color)

(require math/flonum)

(require "../cie.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Coordinate (List Flonum Flonum Flonum))
(define-type ColorMap-Info
  (List (List Hexa Coordinate)
        (List Hexa Coordinate)
        (List Hexa Coordinate)
        (List Hexa Coordinate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define position-filter : (-> Flonum Flonum)
  (lambda [pt]
    (define significent-digits 10000.0)
    (/ (round (* pt significent-digits))
       significent-digits)))

(define make-coordinate : (-> Flonum Flonum Coordinate)
  (lambda [r g]
    (define b (- 1.0 r g))
    (list (position-filter r)
          (position-filter g)
          (position-filter b))))

(define chromaticity-diagram-size 300)
(define black (hexa* 0.0 0.0 0.0))
(define zero (make-coordinate 0.0 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rgb-triangle-vertices : (-> ColorMap-Info Flonum Flonum Flonum Flonum Flonum ColorMap-Info)
  (lambda [vertices x y r g b]
    (define red (car vertices))
    (define green (cadr vertices))
    (define blue (caddr vertices))
    (define white (cadddr vertices))
    (define fx (position-filter x))
    (define fy (position-filter y))

    (define hexa (hexa* r g b))

    (list (if (> fx (car (cadr red))) (list hexa (make-coordinate x y)) red)
          (if (> fy (cadr (cadr green))) (list hexa (make-coordinate x y)) green)
          (if (< fx (car (cadr blue))) (list hexa (make-coordinate x y)) blue)
          (if (> (hexa-digits hexa) (hexa-digits (car white))) (list hexa (make-coordinate x y)) white))))

(define make-rgb-color-map-constructor : (->* (CIE-RGB-Weight-Factors) (Flonum) (XYWH->ARGB* ColorMap-Info))
  (lambda [tranpose-matrix [L 1.0]]
    (define-values (xyY->RGB _) (CIE-make-xyY-RGB-convertors tranpose-matrix L))

    (λ [[px : Index] [py : Index] [w : Index] [h : Index] [vertices : ColorMap-Info]]
      (define x (real->double-flonum (/ px w)))
      (define y (real->double-flonum (/ (- h py) h)))
      (define-values (r g b okay?) (xyY->RGB x y))
          
      (if (and okay?)
          (values 1.0 r g b (rgb-triangle-vertices vertices x y r g b))
          (values 0.0 0.0 0.0 0.0 vertices)))))

(define bitmap-rgb-color-map : (-> CIE-RGB-Weight-Factors Any (Values Bitmap ColorMap-Info))
  (lambda [transpose-matrix type]
    (define black (hexa* 0.0 0.0 0.0))
    (define zero (make-coordinate 0.0 0.0))
    (define vertices0 : ColorMap-Info
      (list (list black zero)
            (list black zero)
            (list black (make-coordinate 1.0 1.0))
            (list black zero)))

    (printf "====== ~a =====~n" type)
    (bitmap-rectangular* chromaticity-diagram-size chromaticity-diagram-size
                         (make-rgb-color-map-constructor transpose-matrix)
                         vertices0)))

(define bitmap-XYZ-matching-function : (-> CIE-Observer Bitmap)
  (lambda [observer]
    (define curves (CIE-observer->XYZ-matching-curves observer #:λ-span 0.1))
    (define N (CIE-XYZ-matching-curves-count curves))
    (define X (CIE-XYZ-matching-curves-X curves))
    (define Y (CIE-XYZ-matching-curves-Y curves))
    (define Z (CIE-XYZ-matching-curves-Z curves))
    
    (define draw-curve : (ARGB-Step (Pairof (Listof FlVector) Nonnegative-Fixnum))
      (λ [w h it]
        (define-values (css idx) (values (car it) (cdr it)))
        (cond [(null? css) (values #false 0 0.0 0.0 0.0 0.0 it)]
              [(>= idx N) (values 0 0 0.0 0.0 0.0 0.0 (cons (cdr css) 0))]
              [else (let* ([cs (car css)]
                           [x (exact-round (* (/ idx N) w))]
                           [y (exact-round (* (- 1.0 (* (flvector-ref cs idx) 0.5)) h))]
                           [it++ (cons css (add1 idx))])
                      (case (length css)
                        [(1)  (values x y 1.0 0.0 0.0 1.0 it++)]
                        [(2)  (values x y 1.0 0.0 1.0 0.0 it++)]
                        [else (values x y 1.0 1.0 0.0 0.0 it++)]))])))

    (bitmap-irregular chromaticity-diagram-size (* chromaticity-diagram-size 0.618)
                      draw-curve (cons (list X Y Z) 0))))

(define bitmap-spectral-locus : (-> CIE-Observer CIE-Illuminant Bitmap)
  (lambda [observer illuminant]
    (define curves (CIE-observer->XYZ-matching-curves observer #:λ-span 0.1))
    (define color-spd (CIE-illuminant->color-spectral-power-distribution illuminant #:λ-span 0.1))
    (define k : Flonum (CIE-XYZ-scale curves))
    (define X (flvector* (CIE-XYZ-matching-curves-X curves) color-spd))
    (define Y (flvector* (CIE-XYZ-matching-curves-Y curves) color-spd))
    (define Z (flvector* (CIE-XYZ-matching-curves-Z curves) color-spd))

    (define draw-locus : (ARGB-Step Nonnegative-Fixnum)
      (λ [w h idx]
        (if (>= idx (flvector-length X))
            (values #false 0 0.0 0.0 0.0 0.0 idx)
            (let-values ([(x y) (CIE-XYZ->xyY (* (flvector-ref X idx) k) (* (flvector-ref Y idx) k) (* (flvector-ref Z idx) k))])
              (values (exact-round (* x w)) (exact-round (* (- 1.0 y) h))
                      1.0 0.0 0.0 0.0
                      (add1 idx))))))

    (bitmap-irregular chromaticity-diagram-size chromaticity-diagram-size
                      draw-locus 0)))

(define bitmap-chromaticity-diagram : (-> CIE-Observer CIE-Illuminant CIE-RGB-Weight-Factors Any (Values Bitmap ColorMap-Info))
  (lambda [observer illuminant transpose-matrix type]
    (define-values (color-map mapinfo) (bitmap-rgb-color-map transpose-matrix type))
    (define locus (bitmap-spectral-locus observer illuminant))

    (values (bitmap-cc-superimpose color-map locus)
            mapinfo)))


(module+ main
  (pretty-print-columns 80)
  (current-print pretty-print-handler)

  (define-values (observer illuminants) (CIE-load-default-spectrum-samples))
  
  (bitmap-XYZ-matching-function observer)
  (bitmap-chromaticity-diagram observer (car illuminants) CIE-primary 'CIE-Primary)
  #;(bitmap-chromaticity-diagram observer (car illuminants) CIE-sRGB-D65 'sRGB-D65)
  #;(bitmap-chromaticity-diagram observer (car illuminants) CIE-sRGB-D50 'sRGB-D50))
