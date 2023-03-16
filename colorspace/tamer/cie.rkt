#lang typed/racket

(require bitmap/constructor)
(require bitmap/composite)
(require bitmap/pixel)
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

(define make-rgb-color-map-generator : (->* (CIE-RGB-Weight-Factors) (Flonum) (XYWH->ARGB* ColorMap-Info))
  (lambda [tranpose-matrix [L 1.0]]
    (define-values (XYZ->RGB _) (CIE-make-XYZ-RGB-convertors tranpose-matrix #:rgb-filter CIE-RGB-normalize #:gamma? #true))

    (λ [[px : Index] [py : Index] [w : Index] [h : Index] [vertices : ColorMap-Info]]
      (define x (real->double-flonum (/ px w)))
      (define y (real->double-flonum (/ (- h py) h)))
      (define-values (X Y Z) (CIE-xyY->XYZ x y L))
      (define-values (r g b) (XYZ->RGB X Y Z))
          
      (if (and (>= r 0.0) (>= g 0.0) (>= b 0.0))
          (values 1.0 r g b (rgb-triangle-vertices vertices x y r g b))
          (values 0.0 0.0 0.0 0.0 vertices)))))

(define make-gamut-generator : (->* (CIE-RGB-Weight-Factors) (Flonum) ARGB-Map)
  (lambda [tranpose-matrix [L 1.0]]
    (define-values (XYZ->RGB _) (CIE-make-XYZ-RGB-convertors tranpose-matrix #:rgb-filter CIE-RGB-normalize #:gamma? #true))

    (λ [[px : Index] [py : Index] [w : Index] [h : Index] [mA : Byte] [mR : Byte] [mG : Byte] [mB : Byte]]
      (if (> mA 0)
          (let ([x (real->double-flonum (/ px w))]
                [y (real->double-flonum (/ (- h py) h))])
            (define-values (X Y Z) (CIE-xyY->XYZ x y L))
            (define-values (r g b) (XYZ->RGB X Y Z))
          
            (values 1.0 r g b))
          (values 0.0 0.0 0.0 0.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-rgb-color-map : (-> CIE-RGB-Weight-Factors (Values Bitmap ColorMap-Info))
  (lambda [transpose-matrix]
    (define black (hexa* 0.0 0.0 0.0))
    (define zero (make-coordinate 0.0 0.0))
    (define vertices0 : ColorMap-Info
      (list (list black zero)
            (list black zero)
            (list black (make-coordinate 1.0 1.0))
            (list black zero)))

    (bitmap-rectangular* chromaticity-diagram-size chromaticity-diagram-size
                         (make-rgb-color-map-generator transpose-matrix)
                         vertices0)))

(define bitmap-spectral-locus : (-> CIE-Observer CIE-RGB-Weight-Factors Bitmap)
  (lambda [observer transpose-matrix]
    (define-values (XYZ->RGB RGB->XYZ) (CIE-make-XYZ-RGB-convertors transpose-matrix #:rgb-filter CIE-RGB-normalize #:gamma? #false))
    (define curves (CIE-observer->XYZ-matching-curves observer #:λ-span 0.1))
    (define X (CIE-XYZ-matching-curves-X curves))
    (define Y (CIE-XYZ-matching-curves-Y curves))
    (define Z (CIE-XYZ-matching-curves-Z curves))

    (define draw-locus : (ARGB-Step Nonnegative-Fixnum)
      (λ [w h idx]
        (if (>= idx (flvector-length X))
            (values #false 0 0.0 0.0 0.0 0.0 idx)
            (let*-values ([(xbar ybar zbar) (values (flvector-ref X idx) (flvector-ref Y idx) (flvector-ref Z idx))]
                          [(r g b) (XYZ->RGB xbar ybar zbar)]
                          [(x y) (CIE-XYZ->xyY xbar ybar zbar)])
              (values (exact-round (* x w)) (exact-round (* (- 1.0 y) h))
                      1.0 r g b
                      (add1 idx))))))

    (bitmap-irregular chromaticity-diagram-size chromaticity-diagram-size
                      draw-locus 0)))

(define bitmap-color-map : (-> CIE-Observer CIE-RGB-Weight-Factors (Values Bitmap ColorMap-Info))
  (lambda [observer transpose-matrix]
    (define-values (cmap cminfo) (bitmap-rgb-color-map transpose-matrix))
    (define locus (bitmap-spectral-locus observer transpose-matrix))

    (values (bitmap-cc-superimpose cmap locus) cminfo)))

(define bitmap-chromaticity-diagram : (-> CIE-Observer CIE-RGB-Weight-Factors Bitmap)
  (lambda [observer transpose-matrix]
    (define curves (CIE-observer->XYZ-matching-curves observer #:λ-span 1.0))
    (define dots (CIE-XYZ-matching-curves->dots curves))
    (define window-size (make-rectangular chromaticity-diagram-size chromaticity-diagram-size))
    
    (bitmap-map (bitmap-polygon dots #:fill 'black #:scale window-size #:window window-size)
                (make-gamut-generator transpose-matrix))))

(define bitmap-XYZ-matching-curves : (-> CIE-Observer Bitmap)
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


(module+ main
  (pretty-print-columns 80)
  (current-print pretty-print-handler)

  (define-values (observer illuminants) (CIE-load-default-spectrum-samples))
  
  (bitmap-XYZ-matching-curves observer)

  (printf "====== ~a =====~n" 'CIE-Primary)
  (bitmap-chromaticity-diagram observer CIE-primary)
  (bitmap-color-map observer CIE-primary)
  
  (printf "====== ~a =====~n" 'sRGB-D65)
  (bitmap-chromaticity-diagram observer CIE-sRGB-D65)
  (bitmap-color-map observer CIE-sRGB-D65)
  
  (printf "====== ~a =====~n" 'sRGB-D50)
  (bitmap-chromaticity-diagram observer CIE-sRGB-D50)
  (bitmap-color-map observer CIE-sRGB-D50))
