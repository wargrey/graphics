#lang typed/racket

(require bitmap/constructor)
(require bitmap/color)

(require racket/flonum)

(require "../cie.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Coordinate (List Flonum Flonum Flonum))
(define-type RGBW-Info
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
(define rgb-triangle-vertices : (-> RGBW-Info Flonum Flonum Flonum Flonum Flonum RGBW-Info)
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

(define make-rgb-color-map-constructor : (->* (CIE-RGB-Weight-Factors) (Flonum) (XYWH->ARGB* RGBW-Info))
  (lambda [tranpose-matrix [L 1.0]]
    (define-values (xyY->RGB _) (CIE-make-xyY-RGB-convertors tranpose-matrix L))

    (λ [[px : Index] [py : Index] [w : Index] [h : Index] [vertices : RGBW-Info]]
      (define x (real->double-flonum (/ px w)))
      (define y (real->double-flonum (/ (- h py) h)))
      (define-values (r g b okay?) (xyY->RGB x y))
          
      (if (and okay?)
          (values 1.0 r g b (rgb-triangle-vertices vertices x y r g b))
          (values 0.0 0.0 0.0 0.0 vertices)))))

(define bitmap-rgb-color-map : (-> CIE-RGB-Weight-Factors Any (Values Bitmap RGBW-Info))
  (lambda [transpose-matrix type]
    (define black (hexa* 0.0 0.0 0.0))
    (define zero (make-coordinate 0.0 0.0))
    (define vertices0 : RGBW-Info
      (list (list black zero)
            (list black zero)
            (list black (make-coordinate 1.0 1.0))
            (list black zero)))

    (printf "====== ~a =====~n" type)
    (bitmap-rectangular* chromaticity-diagram-size chromaticity-diagram-size
                         (make-rgb-color-map-constructor transpose-matrix)
                         vertices0)))

(define bitmap-spectral-locus : (-> CIE-XYZ-Function-Samples Bitmap)
  (lambda [samples]
    (bitmap-irregular chromaticity-diagram-size chromaticity-diagram-size
                      (λ [[w : Index] [h : Index] [samples : CIE-XYZ-Function-Samples]] : (Values Integer Integer Real Real Real Real CIE-XYZ-Function-Samples)
                        (if (null? samples)
                            (values -1 -1 0.0 0.0 0.0 0.0 null)
                            (let* ([XYZ-function-values (cdar samples)]
                                   [xbar (flvector-ref XYZ-function-values 0)]
                                   [ybar (flvector-ref XYZ-function-values 1)])
                              (values (exact-round (* xbar w)) (exact-round (* ybar h))
                                      1.0 0.0 0.0 0.0
                                      (cdr samples)))))
                      samples)))


(module+ main
  (pretty-print-columns 80)
  (current-print pretty-print-handler)

  (bitmap-spectral-locus (CIE-load-default-XYZ-function-samples))
  
  (bitmap-rgb-color-map CIE-primary 'CIE-Primary)
  (bitmap-rgb-color-map CIE-sRGB-D65 'sRGB-D65)
  (bitmap-rgb-color-map CIE-sRGB-D50 'sRGB-D50))
