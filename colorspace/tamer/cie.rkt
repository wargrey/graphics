#lang typed/racket

(require bitmap/constructor)
(require bitmap/color)

(require "../cie.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Coordinate (List Flonum Flonum Flonum))
(define-type RGBW-Info
  (List (List Hexa Coordinate)
        (List Hexa Coordinate)
        (List Hexa Coordinate)
        (List Hexa Coordinate)))

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

(define chromaticity-diagram-boundary : (-> RGBW-Info Flonum Flonum Flonum Flonum Flonum RGBW-Info)
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

(define make-chromaticity-diagram-constructor : (->* (CIE-RGB-Weight-Factors) (Flonum) (XYWH->ARGB* RGBW-Info))
  (lambda [tranpose-matrix [L 1.0]]
    (define-values (xyY->RGB _) (CIE-make-xyY-RGB-convertors tranpose-matrix L))

    (λ [[px : Index] [py : Index] [w : Index] [h : Index] [vertices : RGBW-Info]]
      (define x (real->double-flonum (/ px w)))
      (define y (real->double-flonum (/ (- h py) h)))
      (define-values (r g b okay?) (xyY->RGB x y))
          
      (if (and okay?)
          (values 1.0 r g b (chromaticity-diagram-boundary vertices x y r g b))
          (values 0.0 0.0 0.0 0.0 vertices)))))

(define bitmap-chromaticity-diagram : (-> CIE-RGB-Weight-Factors Any (Values Bitmap RGBW-Info))
  (lambda [transpose-matrix type]
    (define chromaticity-diagram-size 300)
    (define black (hexa* 0.0 0.0 0.0))
    (define zero (make-coordinate 0.0 0.0))
    (define vertices0 : RGBW-Info
      (list (list black zero)
            (list black zero)
            (list black (make-coordinate 1.0 1.0))
            (list black zero)))

    (printf "====== ~a =====~n" type)
    (build-bitmap* chromaticity-diagram-size chromaticity-diagram-size
                   (make-chromaticity-diagram-constructor transpose-matrix)
                   vertices0)))


(module+ main
  (pretty-print-columns 80)
  (current-print pretty-print-handler)
  
  (bitmap-chromaticity-diagram CIE-primary 'CIE-Primary)
  (bitmap-chromaticity-diagram CIE-sRGB-D65 'sRGB-D65)
  (bitmap-chromaticity-diagram CIE-sRGB-D50 'sRGB-D50))
