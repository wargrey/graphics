#lang typed/racket/base

(provide (all-defined-out))

(require "../constants.rkt")
(require "../footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dart-Key-Vertices (List Float-Complex Float-Complex Float-Complex Float-Complex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-dart-metrics : (->* (Nonnegative-Flonum Flonum (Option Flonum)) (Float-Complex)
                                (Values Geo-Path-Clean-Prints+ Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [head-radius rpoint wing-angle [offset 0.0+0.0i]]
    (define fllength : Nonnegative-Flonum (* head-radius 2.0))
    (define rdelta : Flonum (if (not wing-angle) (* 0.6 pi #;108.0) (- pi (* 0.5 wing-angle))))
    
    (values (list (gpp:point #\M offset)
                  (gpp:point #\L (+ (make-polar head-radius (+ rpoint rdelta)) offset))
                  (gpp:point #\L (+ (make-polar head-radius rpoint) offset))
                  (gpp:point #\L (+ (make-polar head-radius (- rpoint rdelta)) offset))
                  (gpp:point #\L offset))
            (+ head-radius (real-part offset))
            (+ head-radius (imag-part offset))
            fllength fllength)))

(define geo-curved-dart-metrics : (->* (Nonnegative-Flonum Flonum (Option Flonum)) (Float-Complex Flonum)
                                       (Values Geo-Path-Clean-Prints+ Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [head-radius rpoint wing-angle [offset 0.0+0.0i] [ctrl-ratio 0.5]]
    (define fllength : Nonnegative-Flonum (* head-radius 2.0))
    (define rdelta : Flonum (if (not wing-angle) (* 0.6 pi #;108.0) (- pi (* 0.5 wing-angle))))
    (define arrow-top : Float-Complex (make-polar head-radius rpoint))
    (define arrow : Float-Complex (+ arrow-top offset))
    (define wing1 : Float-Complex (+ (make-polar head-radius (+ rpoint rdelta)) offset))
    (define wing2 : Float-Complex (+ (make-polar head-radius (- rpoint rdelta)) offset))
    (define ctrl : Float-Complex (+ (* arrow-top ctrl-ratio) offset))
    
    (values (list (gpp:point #\M offset)
                  (gpp:point #\L wing1)
                  (gpp:bezier #\Q arrow wing1 ctrl)
                  (gpp:bezier #\Q wing2 arrow ctrl)
                  (gpp:point #\L wing2)
                  (gpp:point #\L offset))
            (+ head-radius (real-part offset))
            (+ head-radius (imag-part offset))
            fllength fllength)))

(define geo-arrow-metrics : (-> Nonnegative-Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                (Values Geo-Path-Clean-Prints+ Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [head-radius rpoint shaft-thickness shaft-length wing-angle]
    (define fllength : Nonnegative-Flonum (* head-radius 2.0))
    (define rdelta : Flonum (if (not wing-angle) (* 0.6 pi #;108.0) (- pi (* 0.5 wing-angle))))
    (define rwing1 : Flonum (+ rpoint rdelta))
    (define rwing2 : Flonum (- rpoint rdelta))
    (define-values (rpx rpy) (values (* head-radius (cos rpoint)) (* head-radius (sin rpoint))))
    (define-values (wx1 wy1) (values (* head-radius (cos rwing1)) (* head-radius (sin rwing1))))
    (define-values (wx2 wy2) (values (* head-radius (cos rwing2)) (* head-radius (sin rwing2))))
    
    (define shaft-thickness/2 : Nonnegative-Flonum (* shaft-thickness 0.5))
    (define draw-shaft? : Boolean (< shaft-thickness/2 head-radius))
    (define wing-theta : Flonum (- rdelta pi/2))
    (define wing-radius : Flonum (/ shaft-thickness/2 (cos wing-theta)))
    (define shaft-minlen : Flonum (* shaft-thickness/2 (tan wing-theta)))
    
    (define-values (flwidth flheight tx ty shx1 shy1 shx2 shy2)
      (if (and draw-shaft? (> shaft-length shaft-minlen))
          (let*-values ([(shaft-radius) (sqrt (+ (* shaft-thickness/2 shaft-thickness/2)
                                                 (* shaft-length shaft-length)))]
                        [(shdelta) (+ (acos (/ shaft-thickness/2 shaft-radius)) pi/2)]
                        [(shtail1 shtail2) (values (+ rpoint shdelta) (- rpoint shdelta))]
                        [(shx1 shy1) (values (* shaft-radius (cos shtail1)) (* shaft-radius (sin shtail1)))]
                        [(shx2 shy2) (values (* shaft-radius (cos shtail2)) (* shaft-radius (sin shtail2)))])
            (if (<= shaft-length head-radius)
                (values fllength fllength head-radius head-radius shx1 shy1 shx2 shy2)

                (let*-values ([(axmin axmax) (values (min rpx wx1 wx2) (max rpx wx1 wx2))]
                              [(aymin aymax) (values (min rpy wy1 wy2) (max rpy wy1 wy2))]
                              [(shxmin shxmax) (values (min shx1 shx2) (max shx1 shx2))]
                              [(shymin shymax) (values (min shy1 shy2) (max shy1 shy2))]
                              [(xmin xmax) (values (min axmin shxmin) (max axmax shxmax))]
                              [(ymin ymax) (values (min aymin shymin) (max aymax shymax))])
                  (values (max (- xmax xmin) 0.0) (max (- ymax ymin) 0.0) (- xmin) (- ymin) shx1 shy1 shx2 shy2))))
          (values fllength fllength head-radius head-radius 0.0 0.0 0.0 0.0)))

    (define-values (pos0 shaft-prints)
      (if (or draw-shaft?)
          (values (make-polar wing-radius rwing1)
                  (list (gpp:point #\L (make-rectangular shx1 shy1))
                        (gpp:point #\L (make-rectangular shx2 shy2))
                        (gpp:point #\L (make-polar wing-radius rwing2))))
          (values 0.0+0.0i null)))

    (define head-prints : Geo-Path-Clean-Prints+
      (let ([ctrl (* (make-rectangular rpx rpy) 0.5)])
        (list (gpp:point #\L (make-rectangular wx2 wy2))
              (gpp:point #\L (make-rectangular rpx rpy))
              (gpp:point #\L (make-rectangular wx1 wy1))
              (gpp:point #\L pos0))))
      
    (values (if (null? shaft-prints)
                (cons (gpp:point #\M pos0) head-prints)
                (cons (gpp:point #\M pos0) (append shaft-prints head-prints)))
            tx ty flwidth flheight)))
