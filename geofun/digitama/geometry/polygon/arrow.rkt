#lang typed/racket/base

(provide (all-defined-out))

(require "../constants.rkt")
(require "../footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-arrow-metrics : (-> Nonnegative-Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                (Values (Listof Geo-Path-Print) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
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
      (if (or (not draw-shaft?) (<= shaft-length shaft-minlen))
          (values fllength fllength head-radius head-radius 0.0 0.0 0.0 0.0)

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
                  (values (max (- xmax xmin) 0.0) (max (- ymax ymin) 0.0) (- xmin) (- ymin) shx1 shy1 shx2 shy2))))))

    (define shaft-prints : (Listof Geo-Path-Print)
      (if (or draw-shaft?)
          (list (cons #\M (make-rectangular (* wing-radius (cos rwing1)) (* wing-radius (sin rwing1))))
                (cons #\L (make-rectangular shx1 shy1))
                (cons #\L (make-rectangular shx2 shy2))
                (cons #\L (make-rectangular (* wing-radius (cos rwing2)) (* wing-radius (sin rwing2)))))
          (list (cons #\M 0.0+0.0i))))

    (define head-prints : (Listof Geo-Path-Print)
      (list (cons #\L (make-rectangular wx2 wy2))
            (cons #\L (make-rectangular rpx rpy))
            (cons #\L (make-rectangular wx1 wy1))
            (cons #\Z #false)))
      
    (values (append shaft-prints head-prints)
            tx ty flwidth flheight)))
