#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../path.rkt")
(require "../source.rkt")
(require "../visual/ctype.rkt")

(require "../../base.rkt")
(require "../../geometry/constants.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "../pangocairo.rkt")
  (require "../paint.rkt")
  
  (require (submod "../path.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_arrow create-surface metrics stroke background density)
    (define linewidth (~bdwidth stroke))
    (define offset (unsafe-fl* linewidth 0.5))
    (define-values (sfc cr)
      (create-surface (unsafe-fl+ (unsafe-vector*-ref metrics 3) linewidth)
                      (unsafe-fl+ (unsafe-vector*-ref metrics 4) linewidth)
                      density #true))
    
    (cairo_translate cr (unsafe-fl+ (unsafe-vector*-ref metrics 1) offset) (unsafe-fl+ (unsafe-vector*-ref metrics 2) offset))
    (cairo_new_sub_path cr)
    (cairo_move_to cr 0.0 0.0)
    (cairo_path cr (unsafe-vector*-ref metrics 0) 0.0 0.0)
    
    (cairo-render cr stroke background)
    (cairo_destroy cr)
    
    sfc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Arrow-Metrics (Immutable-Vector (Listof Geo-Path-Print) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_arrow (All (S) (-> (Cairo-Surface-Create S) Geo-Arrow-Metrics (Option Paint) (Option Fill-Source) Flonum S))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc-arrow-metrics : (-> Nonnegative-Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Flonum) Geo-Arrow-Metrics)
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
          null))

    (define head-prints : (Listof Geo-Path-Print)
      (list (cons #\L (make-rectangular wx2 wy2))
            (cons #\L (make-rectangular rpx rpy))
            (cons #\L (make-rectangular wx1 wy1))
            (cons #\Z #false)))
      
    (vector-immutable (append shaft-prints head-prints)
                      tx ty flwidth flheight)))
