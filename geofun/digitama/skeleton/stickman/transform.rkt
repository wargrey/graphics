#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "self.rkt")
(require "interface.rkt")

(require "../../paint/self.rkt")
(require "../../geometry/bbox.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-standing-stickman-snapshot : (-> Geo-Standing-Stickman Geo-Stickman-Skeleton)
  (lambda [self]
    (define-values (torso-width torso-length)
      (values (~dimension (geo-standing-stickman-torso-width self)
                          (~dimension (geo-standing-stickman-torso-width the-standing-stickman) 100.0))
              (~dimension (geo-standing-stickman-torso-length self)
                          (~dimension (geo-standing-stickman-torso-length the-standing-stickman) 100.0))))
    
    (define-values (leg-width arm-width)
      (values (~dimension (geo-standing-stickman-leg-width self) torso-width)
              (~dimension (geo-standing-stickman-arm-width self) torso-width)))

    (define head-radius (~dimension (geo-standing-stickman-head-radius self) torso-length))
    (define neck (make-rectangular 0.0 (+ head-radius (~dimension (geo-standing-stickman-neck-length self) torso-length))))
    (define hip (+ neck (make-rectangular 0.0 torso-length)))

    (define-values (lft-arm rgt-arm)
      (let* ([half-shoulder (* (~dimension (geo-standing-stickman-shoulder-breadth self) torso-width) 0.5)]
             [rgt-arm-angle (real->double-flonum (geo-standing-stickman-arm-angle self))]
             [lft-arm-angle (- pi rgt-arm-angle)]
             [elb-angle (real->double-flonum (geo-standing-stickman-elbow-angle self))]
             [upper-length (~dimension (geo-standing-stickman-upper-arm-length self) torso-length)]
             [lower-length (~dimension (geo-standing-stickman-lower-arm-length self) torso-length)]
             [lft-shoulder (- neck half-shoulder)]
             [rgt-shoulder (+ neck half-shoulder)]
             [lft-elbow (+ lft-shoulder (make-polar upper-length lft-arm-angle))]
             [rgt-elbow (+ rgt-shoulder (make-polar upper-length rgt-arm-angle))]
             [lft-hand (+ lft-elbow (make-polar lower-length (- lft-arm-angle elb-angle)))]
             [rgt-hand (+ rgt-elbow (make-polar lower-length (+ rgt-arm-angle elb-angle)))])
        (values (stickman-arm lft-shoulder lft-elbow lft-hand)
                (stickman-arm rgt-shoulder rgt-elbow rgt-hand))))

    (define-values (lft-leg rgt-leg)
      (let*-values ([(leg-angle) (real->double-flonum (geo-standing-stickman-leg-angle self))]
                    [(leg-length) (~dimension (geo-standing-stickman-leg-length self) torso-length)]
                    [(foot.pt) (make-polar leg-length (+ (* leg-angle 0.5) pi/2))]
                    [(dx dy) (values (real-part foot.pt) (imag-part foot.pt))])
        (values (stickman-leg #false (+ hip (make-rectangular (- dx) dy)))
                (stickman-leg #false (+ hip (make-rectangular (+ dx) dy))))))
    
    (geo-stickman-skeleton head-radius torso-width leg-width arm-width
                           (make-rectangular 0.0 head-radius)
                           neck lft-arm rgt-arm
                           hip lft-leg rgt-leg)))

(define geo-standing-stickman-size : (->* (Geo-Stickman-Skeleton)
                                          (Nonnegative-Flonum #:stroke (Option Pen))
                                          (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [self [fallback-thickness 0.0] #:stroke [stroke #false]]
    (define thick-ext : Nonnegative-Flonum (* (pen-maybe-width stroke fallback-thickness) 2.0))
    (define leg-width : Nonnegative-Flonum (geo-stickman-skeleton-leg-width self))
    (define arm-width : Nonnegative-Flonum (geo-stickman-skeleton-arm-width self))
    (define bbox : Geo-BBox (make-geo-bbox (cons 0.0+0.0i (geo-stickman-skeleton-points self))))
    (define-values (width height _) (geo-bbox-values bbox))

    (values (+ width arm-width thick-ext)
            (+ height (* leg-width 0.36) thick-ext))))
