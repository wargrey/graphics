#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct stickman-arm
  ([shoulder : Float-Complex]
   [elbow : Float-Complex]
   [hand : Float-Complex])
  #:type-name Stickman-Arm
  #:transparent)

(struct stickman-leg
  ([knee : (Option Float-Complex)]
   [foot : Float-Complex])
  #:type-name Stickman-Leg
  #:transparent)

(struct geo-stickman-skeleton
  ([head-radius : Nonnegative-Flonum]
   [torso-width : Nonnegative-Flonum]
   [leg-width : Nonnegative-Flonum]
   [arm-width : Nonnegative-Flonum]
   [head : Float-Complex]
   [neck : Float-Complex]
   [left-arm : Stickman-Arm]
   [right-arm : Stickman-Arm]
   [hip : Float-Complex]
   [left-leg : Stickman-Leg]
   [right-leg : Stickman-Leg])
  #:type-name Geo-Stickman-Skeleton
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stickman-arm-points : (case-> [Stickman-Arm -> (List Float-Complex Float-Complex Float-Complex)]
                                      [Stickman-Arm (-> Float-Complex Float-Complex) -> (List Float-Complex Float-Complex Float-Complex)])
  (case-lambda
    [(self)
     (list (stickman-arm-shoulder self)
           (stickman-arm-elbow self)
           (stickman-arm-hand self))]
    [(self transform)
     (list (transform (stickman-arm-shoulder self))
           (transform (stickman-arm-elbow self))
           (transform (stickman-arm-hand self)))]))

(define stickman-leg-points : (case-> [Stickman-Leg -> (Pairof Float-Complex (Listof Float-Complex))]
                                      [Stickman-Leg (-> Float-Complex Float-Complex) -> (Pairof Float-Complex (Listof Float-Complex))])
  (case-lambda
    [(self)
     (define knee (stickman-leg-knee self))
     (define foot (stickman-leg-foot self))
     
     (if (or knee)
         (list knee foot)
         (list foot))]
    [(self transform)
     (define knee (stickman-leg-knee self))
     (define foot (stickman-leg-foot self))
     
     (if (or knee)
         (list (transform knee) (transform foot))
         (list (transform foot)))]))

(define geo-stickman-skeleton-points : (case-> [Geo-Stickman-Skeleton -> (Pairof Float-Complex (Listof Float-Complex))]
                                               [Geo-Stickman-Skeleton (-> Float-Complex Float-Complex) -> (Pairof Float-Complex (Listof Float-Complex))])
  (case-lambda
    [(self)
     (append (list (geo-stickman-skeleton-head self)
                   (geo-stickman-skeleton-neck self)
                   (geo-stickman-skeleton-hip self))
             (stickman-arm-points (geo-stickman-skeleton-left-arm self))
             (stickman-arm-points (geo-stickman-skeleton-right-arm self))
             (stickman-leg-points (geo-stickman-skeleton-left-leg self))
             (stickman-leg-points (geo-stickman-skeleton-right-leg self)))]
    [(self transform)
     (append (list (transform (geo-stickman-skeleton-head self))
                   (transform (geo-stickman-skeleton-neck self))
                   (transform (geo-stickman-skeleton-hip self)))
             (stickman-arm-points (geo-stickman-skeleton-left-arm self) transform)
             (stickman-arm-points (geo-stickman-skeleton-right-arm self) transform)
             (stickman-leg-points (geo-stickman-skeleton-left-leg self) transform)
             (stickman-leg-points (geo-stickman-skeleton-right-leg self) transform))]))
