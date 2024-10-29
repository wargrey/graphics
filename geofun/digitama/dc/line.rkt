#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/shape.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:line geo
  ([x : Flonum]
   [y : Flonum]
   [dx : Flonum]
   [dy : Flonum])
  #:type-name Geo:Line
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-hline : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint) Geo:Line)
  (lambda [#:id [id #false] #:stroke [stroke (void)] width height]
    (define-values (flwidth flheight) (~size width height))
    
    (create-geometry-object geo:line
                            #:with [id (geo-draw-surface stroke)
                                       (geo-shape-plain-extent flwidth flheight)]
                            0.0 (* flheight 0.5) flwidth 0.0)))

(define geo-vline : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint) Geo:Line)
  (lambda [#:id [id #false] #:stroke [stroke (void)] width height]
    (define-values (flwidth flheight) (~size width height))
    
    (create-geometry-object geo:line
                            #:with [id (geo-draw-surface stroke)
                                       (geo-shape-plain-extent flwidth flheight)]
                            (* flwidth 0.5) 0.0 0.0 flheight)))

(define geo-diagonal : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint) Geo:Line)
  (lambda [#:id [id #false] #:stroke [stroke (void)] width height]
    (define-values (flwidth flheight) (~size width height))
    
    (create-geometry-object geo:line
                            #:with [id (geo-draw-surface stroke)
                                       (geo-shape-plain-extent flwidth flheight)]
                            0.0 0.0 flwidth flheight)))

(define geo-anti-diagonal : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint) Geo:Line)
  (lambda [#:id [id #false] #:stroke [stroke (void)] width height]
    (define-values (flwidth flheight) (~size width height))
    
    (create-geometry-object geo:line
                            #:with [id (geo-draw-surface stroke)
                                       (geo-shape-plain-extent flwidth flheight)]
                            0.0 flheight flwidth (- flheight))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-surface : (-> Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke]
    (λ [self cr x0 y0 width height]
      (when (geo:line? self)
        (dc_line cr x0 y0 width height
                 (geo:line-x self) (geo:line-y self) (geo:line-dx self) (geo:line-dy self)
                 (geo-select-stroke-paint* alt-stroke))))))
  