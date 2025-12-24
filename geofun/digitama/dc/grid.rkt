#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../unsafe/dc/plain.rkt")

(require "../self.rkt")
(require "../convert.rkt")
(require "../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:grid geo
  ([major-xs : (Listof Nonnegative-Flonum)]
   [major-ys : (Listof Nonnegative-Flonum)]
   [minor-xs : (Listof Nonnegative-Flonum)]
   [minor-ys : (Listof Nonnegative-Flonum)])
  #:type-name Geo:Grid
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-grid
  (lambda [#:id [id : (Option Symbol) #false]
           #:major-stroke [major-stroke : Maybe-Stroke-Paint (void)]
           #:minor-stroke [minor-stroke : Maybe-Stroke-Paint (void)]
           #:extra-major-xs [major-xs : (Listof Real) null]
           #:extra-major-ys [major-ys : (Listof Real) null]
           #:extra-minor-xs [minor-xs : (Listof Real) null]
           #:extra-minor-ys [minor-ys : (Listof Real) null]
           [width : Real] [height : Real+%]] : Geo:Grid
    (define-values (flwidth flheight) (~extent width height))
    (define +epsilon : Nonnegative-Flonum 1e-3)
    (define -epsilon : Flonum (- +epsilon))
    
    (create-geometry-object geo:grid
                            #:with [id (geo-draw-grid major-stroke minor-stroke)
                                       (geo-shape-extent flwidth flheight)
                                       (geo-shape-outline major-stroke)]
                            (for/list ([x (in-list major-xs)] #:when (<= -epsilon x (+ flwidth +epsilon)))
                              (~clamp x 0.0 flwidth))
                            (for/list ([y (in-list major-ys)] #:when (<= -epsilon y (+ flheight +epsilon)))
                              (~clamp y 0.0 flheight))
                            (for/list ([x (in-list minor-xs)] #:when (<= -epsilon x (+ flwidth +epsilon)))
                              (~clamp x 0.0 flwidth))
                            (for/list ([y (in-list minor-ys)] #:when (<= -epsilon y (+ flheight +epsilon)))
                              (~clamp y 0.0 flheight)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-grid : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Geo-Surface-Draw!)
  (lambda [alt-major-stroke alt-minor-stroke]
    (λ [self cr x0 y0 width height]
      (when (geo:grid? self)
        (dc_grid cr x0 y0 width height
                 (geo:grid-major-xs self) (geo:grid-major-ys self) (geo-select-stroke-paint alt-major-stroke)
                 (geo:grid-minor-xs self) (geo:grid-minor-ys self) (geo-select-stroke-paint alt-minor-stroke))))))
