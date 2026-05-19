#lang typed/racket/base

(provide (all-defined-out))

(require "../self.rkt")
(require "../convert.rkt")
(require "../geometry/bleed.rkt")

(require "../paint.rkt")
(require "../paint/source.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:adjuster geo
  ([body : Geo])
  #:type-name Geo:Adjuster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-try-repaint
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:border [border : Maybe-Stroke-Paint (void)]
           #:fill [fill : Maybe-Fill-Paint (void)]
           [geo : Geo]]
    (define body (if (geo:adjuster? geo) (geo:adjuster-body geo) geo))
    
    (create-geometry-object geo:adjuster
                            #:with [id (geo-draw-adjuster! body stroke border fill)
                                       (geo-delegate-expand body)
                                       (geo-adjuster-bleed body stroke border)]
                            body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-adjuster-bleed : (-> Geo Maybe-Stroke-Paint Maybe-Stroke-Paint Geo-Calculate-Bleed)
  (lambda [body alt-stroke alt-border]
    (λ [[self : Geo<%>] [stroke : Option-Stroke-Paint] [border : Option-Stroke-Paint]] : Geo-Bleed
      (geo-bleed* body
                  (stroke-paint->source* alt-stroke)
                  (border-paint->source* alt-border)))))

(define geo-draw-adjuster! : (-> Geo Maybe-Stroke-Paint Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [body alt-stroke alt-border alt-fill]
    (λ [self cr x0 y0 width height]
      (parameterize ([default-stroke-source (if (void? alt-stroke) (void) (stroke-paint->source* alt-stroke))]
                     [default-border-source (if (void? alt-border) (void) (border-paint->source* alt-border))]
                     [default-fill-source (if (void? alt-fill) (void) (fill-paint->source* alt-fill))])
        ((geo<%>-draw! body) body cr x0 y0 width height)))))
