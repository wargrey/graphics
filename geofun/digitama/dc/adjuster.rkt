#lang typed/racket/base

(provide (all-defined-out))

(require "../../paint.rkt")
(require "../paint.rkt")
(require "../paint/source.rkt")
(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:adjuster geo
  ([body : Geo])
  #:type-name Geo:Adjuster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-try-repaint : (-> Geo [#:id (Option Symbol)]
                              [#:stroke Maybe-Stroke-Paint]
                              [#:border Maybe-Stroke-Paint]
                              [#:fill Maybe-Fill-Paint]
                              Geo:Adjuster)
  (lambda [geo #:id [id #false] #:stroke [stroke (void)] #:fill [fill (void)] #:border [border (void)]]
    (define body (if (geo:adjuster? geo) (geo:adjuster-body geo) geo))
    
    (create-geometry-object geo:adjuster
                            #:with [id (geo-draw-adjuster! stroke border fill)
                                       (geo<%>-extent body)
                                       (geo-adjuster-outline stroke border)]
                            body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-adjuster-outline : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Geo-Calculate-Outline)
  (lambda [alt-stroke alt-border]
    (λ [[self : Geo<%>] [stroke : Option-Stroke-Paint] [border : Option-Stroke-Paint]] : Geo-Pad
      (geo-outline* (if (geo:adjuster? self) (geo:adjuster-body self) self)
                    (stroke-paint->source* alt-stroke)
                    (border-paint->source* alt-border)))))

(define geo-draw-adjuster! : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-border alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:adjuster? self)
        (define body (geo:adjuster-body self))

        (parameterize ([default-stroke-source (if (void? alt-stroke) (void) (stroke-paint->source* alt-stroke))]
                       [default-border-source (if (void? alt-border) (void) (border-paint->source* alt-border))]
                       [default-fill-source (if (void? alt-fill) (void) (fill-paint->source* alt-fill))])
          ((geo<%>-draw! body) body cr x0 y0 width height))))))
