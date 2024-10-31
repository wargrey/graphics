#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require digimon/metrics)

(require "../../paint.rkt")

(require "../paint.rkt")
(require "../convert.rkt")
(require "../unsafe/dc/shape.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:rect geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [corner-radius : Nonnegative-Flonum]
   [vlines : (Listof Flonum)]
   [hlines : (Listof Flonum)])
  #:type-name Geo:Rectangle
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-square
  (lambda [#:id [id : (Option Symbol) #false] #:stroke [stroke : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Real) null] #:hlines [hlines : (Listof Real) null]
           [width : Real] [corner-radius : Real 0.0]] : Geo:Rectangle
    (define w : Nonnegative-Flonum (~length width))
    
    (create-geometry-object geo:rect
                            #:with [id (geo-draw-rectangle stroke pattern)
                                       (geo-shape-extent w 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            w w (~length corner-radius w)
                            (map real->double-flonum vlines)
                            (map real->double-flonum hlines))))

(define geo-rectangle
  (lambda [#:id [id : (Option Symbol) #false] #:stroke [stroke : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Real) null] #:hlines [hlines : (Listof Real) null]
           [width : Real] [height : Real -0.618] [corner-radius : Real 0.0]]
    (define-values (w h) (~size width height))
    
    (create-geometry-object geo:rect
                            #:with [id (geo-draw-rectangle stroke pattern)
                                       (geo-shape-extent w h 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            w h (~length corner-radius (min w h))
                            (map real->double-flonum vlines)
                            (map real->double-flonum hlines))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-rectangle : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:rect? self)
        (define cradius : Nonnegative-Flonum (geo:rect-corner-radius self))
        (define vls : (Listof Flonum) (geo:rect-vlines self))
        (define hls : (Listof Flonum) (geo:rect-hlines self))
        
        (if (or (zero? cradius) (nan? cradius))
            (dc_rectangle cr x0 y0 width height (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill) vls hls)
            (dc_rounded_rectangle cr x0 y0 width height cradius (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill) vls hls))))))
