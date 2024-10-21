#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require digimon/metrics)

(require "paint.rkt")
(require "../convert.rkt")
(require "../../paint.rkt")
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
(define geo-square : (->* (Real)
                          (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint
                                #:vlines (Listof Real) #:hlines (Listof Real))
                          Geo:Rectangle)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)] #:vlines [vlines null] #:hlines [hlines null]
           width [corner-radius 0.0]]
    (define w : Nonnegative-Flonum (~length width))
    
    (create-geometry-object geo:rect (geo-draw-rectangle stroke pattern)
                            #:extent (geo-shape-plain-extent w 0.0 0.0)
                            #:id id
                            w w (~length corner-radius w)
                            (map real->double-flonum vlines)
                            (map real->double-flonum hlines))))

(define geo-rectangle : (->* (Real)
                             (Real Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint
                                   #:vlines (Listof Real) #:hlines (Listof Real))
                             Geo:Rectangle)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)] #:vlines [vlines null] #:hlines [hlines null]
           width [height -0.618] [corner-radius 0.0]]
    (define-values (w h) (~size width height))
    
    (create-geometry-object geo:rect (geo-draw-rectangle stroke pattern)
                            #:extent (geo-shape-plain-extent w h 0.0 0.0)
                            #:id id
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
