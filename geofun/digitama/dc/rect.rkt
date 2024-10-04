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
    
    (create-geometry-object geo:rect
                            #:surface geo-rect-surface stroke pattern
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
    
    (create-geometry-object geo:rect
                            #:surface geo-rect-surface stroke pattern
                            #:extent (geo-shape-plain-extent w h 0.0 0.0)
                            #:id id
                            w h (~length corner-radius (min w h))
                            (map real->double-flonum vlines)
                            (map real->double-flonum hlines))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rect-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:rect?])
      (define cr : Nonnegative-Flonum (geo:rect-corner-radius self))
      (define w : Nonnegative-Flonum (geo:rect-width self))
      (define h : Nonnegative-Flonum (geo:rect-height self))
      (define vls : (Listof Flonum) (geo:rect-vlines self))
      (define hls : (Listof Flonum) (geo:rect-hlines self))
      
      (if (or (zero? cr) (nan? cr))
          (dc_rectangle create-abstract-surface w h (current-stroke-source) (current-fill-source) vls hls (default-geometry-density))
          (dc_rounded_rectangle create-abstract-surface w h cr (current-stroke-source) (current-fill-source) vls hls (default-geometry-density))))))
