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
   [corner-radius : Nonnegative-Flonum])
  #:type-name Geo:Rectangle
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-square : (->* (Real) (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Rectangle)
  (lambda [width [corner-radius 0.0] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define w : Nonnegative-Flonum (~length width))
    
    (create-geometry-object geo:rect
                            #:surface geo-rect-surface stroke pattern
                            #:bbox (geo-shape-plain-bbox w)
                            #:id id
                            w w (~length corner-radius w))))

(define geo-rectangle : (->* (Real) (Real Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Rectangle)
  (lambda [width [height -0.618] [corner-radius 0.0] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (w h) (~size width height))
    
    (create-geometry-object geo:rect
                            #:surface geo-rect-surface stroke pattern
                            #:bbox (geo-shape-plain-bbox w h)
                            #:id id
                            w h (~length corner-radius (min w h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rect-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:rect?])
      (define cr : Nonnegative-Flonum (geo:rect-corner-radius self))
      (define w : Nonnegative-Flonum (geo:rect-width self))
      (define h : Nonnegative-Flonum (geo:rect-height self))
      
      (if (or (zero? cr) (nan? cr))
          (dc_rectangle create-abstract-surface w h (current-stroke-source) (current-fill-source) (default-geometry-density))
          (dc_rounded_rectangle create-abstract-surface w h cr (current-stroke-source) (current-fill-source) (default-geometry-density))))))
