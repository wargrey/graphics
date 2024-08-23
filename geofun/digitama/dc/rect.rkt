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
  ([corner-radius : Nonnegative-Flonum])
  #:type-name Geo:Rectangle
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-square : (->* (Real) (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Rectangle)
  (lambda [width [corner-radius 0.0] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define w : Nonnegative-Flonum (~length width))
    (define rect-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox w))
    
    (create-geometry-object geo:rect
                            #:with [(geo-shape-surface-wrapper geo-rect-surface stroke pattern) rect-bbox] #:id id
                            (~length corner-radius w))))

(define geo-rectangle : (->* (Real) (Real Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Rectangle)
  (lambda [width [height -0.618] [corner-radius 0.0] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (w h) (~size width height))
    (define rect-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox w h))
    
    (create-geometry-object geo:rect
                            #:with [(geo-shape-surface-wrapper geo-rect-surface stroke pattern) rect-bbox] #:id id
                            (~length corner-radius (min w h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rect-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:rect?])
      (define cr : Nonnegative-Flonum (geo:rect-corner-radius self))
      (define-values (x y w h) ((geo<%>-aabox self) self))
      
      (if (or (zero? cr) (nan? cr))
          (dc_rectangle create-abstract-surface w h (current-stroke-source) (current-fill-source) (default-geometry-density))
          (dc_rounded_rectangle create-abstract-surface w h cr (current-stroke-source) (current-fill-source) (default-geometry-density))))))
