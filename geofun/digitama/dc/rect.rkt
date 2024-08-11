#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require digimon/metrics)

(require "../convert.rkt")
(require "../source.rkt")

(require "../unsafe/dc/shape.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:rect geo
  ([corner-radius : Nonnegative-Flonum])
  #:type-name Geo:Rectangle
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-square : (->* (Real) (Real #:id Symbol #:border (Option Stroke-Paint) #:fill (Option Fill-Paint)) Geo:Rectangle)
  (lambda [width [corner-radius 0.0] #:id [id #false] #:border [border #false] #:fill [pattern #false]]
    (define w : Nonnegative-Flonum (~length width))
    (define rect-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox w))
    
    (create-geometry-object geo:rect
                            #:with [(geo-shape-surface-wrapper geo-rect-surface (stroke-paint->source* border) pattern) rect-bbox] #:id id
                            (~length corner-radius w))))

(define geo-rectangle : (->* (Real) (Real Real #:id Symbol #:border (Option Stroke-Paint) #:fill (Option Fill-Paint)) Geo:Rectangle)
  (lambda [width [height -0.618] [corner-radius 0.0] #:id [id #false] #:border [border #false] #:fill [pattern #false]]
    (define-values (w h) (~size width height))
    (define rect-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox w h))
    
    (create-geometry-object geo:rect
                            #:with [(geo-shape-surface-wrapper geo-rect-surface (stroke-paint->source* border) pattern) rect-bbox] #:id id
                            (~length corner-radius (min w h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rect-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:rect?])
      (define cr : Nonnegative-Flonum (geo:rect-corner-radius self))
      (define-values (x y w h) ((geo<%>-aabox self) self))
      
      (if (or (zero? cr) (nan? cr))
          (dc_rectangle create-abstract-surface w h
                        (default-border) (fill-paint->source* (default-fill-paint))
                        (default-geometry-density))
          (dc_rounded_rectangle create-abstract-surface w h cr
                                (default-border) (fill-paint->source* (default-fill-paint))
                                (default-geometry-density))))))
