#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/constructor)
(require geofun/digitama/geometry/polygon/triangle)
(require geofun/digitama/geometry/polygon/quadrilateral)

(require "../dc.rkt")
(require "../style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-symbol-circle : (->* (Symbol (Dia-Block-Style-Spec S) Nonnegative-Flonum Nonnegative-Flonum (Listof Real))
                                              (Any)
                                              Dia:Block)
  (lambda [key style width height diameters [tags #false]]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    
    (create-dia-block #:block dia:block:circle #:id key tags
                      #:intersect dia-circle-intersect
                      #:create-with style [geo-circle r #:diameters diameters]
                      #false r)))

(define #:forall (S) dia-symbol-bullseye : (->* (Symbol (Dia-Block-Style-Spec S) Nonnegative-Flonum Nonnegative-Flonum Length+%)
                                                (Any)
                                                Dia:Block)
  (lambda [key style width height r% [tags #false]]
    (define R : Nonnegative-Flonum (* (min width height) 0.5))
    (define r : Nonnegative-Flonum (~placement r% R))
    
    (create-dia-block #:block dia:block:circle #:id key tags
                      #:intersect dia-circle-intersect
                      #:with style
                      (geo-bullseye #:id (dia-block-shape-id key)
                                    #:stroke (dia-block-resolve-stroke-paint style)
                                    #:fill #false
                                    #:eye-stroke #false
                                    #:eye-fill (dia-block-resolve-fill-paint style)
                                    R r)
                      #false R)))

(define #:forall (S) dia-symbol-diamond : (->* (Symbol (Dia-Block-Style-Spec S) Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                               (Any)
                                               Dia:Block)
  (lambda [key style width height direction [tags #false]]
    (define size : Nonnegative-Flonum (min width height))
    (define vertices (geo-rhombus-vertices size size))
    
    (create-dia-block #:block dia:block:polygon #:id key tags
                      #:intersect dia-polygon-intersect
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      #false vertices)))

(define #:forall (S) dia-symbol-regular-triangle : (->* (Symbol (Dia-Block-Style-Spec S) Nonnegative-Flonum (Option Flonum) Symbol)
                                                        (Any)
                                                        Dia:Block)
  (lambda [key style height direction apex-position [tags #false]]
    (define width (* height 2.0 (/ 1.0 (sqrt 3.0))))
    (define vertices
      (if (eq? apex-position 'apex@top)
          (geo-isosceles-triangle-vertices/apex@top width height)
          (geo-isosceles-triangle-vertices/apex@bot width height)))
    
    (create-dia-block #:block dia:block:polygon #:id key tags
                      #:intersect dia-polygon-intersect
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      #false vertices)))
