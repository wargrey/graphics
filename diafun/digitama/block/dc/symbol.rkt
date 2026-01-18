#lang typed/racket/base

(provide (all-defined-out))

(require "../dc.rkt")
(require "../style.rkt")

(require geofun/constructor)
(require geofun/digitama/geometry/polygon/triangle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-symbol-circle : (->* (Symbol (Listof Real) (Dia-Block-Style-Spec S)
                                                      Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                      Symbol)
                                              (Any)
                                              Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    
    (create-dia-block #:block dia:block:circle
                      #:id id #:tag type etags
                      #:intersect dia-circle-intersect
                      #:create-with style [geo-circle r #:diameters caption]
                      #false r)))

(define #:forall (S) dia-symbol-regular-triangle : (->* (Symbol (Dia-Block-Style-Spec S)
                                                                Nonnegative-Flonum (Option Flonum)
                                                                Symbol Symbol)
                                                        (Any)
                                                        Dia:Block)
  (lambda [block-key style height direction apex-position type [etags #false]]
    (define width (* height 2.0 (/ 1.0 (sqrt 3.0))))
    (define vertices
      (if (eq? apex-position 'apex@top)
          (geo-isosceles-triangle-vertices/apex@top width height)
          (geo-isosceles-triangle-vertices/apex@bot width height)))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:tag type etags
                      #:intersect dia-polygon-intersect
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      #false vertices)))
