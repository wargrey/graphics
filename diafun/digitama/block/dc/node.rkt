#lang typed/racket/base

(provide (all-defined-out))

(require "../dc.rkt")
(require "../style.rkt")

(require digimon/measure)

(require geofun/constructor)
(require geofun/digitama/self)

(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/hexagon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-rectangle : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                        Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                        Symbol)
                                                (Any)
                                                Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (create-dia-block #:id id #:tag type etags
                      #:create-with style [geo-rectangle width height]
                      caption)))

(define #:forall (S) dia-block-rectangle/cr:4th : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                               Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                               Symbol)
                                                       (Any)
                                                       Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (create-dia-block #:id id #:tag type etags
                      ;#:fit-region 0.85 1.0
                      #:create-with style [geo-rounded-rectangle width height (&% 25)]
                      caption)))

(define #:forall (S) dia-block-rectangle/cr:8th : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                               Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                               Symbol)
                                                       (Any)
                                                       Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (create-dia-block #:id id #:tag type etags
                      ;#:fit-region 0.85 1.0
                      #:create-with style [geo-rounded-rectangle width height (&% 12.5)]
                      caption)))

(define #:forall (S) dia-block-circle : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                     Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                     Symbol)
                                             (Any)
                                             Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    
    (create-dia-block #:block dia:block:circle
                      #:id id #:tag type etags
                      #:intersect dia-circle-intersect
                      #:create-with style [geo-circle r]
                      caption r)))

(define #:forall (S) dia-block-ellipse : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                      Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                      Symbol)
                                              (Any)
                                              Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (define thickness (dia-block-resolve-stroke-width style))
    
    (create-dia-block #:block dia:block:ellipse
                      #:id id #:tag type etags
                      #:intersect dia-ellipse-intersect
                      #:fit-region 0.81 0.875
                      #:create-with style [geo-ellipse width height]
                      caption
                      (+ (* width 0.5) thickness)
                      (+ (* height 0.5) thickness))))

(define #:forall (S) dia-block-polygon : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S) (Option Flonum)
                                                      (List* Float-Complex Float-Complex (Listof Float-Complex))
                                                      Flonum Flonum Flonum Flonum
                                                      Symbol)
                                              (Any)
                                              Dia:Block)
  (lambda [id caption style direction vertices hfit% vfit% lft% top% type [etags #false]]
    (create-dia-block #:block dia:block:polygon
                      #:id id #:tag type etags
                      #:intersect dia-polygon-intersect
                      #:fit-region hfit% vfit% lft% top%
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption vertices)))

(define #:forall (S) dia-block-symmetric-polygon : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S) (Option Flonum)
                                                                (List* Float-Complex Float-Complex (Listof Float-Complex))
                                                                Flonum Flonum
                                                                Symbol)
                                                        (Any)
                                                        Dia:Block)
  (lambda [id caption style direction vertices hfit% vfit% type [etags #false]]
    (create-dia-block #:block dia:block:polygon
                      #:id id #:tag type etags
                      #:intersect dia-polygon-intersect
                      #:fit-region hfit% vfit%
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption vertices)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-diamond : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                      Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                      Symbol)
                                              (Any)
                                              Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (dia-block-symmetric-polygon id caption style direction
                                 (geo-rhombus-vertices width height) 0.85 0.85
                                 type etags)))

(define #:forall (S) dia-block-parallelogram : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                            Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                            Symbol)
                                                    (Any)
                                                    Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (dia-block-symmetric-polygon id caption style direction
                                 (geo-parallelogram-vertices width height pi/3)
                                 (- 1.0 (/ (* height (sqrt 3.0) 2/3) width)) 1.00
                                 type etags)))

(define #:forall (S) dia-block-hexagon : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                      Nonnegative-Flonum Nonnegative-Flonum (Option Flonum)
                                                      Symbol)
                                              (Any)
                                              Dia:Block)
  (lambda [id caption style width height direction type [etags #false]]
    (dia-block-symmetric-polygon id caption style direction
                                 (geo-hexagon-tile-vertices width height) 0.75 1.00
                                 type etags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-polygon-shape : (-> (Option Symbol) (Dia-Block-Style-Spec S) (Listof Float-Complex) Geo)
  (lambda [shape-key style vertices]
    (geo-polygon #:id (and shape-key (dia-block-shape-id shape-key))
                 #:stroke (dia-block-resolve-stroke-paint style)
                 #:fill (dia-block-resolve-fill-paint style)
                 #:window +nan.0+nan.0i
                 vertices)))
