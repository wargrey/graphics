#lang typed/racket/base

(provide (all-defined-out))

(require "../dc.rkt")
(require "../style.rkt")

(require digimon/measure)

(require geofun/font)
(require geofun/composite)
(require geofun/constructor)

(require geofun/digitama/self)
(require geofun/digitama/dc/resize)
(require geofun/digitama/geometry/sides)

(require geofun/digitama/geometry/polygon/hexagon)
(require geofun/digitama/geometry/polygon/quadrilateral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-rectangle : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                        Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                                (Any (Option Symbol) (Option Font))
                                                Dia:Block)
  (lambda [key caption style width height direction [tags #false] [sotype #false] [sofont #false]]
    (if (and caption sotype)
        (let*-values ([(padding) (dia-block-resolve-padding style)]
                      [(top rgt bot lft) (geo-inset-values padding)]
                      [(stereotype) (dia-block-stereotype sotype style sofont (- width lft rgt))])
          (create-dia-block #:id key tags
                            #:fit-region 1.0 (- 1.0 (/ (geo-height stereotype) height)) 0.0 1.0
                            #:margin padding
                            #:with style
                            (geo-pin* #:id (dia-block-shape-id key)
                                      0.5 (/ top height) 0.5 0.0
                                      (geo-rectangle #:stroke (dia-block-resolve-stroke-paint style)
                                                     #:fill (dia-block-resolve-fill-paint style)
                                                     width height)
                                      stereotype)
                            caption))
        (create-dia-block #:id key tags
                          #:create-with style [geo-rectangle width height]
                          caption))))

(define #:forall (S) dia-block-rounded-rectangle : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                                Nonnegative-Flonum Nonnegative-Flonum Length+% (Option Flonum))
                                                        (Any (Option Symbol) (Option Font))
                                                        Dia:Block)
  (lambda [key caption style width height corner-radius direction [tags #false] [sotype #false] [sofont #false]]
    (if (and caption sotype)
        (let*-values ([(padding) (dia-block-resolve-padding style)]
                      [(top rgt bot lft) (geo-inset-values padding)]
                      [(stereotype) (dia-block-stereotype sotype style sofont (- width lft rgt))])
          (create-dia-block #:id key tags
                            #:fit-region 1.0 (- 1.0 (/ (geo-height stereotype) height)) 0.0 1.0
                            #:margin padding
                            #:with style
                            (geo-pin* #:id (dia-block-shape-id key)
                                      0.5 (/ top height) 0.5 0.0
                                      (geo-rounded-rectangle #:stroke (dia-block-resolve-stroke-paint style)
                                                             #:fill (dia-block-resolve-fill-paint style)
                                                             width height corner-radius)
                                      stereotype)
                            caption))
        (create-dia-block #:id key tags
                          #:create-with style [geo-rounded-rectangle width height corner-radius]
                          caption))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-circle : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                     Nonnegative-Flonum Nonnegative-Flonum)
                                             (Any)
                                             Dia:Block)
  (lambda [key caption style width height [tags #false]]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    
    (create-dia-block #:block dia:block:circle #:id key tags
                      #:intersect dia-circle-intersect
                      #:create-with style [geo-circle r]
                      caption r)))

(define #:forall (S) dia-block-ellipse : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                      Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                              (Any)
                                              Dia:Block)
  (lambda [key caption style width height direction [tags #false]]
    (define thickness (dia-block-resolve-stroke-width style))
    
    (create-dia-block #:block dia:block:ellipse #:id key tags
                      #:intersect dia-ellipse-intersect
                      #:fit-region 0.81 0.875
                      #:create-with style [geo-ellipse width height]
                      caption
                      (+ (* width 0.5) thickness)
                      (+ (* height 0.5) thickness))))

(define #:forall (S) dia-block-polygon : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S) (Option Flonum)
                                                      (List* Float-Complex Float-Complex (Listof Float-Complex))
                                                      Flonum Flonum Flonum Flonum)
                                              (Any (Option (List* Float-Complex Float-Complex (Listof Float-Complex))))
                                              Dia:Block)
  (lambda [key caption style direction vertices hfit% vfit% lft% top% [tags #false] [alt-vertices #false]]
    (create-dia-block #:block dia:block:polygon #:id key tags
                      #:intersect dia-polygon-intersect
                      #:fit-region hfit% vfit% lft% top%
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption (or alt-vertices vertices))))

(define #:forall (S) dia-block-symmetric-polygon : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S) (Option Flonum)
                                                                (List* Float-Complex Float-Complex (Listof Float-Complex))
                                                                Flonum Flonum)
                                                        (Any)
                                                        Dia:Block)
  (lambda [key caption style direction vertices hfit% vfit% [tags #false]]
    (create-dia-block #:block dia:block:polygon #:id key tags
                      #:intersect dia-polygon-intersect
                      #:fit-region hfit% vfit%
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption vertices)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-rectangle/cr:2nd : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                               Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                                       (Any (Option Symbol) (Option Font))
                                                       Dia:Block)
  (lambda [key caption style width height direction [tags #false] [sotype #false] [sofont #false]]
    (dia-block-rounded-rectangle key caption style width height (&% 50) direction tags sotype sofont)))

(define #:forall (S) dia-block-rectangle/cr:4th : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                               Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                                       (Any (Option Symbol) (Option Font))
                                                       Dia:Block)
  (lambda [key caption style width height direction [tags #false] [sotype #false] [sofont #false]]
    (dia-block-rounded-rectangle key caption style width height (&% 25) direction tags sotype sofont)))

(define #:forall (S) dia-block-rectangle/cr:8th : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                               Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                                       (Any (Option Symbol) (Option Font))
                                                       Dia:Block)
  (lambda [key caption style width height direction [tags #false] [sotype #false] [sofont #false]]
    (dia-block-rounded-rectangle key caption style width height (&% 12.5) direction tags sotype sofont)))


(define #:forall (S) dia-block-diamond : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                      Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                              (Any)
                                              Dia:Block)
  (lambda [key caption style width height direction [tags #false]]
    (dia-block-symmetric-polygon key caption style direction
                                 (geo-rhombus-vertices width height) 0.85 0.85
                                 tags)))

(define #:forall (S) dia-block-parallelogram : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                            Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                                    (Any)
                                                    Dia:Block)
  (lambda [key caption style width height direction [tags #false]]
    (dia-block-symmetric-polygon key caption style direction
                                 (geo-parallelogram-vertices width height pi/3)
                                 (- 1.0 (/ (* height (sqrt 3.0) 2/3) width)) 1.00
                                 tags)))

(define #:forall (S) dia-block-hexagon : (->* (Symbol (Option Geo) (Dia-Block-Style-Spec S)
                                                      Nonnegative-Flonum Nonnegative-Flonum (Option Flonum))
                                              (Any)
                                              Dia:Block)
  (lambda [key caption style width height direction [tags #false]]
    (dia-block-symmetric-polygon key caption style direction
                                 (geo-hexagon-tile-vertices width height) 0.75 1.00
                                 tags)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-block-stereotype : (-> Symbol (Dia-Block-Style-Spec S) (Option Font+Tweak) Flonum Geo)
  (lambda [stereotype style stereotype-font max-width]
    (define stereotype.txt : Geo:Text
      (geo-text #:color (dia-block-resolve-font-paint style)
                (format "«~a»" stereotype)
                (cond [(font? stereotype-font) stereotype-font]
                      [(not stereotype-font) (dia-block-resolve-font style)]
                      [else (desc-font* (dia-block-resolve-font style) #:tweak stereotype-font)])))

    (cond[(<= max-width 0.0) stereotype.txt]
         [else (let ([swidth (geo-width stereotype.txt)])
                 (cond [(<= swidth max-width) stereotype.txt]
                       [else (geo-scale stereotype.txt (/ max-width swidth) 1.0)]))])))

(define #:forall (S) dia-polygon-shape : (-> (Option Symbol) (Dia-Block-Style-Spec S) (Listof Float-Complex) Geo)
  (lambda [key style vertices]
    (geo-polygon #:id (and key (dia-block-shape-id key))
                 #:stroke (dia-block-resolve-stroke-paint style)
                 #:fill (dia-block-resolve-fill-paint style)
                 #:window +nan.0+nan.0i
                 vertices)))
