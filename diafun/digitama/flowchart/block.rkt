#lang typed/racket/base

(provide (all-defined-out))

(require "../block/dc.rkt")
(require "../block/style.rkt")
(require "../block/shape.rkt")
(require "../interface.rkt")

(require geofun/constructor)
(require geofun/composite)

(require geofun/digitama/self)
(require geofun/digitama/geometry/constants)
(require geofun/digitama/geometry/polygon/triangle)
(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/pentagon)
(require geofun/digitama/geometry/polygon/hexagon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-process : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (cond [(eq? subtype 'Predefined) (diaflow-block-prefab block-key caption style width height direction subtype)]
          [(eq? subtype 'Alternate)  (diaflow-block-alternate block-key caption style width height direction subtype)]
          [else (create-dia-block #:id block-key #:type 'Process subtype
                                  #:create-with style [geo-rectangle width height]
                                  caption)])))

(define diaflow-block-prefab : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define width% : Nonnegative-Flonum 0.84)
    (define vline : Flonum (* (- 1.0 width%) 0.5))
    
    (create-dia-block #:id block-key #:type 'Process subtype
                      #:fit-region width% 1.0
                      #:create-with style [geo-rectangle #:vlines (list vline (- vline)) width height]
                      caption)))

(define diaflow-block-alternate : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (create-dia-block #:id block-key #:type 'Alternate subtype
                      #:fit-region 0.85 1.0
                      #:create-with style [geo-rectangle width height '(25 %)]
                      caption)))

(define diaflow-block-decision : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define vertices (geo-rhombus-vertices width height))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Decision subtype
                      #:intersect dia-polygon-intersect
                      #:fit-region 0.75 0.75
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption vertices)))

(define diaflow-block-preparation : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define vertices (geo-hexagon-tile-vertices width height))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Preparation subtype
                      #:intersect dia-polygon-intersect
                      #:fit-region 0.75 1.00
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption vertices)))

(define diaflow-block-terminal : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define r : Nonnegative-Flonum (* height 0.5))
    
    (create-dia-block #:id block-key #:type 'Terminal subtype
                      #:fit-region (max (/ (- width r) width) 0.0) 1.00
                      #:create-with style [geo-stadium (- width (* r 2.0)) r]
                      caption)))

(define diaflow-block-input : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (if (eq? subtype 'user)
        (let* ([ratio 0.80]
               [vertices (geo-keyboard-vertices width height ratio)])
          (create-dia-block #:block dia:block:polygon
                            #:id block-key #:type 'Input subtype
                            #:intersect dia-polygon-intersect
                            #:fit-region 1.0 ratio 0.0 (- 1.0 ratio)
                            #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                            caption vertices))
        (diaflow-block-dataIO block-key caption style width height 'Input subtype))))

(define diaflow-block-output : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (if (eq? subtype 'user)
        (let-values ([(ogive barrel r) (values (* width 0.384) (* width 0.618) (* height 0.5))])
          (create-dia-block #:id block-key #:type 'Output subtype
                            #:fit-region 0.75 1.00 (/ (- ogive r) width) 0.0
                            #:create-with style [geo-bullet ogive r barrel]
                            caption))
        (diaflow-block-dataIO block-key caption style width height 'Output subtype))))

(define diaflow-block-inspection : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    
    (create-dia-block #:block dia:block:circle
                      #:id block-key #:type 'Connector (or subtype 'Inspection)
                      #:intersect dia-circle-intersect
                      #:fit-region 0.95 0.95
                      #:create-with style [geo-circle r]
                      caption r)))

(define diaflow-block-reference : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define ratio : Nonnegative-Flonum 0.618)
    (define vertices (geo-house-vertices width height (- ratio)))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Connector (or subtype 'Reference)
                      #:intersect dia-polygon-intersect
                      #:fit-region 1.00 ratio 0.0 0.0
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption vertices)))

(define diaflow-block-selection : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    
    (create-dia-block #:block dia:block:circle
                      #:id block-key #:type 'Selection subtype
                      #:intersect dia-circle-intersect
                      #:fit-region 0.75 0.75
                      #:create-with style [geo-circle #:diameters (list 0.0 pi/2) r]
                      #false r)))

(define diaflow-block-junction : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))

    (create-dia-block #:block dia:block:circle
                      #:id block-key #:type 'Junction subtype
                      #:intersect dia-circle-intersect
                      #:fit-region 0.75 0.75
                      #:create-with style [geo-circle #:diameters (list pi/4 3pi/4) r]
                      #false r)))

(define diaflow-block-manual-operation : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define ratio : Nonnegative-Flonum 0.75)
    (define t% : Nonnegative-Flonum (abs (/ 1.0 ratio)))
    (define vertices (geo-isosceles-trapezium-vertices width height t%))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Operation subtype
                      #:intersect dia-polygon-intersect
                      #:fit-region 1.00 ratio
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption vertices)))

(define diaflow-block-extract : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define vertices (geo-isosceles-upwards-triangle-vertices width height))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Extract subtype
                      #:intersect dia-polygon-intersect
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      #false vertices)))

(define diaflow-block-merge : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define vertices (geo-isosceles-downwards-triangle-vertices width height))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Merge subtype
                      #:intersect dia-polygon-intersect
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      #false vertices)))

(define diaflow-block-delay : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define r : Nonnegative-Flonum (* height 0.5))
    
    (create-dia-block #:id block-key #:type 'Delay subtype
                      #:fit-region (- 1.0 (/ r width)) 1.00 (* (/ r width) 0.25) 0.0
                      #:create-with style [geo-rstadium (- width r) r]
                      caption)))

(define diaflow-block-collation : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (create-dia-block #:id block-key #:type 'Collate subtype
                      #:fit-region 0.5 0.375 0.25 0.0
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i (geo-poor-hourglass-vertices width height)]
                      caption)))

(define diaflow-block-sort : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define h/2 : Nonnegative-Flonum (* height 0.5))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Sort subtype
                      #:intersect dia-polygon-intersect
                      #:fit-region 0.5 0.375 0.25 0.125
                      #:with (geo-vc-append #:id (dia-block-shape-id block-key)
                                            (dia-polygon-shape #false style (geo-isosceles-upwards-triangle-vertices width h/2))
                                            (dia-polygon-shape #false style (geo-isosceles-downwards-triangle-vertices width h/2)))
                      caption (geo-rhombus-vertices width height))))

(define diaflow-block-storage : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (cond [(eq? subtype 'Memory) (diaflow-block-memory block-key caption style width height direction subtype)]
          [(eq? subtype 'File) (diaflow-block-document block-key caption style width height direction subtype)]
          [(eq? subtype 'Directory) (diaflow-block-multiple-document block-key caption style width height direction subtype)]
          [(eq? subtype 'Database) (diaflow-block-database block-key caption style width height direction subtype)]
          [else (let* ([aradius (* height 0.5 0.384)]
                       [ar% (/ aradius width)])
                  (create-dia-block #:id block-key #:type 'Storage subtype
                                    #:fit-region (- 1.0 ar% ar%) 1.0 (* ar% 0.5) 0.0
                                    #:create-with style [geo-storage width height aradius]
                                    caption))])))

(define diaflow-block-memory : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define line% : Nonnegative-Flonum 0.1618)
    (define caption-pos : Nonnegative-Flonum (min (* width line%) (* height line%)))
    (define left% : Flonum (/ caption-pos width))
    (define top% : Flonum (/ caption-pos height))
    
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-region (- 1.0 left%) (- 1.0 top%) left% top%
                      #:create-with style [geo-rectangle #:vlines (list caption-pos) #:hlines (list caption-pos) width height]
                      caption)))

(define diaflow-block-document : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define hratio : Nonnegative-Flonum 0.85)
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-region 1.0 hratio 0.0 0.0
                      #:create-with style [geo-document width height `(,(* (- 1.0 hratio) 0.5) :)]
                      caption)))

(define diaflow-block-multiple-document : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define extra-n : Index 2)
    (define gapsize : Nonnegative-Flonum (* height 0.1))
    (define ngap : Nonnegative-Flonum (* gapsize (real->double-flonum extra-n)))
    (define offset% : Flonum (/ ngap width))
    (define wave% : Nonnegative-Flonum 0.25)
    
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-region (- 1.00 offset%) (- 1.00 wave% offset%) 0.0 offset%
                      #:create-with style [geo-document #:gapsize gapsize #:extra-n extra-n width height `(,(* wave% 0.5) :)]
                      caption)))

(define diaflow-block-database : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (define extra-n : Index 2)
    (define bradius : Nonnegative-Flonum (* height 0.1618))
    (define gapsize : Nonnegative-Flonum (* bradius 0.618))
    (define caption-height-zone : Flonum (- height (* bradius 3.0) (* gapsize (exact->inexact extra-n))))
    
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-region 1.0 (/ caption-height-zone height) 0.0 0.5
                      #:create-with style [geo-database #:extra-n extra-n #:gapsize gapsize width height bradius]
                      caption)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-dataIO : (-> Symbol (Option Geo) Dia-Block-Style-Layers Nonnegative-Flonum Nonnegative-Flonum Symbol (Option Symbol) Dia:Block)
  (lambda [block-key caption style width height type subtype]
    (define rad : Flonum (/ pi 3.0))
    (define vertices (geo-parallelogram-vertices width height rad))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type type subtype
                      #:intersect dia-polygon-intersect
                      #:fit-region (- 1.0 (/ (* height (sqrt 3.0) 2/3) width)) 1.0
                      #:create-with style [geo-polygon #:window +nan.0+nan.0i vertices]
                      caption vertices)))
