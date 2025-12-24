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
  (lambda [block-key brief style width height direction subtype]
    (cond [(eq? subtype 'Predefined) (diaflow-block-prefab block-key brief style width height direction subtype)]
          [(eq? subtype 'Alternate)  (diaflow-block-alternate block-key brief style width height direction subtype)]
          [else (create-dia-block #:id block-key #:type 'Process subtype
                                  (geo-rectangle #:id (dia-block-shape-id block-key)
                                                 #:stroke (dia-block-select-stroke-paint style)
                                                 #:fill (dia-block-select-fill-paint style)
                                                 width height)
                                  brief)])))

(define diaflow-block-prefab : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define width-ratio : Nonnegative-Flonum 0.84)
    (define vline : Flonum (* (- 1.0 width-ratio) 0.5))
    
    (create-dia-block #:id block-key #:type 'Process subtype
                      #:fit-ratio width-ratio 1.0
                      (geo-rectangle #:id (dia-block-shape-id block-key) #:vlines (list vline (- vline))
                                     #:stroke (dia-block-select-stroke-paint style)
                                     #:fill (dia-block-select-fill-paint style)
                                     width height)
                      brief)))

(define diaflow-block-alternate : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (create-dia-block #:id block-key #:type 'Alternate subtype
                      #:fit-ratio 0.85 1.0
                      (geo-rectangle #:id (dia-block-shape-id block-key)
                                     #:stroke (dia-block-select-stroke-paint style)
                                     #:fill (dia-block-select-fill-paint style)
                                     width height '(25 %))
                      brief)))

(define diaflow-block-decision : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (create-dia-block #:block dia:block:polygon
                      #:id block-key  #:type 'Decision subtype
                      #:intersect dia-polygon-intersect
                      #:fit-ratio 0.64 0.64
                      (dia-polygon-shape block-key style (geo-rhombus-vertices width height))
                      brief
                      (geo-rhombus-vertices width height))))

(define diaflow-block-preparation : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Preparation subtype
                      #:intersect dia-polygon-intersect
                      #:fit-ratio 0.75 1.00
                      (dia-polygon-shape block-key style (geo-hexagon-tile-vertices width height))
                      brief
                      (geo-hexagon-tile-vertices width height))))

(define diaflow-block-terminal : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define r : Nonnegative-Flonum (* height 0.5))
    
    (create-dia-block #:id block-key #:type 'Terminal subtype
                      #:fit-ratio (max (/ (- width r) width) 0.0) 1.00
                      (geo-stadium #:id (dia-block-shape-id block-key)
                                   #:stroke (dia-block-select-stroke-paint style)
                                   #:fill (dia-block-select-fill-paint style)
                                   (- width (* r 2.0)) r)
                      brief)))

(define diaflow-block-input : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (if (eq? subtype 'user)
        (let ([ratio 0.75])
          (create-dia-block #:block dia:block:polygon
                            #:id block-key #:type 'Input subtype
                            #:intersect dia-polygon-intersect
                            #:fit-ratio 1.0 ratio
                            #:position 0.5 (max (- 1.0 (* ratio 0.5)) 0.0)
                            (dia-polygon-shape block-key style (geo-keyboard-vertices width height ratio))
                            brief
                            (geo-keyboard-vertices width height ratio)))
        (diaflow-block-dataIO block-key brief style width height 'Input subtype))))

(define diaflow-block-output : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (if (eq? subtype 'user)
        (let-values ([(ogive barrel r) (values (* width 0.384) (* width 0.618) (* height 0.5))])
          (create-dia-block #:id block-key #:type 'Output subtype
                            #:fit-ratio 0.75 1.00
                            (geo-bullet #:id (dia-block-shape-id block-key)
                                        #:stroke (dia-block-select-stroke-paint style)
                                        #:fill (dia-block-select-fill-paint style)
                                        ogive r barrel)
                            brief))
        (diaflow-block-dataIO block-key brief style width height 'Output subtype))))

(define diaflow-block-inspection : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    
    (create-dia-block #:block dia:block:circle
                      #:id block-key #:type 'Connector (or subtype 'Inspection)
                      #:intersect dia-circle-intersect
                      #:fit-ratio 0.75 0.75
                      (geo-circle #:id (dia-block-shape-id block-key)
                                  #:stroke (dia-block-select-stroke-paint style)
                                  #:fill (dia-block-select-fill-paint style)
                                  r)
                      brief r)))

(define diaflow-block-reference : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define ratio : Nonnegative-Flonum 0.618)
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Connector (or subtype 'Reference)
                      #:intersect dia-polygon-intersect
                      #:fit-ratio 1.00 ratio
                      #:position 0.5 (* ratio 0.5)
                      (dia-polygon-shape block-key style (geo-house-vertices width height (- ratio)))
                      brief
                      (geo-house-vertices width height (- ratio)))))

(define diaflow-block-selection : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    
    (create-dia-block #:block dia:block:circle
                      #:id block-key #:type 'Selection subtype
                      #:intersect dia-circle-intersect
                      #:fit-ratio 0.75 0.75
                      (geo-circle #:id (dia-block-shape-id block-key)
                                  #:stroke (dia-block-select-stroke-paint style)
                                  #:fill (dia-block-select-fill-paint style)
                                  #:diameters (list 0.0 pi/2)
                                  r)
                      #false r)))

(define diaflow-block-junction : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define r : Nonnegative-Flonum (* (min width height) 0.5))
    (create-dia-block #:block dia:block:circle
                      #:id block-key #:type 'Junction subtype
                      #:intersect dia-circle-intersect
                      #:fit-ratio 0.75 0.75
                      (geo-circle #:id (dia-block-shape-id block-key)
                                  #:stroke (dia-block-select-stroke-paint style)
                                  #:fill (dia-block-select-fill-paint style)
                                  #:diameters (list pi/4 3pi/4)
                                  r)
                      #false r)))

(define diaflow-block-manual-operation : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define ratio : Nonnegative-Flonum 0.75)
    (define t-ratio : Nonnegative-Flonum (max (/ 1.0 ratio) 0.0))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Operation subtype
                      #:intersect dia-polygon-intersect
                      #:fit-ratio ratio 1.00
                      (dia-polygon-shape block-key style (geo-isosceles-trapezium-vertices width height t-ratio))
                      brief
                      (geo-isosceles-trapezium-vertices width height t-ratio))))

(define diaflow-block-extract : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Extract subtype
                      #:intersect dia-polygon-intersect
                      (dia-polygon-shape block-key style (geo-isosceles-upwards-triangle-vertices width height))
                      #false
                      (geo-isosceles-upwards-triangle-vertices width height))))

(define diaflow-block-merge : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Merge subtype
                      #:intersect dia-polygon-intersect
                      (dia-polygon-shape block-key style (geo-isosceles-downwards-triangle-vertices width height))
                      #false
                      (geo-isosceles-downwards-triangle-vertices width height))))

(define diaflow-block-delay : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define r : Nonnegative-Flonum (* height 0.5))
    (create-dia-block #:id block-key #:type 'Delay subtype
                      #:fit-ratio (max (/ (- width r) width) 0.0) 1.00
                      (geo-rstadium #:id (dia-block-shape-id block-key)
                                    #:stroke (dia-block-select-stroke-paint style)
                                    #:fill (dia-block-select-fill-paint style)
                                    (- width r) r)
                      brief)))

(define diaflow-block-collation : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (create-dia-block #:id block-key #:type 'Collate subtype
                      #:fit-ratio 0.5 0.375
                      #:position 0.5 0.20
                      (dia-polygon-shape block-key style (geo-poor-hourglass-vertices width height))
                      brief)))

(define diaflow-block-sort : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define h/2 : Nonnegative-Flonum (* height 0.5))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type 'Sort subtype
                      #:intersect dia-polygon-intersect
                      #:fit-ratio 0.5 0.375
                      #:position 0.5 0.30
                      (geo-vc-append #:id (dia-block-shape-id block-key)
                                     (dia-polygon-shape #false style (geo-isosceles-upwards-triangle-vertices width h/2))
                                     (dia-polygon-shape #false style (geo-isosceles-downwards-triangle-vertices width h/2)))
                      brief (geo-rhombus-vertices width height))))

(define diaflow-block-storage : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (cond [(eq? subtype 'Memory) (diaflow-block-memory block-key brief style width height direction subtype)]
          [(eq? subtype 'File) (diaflow-block-document block-key brief style width height direction subtype)]
          [(eq? subtype 'Directory) (diaflow-block-multiple-document block-key brief style width height direction subtype)]
          [(eq? subtype 'Database) (diaflow-block-database block-key brief style width height direction subtype)]
          [else (let ([aradius (* height 0.5 0.384)])
                  (create-dia-block #:id block-key #:type 'Storage subtype
                                    #:fit-ratio (abs (- 1.0 (/ (* aradius 2.0) width))) 1.0
                                    #:position (max (- 0.5 (* (/ aradius width) 0.5)) 0.0) 0.5
                                    (geo-storage #:id (dia-block-shape-id block-key)
                                                 #:stroke (dia-block-select-stroke-paint style)
                                                 #:fill (dia-block-select-fill-paint style)
                                                 width height aradius)
                                    brief))])))

(define diaflow-block-memory : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define line-ratio : Nonnegative-Flonum 0.1618)
    (define brief-pos : Nonnegative-Flonum (min (* width line-ratio) (* height line-ratio)))
    (define brief-zone-width : Flonum  (- width brief-pos))
    (define brief-zone-height : Flonum (- height brief-pos))
    
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-ratio (max (/ brief-zone-width width) 0.0) (max (/ brief-zone-width height) 0.0)
                      #:position (max (/ (+ brief-pos (* brief-zone-width 0.5)) width) 0.0) (max (/ (+ brief-pos (* brief-zone-height 0.5)) height) 0.0)
                      (geo-rectangle #:id (dia-block-shape-id block-key) #:vlines (list brief-pos) #:hlines (list brief-pos)
                                     #:stroke (dia-block-select-stroke-paint style)
                                     #:fill (dia-block-select-fill-paint style)
                                     width height)
                      brief)))

(define diaflow-block-document : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define hratio : Nonnegative-Flonum 0.85)
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-ratio 1.0 hratio
                      #:position 0.5 (* hratio 0.5)
                      (geo-document #:id (dia-block-shape-id block-key)
                                    #:stroke (dia-block-select-stroke-paint style)
                                    #:fill (dia-block-select-fill-paint style)
                                    width height `(,(* (- 1.0 hratio) 0.5) :))
                      brief)))

(define diaflow-block-multiple-document : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define extra-n : Index 2)
    (define gapsize : Nonnegative-Flonum (* height 0.1))
    (define ngap : Nonnegative-Flonum (* gapsize (real->double-flonum extra-n)))
    (define wgr : Flonum (/ ngap width))
    (define hgr : Flonum (/ ngap width))
    (define wave-ratio : Nonnegative-Flonum 0.25)
    (define wratio : Nonnegative-Flonum (abs (- 1.00 wgr)))
    (define hratio : Nonnegative-Flonum (abs (- 1.00 wave-ratio hgr)))
    
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-ratio wratio hratio
                      #:position (abs (- 0.5 (* wgr 0.5))) (abs (+ hgr (* hratio 0.5)))
                      (geo-document #:id (dia-block-shape-id block-key)
                                    #:stroke (dia-block-select-stroke-paint style)
                                    #:fill (dia-block-select-fill-paint style)
                                    #:gapsize gapsize
                                    #:extra-n extra-n
                                    width height `(,(* wave-ratio 0.5) :))
                      brief)))

(define diaflow-block-database : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define extra-n : Index 2)
    (define bradius : Nonnegative-Flonum (* height 0.1618))
    (define gapsize : Nonnegative-Flonum (* bradius 0.618))
    (define brief-zone-height : Flonum (- height (* bradius 3.0) (* gapsize (exact->inexact extra-n))))
    (define hratio : Nonnegative-Flonum (abs (/ brief-zone-height height)))
    
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-ratio 1.0 hratio
                      #:position 0.5 (+ 0.5 (* hratio 0.5))
                      (geo-database #:id (dia-block-shape-id block-key)
                                    #:stroke (dia-block-select-stroke-paint style)
                                    #:fill (dia-block-select-fill-paint style)
                                    #:extra-n extra-n #:gapsize gapsize
                                    width height bradius)
                      brief)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-block-dataIO : (-> Symbol (Option Geo) Dia-Block-Style Nonnegative-Flonum Nonnegative-Flonum Symbol (Option Symbol) Dia:Block)
  (lambda [block-key brief style width height type subtype]
    (define rad : Flonum (/ pi 3.0))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:type type subtype
                      #:intersect dia-polygon-intersect
                      #:fit-ratio (max (- 1.0 (/ (* height (sqrt 3.0) 2/3) width)) 0.0) 1.0
                      (dia-polygon-shape block-key style (geo-parallelogram-vertices width height rad))
                      brief
                      (geo-parallelogram-vertices width height rad))))
