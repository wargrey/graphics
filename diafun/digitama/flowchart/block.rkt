#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../block/dc.rkt")
(require "../block/style.rkt")
(require "../block/interface.rkt")

(require "../block/dc/node.rkt")
(require "../block/dc/symbol.rkt")

(require geofun/constructor)
(require geofun/composite)

(require geofun/digitama/geometry/constants)
(require geofun/digitama/geometry/polygon/triangle)
(require geofun/digitama/geometry/polygon/quadrilateral)
(require geofun/digitama/geometry/polygon/pentagon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) flow-block-process : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (cond [(eq? subtype 'Predefined) (flow-block-prefab block-key caption style width height direction subtype)]
          [(eq? subtype 'Alternate)  (flow-block-alternate block-key caption style width height direction subtype)]
          [else (dia-block-rectangle block-key caption style width height direction 'Process subtype)])))

(define #:forall (S) flow-block-prefab : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (define width% : Nonnegative-Flonum 0.84)
    (define vline : Flonum (* (- 1.0 width%) 0.5 width))
    
    (create-dia-block #:id block-key #:tag 'Process subtype
                      #:fit-region width% 1.0
                      #:create-with style [geo-rectangle #:vlines (list vline (- vline)) width height]
                      caption)))

(define #:forall (S) flow-block-alternate : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-rectangle/cr:4th block-key caption style width height direction 'Alternate subtype)))

(define #:forall (S) flow-block-decision : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-diamond block-key caption style width height direction 'Decision subtype)))

(define #:forall (S) flow-block-preparation : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-hexagon block-key caption style width height direction 'Preparation subtype)))

(define #:forall (S) flow-block-terminal : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (define r : Nonnegative-Flonum (* height 0.5))
    
    (create-dia-block #:id block-key #:tag 'Terminal subtype
                      #:fit-region (max (/ (- width r) width) 0.0) 1.00
                      #:create-with style [geo-stadium (- width (* r 2.0)) r]
                      caption)))

(define #:forall (S) flow-block-input : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (if (eq? subtype 'user)
        (let ([ratio 0.80])
          (dia-block-polygon block-key caption style direction
                             (geo-keyboard-vertices width height ratio)
                             1.0 ratio 0.0 (- 1.0 ratio)
                             'Input subtype))
        (dia-block-parallelogram block-key caption style width height direction 'Input subtype))))

(define #:forall (S) flow-block-output : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (if (eq? subtype 'user)
        (let-values ([(ogive barrel r) (values (* width 0.384) (* width 0.618) (* height 0.5))])
          (create-dia-block #:id block-key #:tag 'Output subtype
                            #:fit-region 0.75 1.00 (/ (- ogive r) width) 0.0
                            #:create-with style [geo-bullet ogive r barrel]
                            caption))
        (dia-block-parallelogram block-key caption style width height direction 'Output subtype))))

(define #:forall (S) flow-block-inspection : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-circle block-key caption style width height direction 'Connector (or subtype 'Inspection))))

(define #:forall (S) flow-block-reference : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignored-width height direction subtype]
    (define ratio : Nonnegative-Flonum 0.618)
    (define width (* height 0.618 1.618))
    
    (dia-block-polygon block-key caption style direction
                       (geo-house-vertices width height (- ratio))
                       1.00 ratio 0.0 0.0
                       'Connector (or subtype 'Reference))))

(define #:forall (S) flow-block-selection : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-symbol-circle block-key (list 0.0 pi/2)
                       style width height direction
                       'Selection subtype)))

(define #:forall (S) flow-block-junction : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-symbol-circle block-key (list pi/4 3pi/4) style
                       width height direction
                       'Junction subtype)))

(define #:forall (S) flow-block-manual-operation : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (dia-block-symmetric-polygon block-key caption style direction
                                 (geo-isosceles-trapezium-vertices width height (abs (/ 4.0 3.0)))
                                 0.81 1.00
                                 'Operation subtype)))

(define #:forall (S) flow-block-extract : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignored-width height direction subtype]
    (dia-symbol-regular-triangle block-key style height direction 'apex@top 'Extract subtype)))

(define #:forall (S) flow-block-merge : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignored-width height direction subtype]
    (dia-symbol-regular-triangle block-key style height direction 'apex@bot 'Merge subtype)))

(define #:forall (S) flow-block-delay : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (define r : Nonnegative-Flonum (* height 0.5))
    
    (create-dia-block #:id block-key #:tag 'Delay subtype
                      #:fit-region (- 1.0 (/ r width)) 1.00 (* (/ r width) 0.25) 0.0
                      #:create-with style [geo-rstadium (- width r) r]
                      caption)))

(define #:forall (S) flow-block-collation : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignored-width height direction subtype]
    (dia-block-polygon block-key caption style direction
                       (geo-poor-hourglass-vertices (* height 1.618) height) 0.5 0.375 0.25 0.0
                       'Collate subtype)))

(define #:forall (S) flow-block-sort : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignored-width height direction subtype]
    (define width (* height 1.618))
    (define h/2 : Nonnegative-Flonum (* height 0.5))
    
    (create-dia-block #:block dia:block:polygon
                      #:id block-key #:tag 'Sort subtype
                      #:intersect dia-polygon-intersect
                      #:fit-region 0.5 0.375 0.25 0.125
                      #:with style (geo-vc-append #:id (dia-block-shape-id block-key)
                                                  (dia-polygon-shape #false style (geo-isosceles-triangle-vertices/apex@top width h/2))
                                                  (dia-polygon-shape #false style (geo-isosceles-triangle-vertices/apex@bot width h/2)))
                      caption (geo-rhombus-vertices width height))))

(define #:forall (S) flow-block-storage : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (cond [(eq? subtype 'Memory) (flow-block-memory block-key caption style width height direction subtype)]
          [(eq? subtype 'File) (flow-block-document block-key caption style width height direction subtype)]
          [(eq? subtype 'Directory) (flow-block-multiple-document block-key caption style width height direction subtype)]
          [(eq? subtype 'Database) (flow-block-database block-key caption style width height direction subtype)]
          [else (let* ([aradius (* height 0.5 0.384)]
                       [ar% (/ aradius width)])
                  (create-dia-block #:id block-key #:tag 'Storage subtype
                                    #:fit-region (- 1.0 ar% ar%) 1.0 (* ar% 0.5) 0.0
                                    #:create-with style [geo-storage width height aradius]
                                    caption))])))

(define #:forall (S) flow-block-memory : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (define line% : Nonnegative-Flonum 0.1618)
    (define caption-pos : Nonnegative-Flonum (min (* width line%) (* height line%)))
    (define left% : Flonum (/ caption-pos width))
    (define top% : Flonum (/ caption-pos height))
    
    (create-dia-block #:id block-key #:tag 'Storage subtype
                      #:fit-region (- 1.0 left%) (- 1.0 top%) left% top%
                      #:create-with style [geo-rectangle #:vlines (list caption-pos) #:hlines (list caption-pos) width height]
                      caption)))

(define #:forall (S) flow-block-document : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (define hratio : Nonnegative-Flonum 0.85)
    (create-dia-block #:id block-key #:tag 'Storage subtype
                      #:fit-region 1.0 hratio 0.0 0.0
                      #:alignment 0.0 0.0 0.0 0.0
                      #:create-with style [geo-document width height (&: (* (- 1.0 hratio) 0.5))]
                      caption)))

(define #:forall (S) flow-block-multiple-document : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style width height direction subtype]
    (define extra-n : Index 2)
    (define gapsize : Nonnegative-Flonum (* height 0.1))
    (define ngap : Nonnegative-Flonum (* gapsize (real->double-flonum extra-n)))
    (define offset% : Flonum (/ ngap width))
    (define wave% : Nonnegative-Flonum 0.25)
    
    (create-dia-block #:id block-key #:tag 'Storage subtype
                      #:fit-region (- 1.00 offset%) (- 1.00 wave% offset%) 0.0 offset%
                      #:create-with style [geo-document #:gapsize gapsize #:extra-n extra-n width height (&: (* wave% 0.5))]
                      caption)))

(define #:forall (S) flow-block-database : (Dia-Block-Create S (Option Symbol))
  (lambda [block-key caption style ignored-width height direction subtype]
    (define extra-n : Index 2)
    (define bradius : Nonnegative-Flonum (* height 0.1618))
    (define gapsize : Nonnegative-Flonum (* bradius 0.618))
    (define caption-pos : Flonum (/ (+ (* bradius 2.0) (* gapsize (exact->inexact extra-n))) height))
    (define width (* height 1.618))
    
    (create-dia-block #:id block-key #:tag 'Storage subtype
                      #:fit-region 1.0 (- 1.0 caption-pos) 0.0 caption-pos
                      #:create-with style [geo-database #:extra-n extra-n #:gapsize gapsize width height bradius]
                      caption)))
