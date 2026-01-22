#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../self.rkt")
(require "../paint.rkt")
(require "../convert.rkt")

(require "../geometry/sides.rkt")

(require "../unsafe/dc/shape.rkt")
(require "../unsafe/dc/border.rkt")
(require "../unsafe/typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:rectangle geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [vlines : (Listof Nonnegative-Flonum)]
   [hlines : (Listof Nonnegative-Flonum)])
  #:type-name Geo:Rectangle
  #:transparent)

(struct geo:rounded-rectangle geo:rectangle
  ([corner-radius : Flonum]
   [corners : (Option (List Boolean Boolean Boolean Boolean))])
  #:type-name Geo:Rounded-Rectangle
  #:transparent)

(struct geo:chamfered-rectangle geo:rectangle
  ([x-chamfer-size : Nonnegative-Flonum]
   [y-chamfer-size : Nonnegative-Flonum]
   [corners : (Option (List Boolean Boolean Boolean Boolean))])
  #:type-name Geo:Chamfered-Rectangle
  #:transparent)

(struct geo:open-rectangle geo:rectangle
  ([vline-span : Nonnegative-Flonum]
   [vline-pos% : Nonnegative-Flonum]
   [hline-span : Nonnegative-Flonum]
   [hline-pos% : Nonnegative-Flonum]
   [open-sides : Geo-Border-Open-Sides])
  #:type-name Geo:Open-Rectangle
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-rectangle
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           [width : Real-Length] [height : Length+% (&% 61.8)]] : Geo:Rectangle
    (define-values (flwidth flheight) (~extent width height))
    
    (create-geometry-object geo:rectangle
                            #:with [id (geo-draw-rectangle stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight
                            (for/list ([v (in-list vlines)]) (~placement v flwidth))
                            (for/list ([h (in-list hlines)]) (~placement h flheight)))))

(define geo-rounded-rectangle
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:exclude-corners [excorners : (Listof Geo-Corner-Choice) null]
           [width : Real-Length] [height : Length+% (&% 61.8)]
           [corner-radius : Length+% 0.0]] : Geo:Rounded-Rectangle
    (define-values (flwidth flheight) (~extent width height))
    (define short-side-len (min flwidth flheight))
    (define cr (~clamp (~distance corner-radius short-side-len) (* short-side-len 0.5)))
    (define round-lt? (not (memq 'lt excorners)))
    (define round-rt? (not (memq 'rt excorners)))
    (define round-lb? (not (memq 'lb excorners)))
    (define round-rb? (not (memq 'rb excorners)))
    
    (create-geometry-object geo:rounded-rectangle
                            #:with [id (geo-draw-rounded-rectangle stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight
                            (for/list ([v (in-list vlines)]) (~placement v flwidth))
                            (for/list ([h (in-list hlines)]) (~placement h flheight))
                            cr
                            (and (or round-lt? round-rt? round-lb? round-rb?)
                                 (list round-lt? round-rt? round-lb? round-rb?)))))

(define geo-chamfered-rectangle
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:exclude-corners [excorners : (Listof Geo-Corner-Choice) null]
           [width : Real-Length] [height : Length+% (&% 61.8)]
           [x-chamfered-size : Length+% 0.0]
           [angle : (Option Real) #false]
           [unit : Angle-Unit 'rad]] : Geo:Chamfered-Rectangle
    (define-values (flwidth flheight) (~extent width height))
    (define short-side-len (min flwidth flheight))
    (define xc-size (~clamp (~dimension x-chamfered-size short-side-len) (* short-side-len 0.5)))
    (define yc-size (if (not angle) xc-size (~clamp (* xc-size (tan (~rad angle unit))) 0.0 (* flheight 0.5))))
    (define chamfer-lt? (not (memq 'lt excorners)))
    (define chamfer-rt? (not (memq 'rt excorners)))
    (define chamfer-lb? (not (memq 'lb excorners)))
    (define chamfer-rb? (not (memq 'rb excorners)))
    
    (create-geometry-object geo:chamfered-rectangle
                            #:with [id (geo-draw-chamfered-rectangle stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight
                            (for/list ([v (in-list vlines)]) (~placement v flwidth))
                            (for/list ([h (in-list hlines)]) (~placement h flheight))
                            xc-size yc-size
                            (and (or chamfer-lt? chamfer-rt? chamfer-lb? chamfer-rb?)
                                 (list chamfer-lt? chamfer-rt? chamfer-lb? chamfer-rb?)))))

(define geo-open-rectangle
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:line-stroke [line-stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:open-sides [open-sides : (U (Listof Geo-Side-Choice) Geo-Open-Sides) null]
           #:hline-span [hspan : Length+% (&% 100)]
           #:hline-pos% [hpos : Real 0.5]
           #:vline-span [vspan : Length+% (&% 100)]
           #:vline-pos% [vpos : Real 0.5]
           [width : Real-Length] [height : Length+% (&% 61.8)]] : Geo:Open-Rectangle
    (define-values (flwidth flheight) (~extent width height))
    (define hls-span (~clamp  (~dimension hspan flwidth) 0.0 flwidth))
    (define vls-span (~clamp (~dimension vspan flheight) 0.0 flheight))
    
    (create-geometry-object geo:open-rectangle
                            #:with [id (geo-draw-open-rectangle stroke line-stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flwidth flheight
                            (for/list ([v (in-list vlines)]) (~placement v flwidth))
                            (for/list ([h (in-list hlines)]) (~placement h flheight))
                            vls-span (~clamp vpos 0.0 1.0)
                            hls-span (~clamp hpos 0.0 1.0)
                            (if (list? open-sides)
                                (list (if (memq 't open-sides) (cons 1.0 0.0) (cons 0.0 0.0))
                                      (if (memq 'r open-sides) (cons 1.0 0.0) (cons 0.0 0.0))
                                      (if (memq 'b open-sides) (cons 1.0 0.0) (cons 0.0 0.0))
                                      (if (memq 'l open-sides) (cons 1.0 0.0) (cons 0.0 0.0)))
                                (geo-open-sides->border-sides open-sides flwidth flheight)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-square
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           [size : Real-Length]] : Geo:Rectangle
    (geo-rectangle #:id id #:stroke stroke #:fill pattern
                   #:vlines vlines #:hlines hlines
                   size size)))

(define geo-rounded-square
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:exclude-corners [excorners : (Listof Geo-Corner-Choice) null]
           [size : Real-Length] [corner-radius : Length+% 0.0]] : Geo:Rounded-Rectangle
    (geo-rounded-rectangle #:id id #:stroke stroke #:fill pattern
                           #:vlines vlines #:hlines hlines
                           #:exclude-corners excorners
                           size size corner-radius)))

(define geo-chamfered-square
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:exclude-corners [excorners : (Listof Geo-Corner-Choice) null]
           [side-size : Real-Length]
           [x-chamfered-size : Length+% 0.0]
           [angle : (Option Real) #false]
           [unit : Angle-Unit 'rad]] : Geo:Chamfered-Rectangle
    (geo-chamfered-rectangle #:id id #:stroke stroke #:fill pattern
                             #:vlines vlines #:hlines hlines
                             #:exclude-corners excorners
                             side-size side-size x-chamfered-size angle unit)))

(define geo-open-square
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:line-stroke [line-stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:open-sides [open-sides : (U (Listof Geo-Side-Choice) Geo-Open-Sides) null]
           #:hline-span [hspan : Length+% (&% 100)]
           #:hline-pos% [hpos : Real 0.5]
           #:vline-span [vspan : Length+% (&% 100)]
           #:vline-pos% [vpos : Real 0.5]
           [size : Real-Length]] : Geo:Open-Rectangle
    (geo-open-rectangle #:id id #:stroke stroke #:line-stroke line-stroke #:fill pattern
                        #:vlines vlines #:hlines hlines #:open-sides open-sides
                        #:hline-span hspan #:hline-pos% hpos
                        #:vline-span vspan #:vline-pos% vpos
                        size size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-rectangle : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:rectangle? self)
        (dc_rectangle cr x0 y0 width height
                      (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                      (geo:rectangle-vlines self) (geo:rectangle-hlines self))))))

(define geo-draw-rounded-rectangle : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:rounded-rectangle? self)
        (define cradius (geo:rounded-rectangle-corner-radius self))
        (define corners (geo:rounded-rectangle-corners self))
        (define vls (geo:rectangle-vlines self))
        (define hls (geo:rectangle-hlines self))
        (define pen (geo-select-stroke-paint alt-stroke))
        (define brush (geo-select-fill-source alt-fill))
        
        (cond [(or (zero? cradius) (not corners)) (dc_rectangle cr x0 y0 width height pen brush vls hls)]
              [(> cradius 0.0) (dc_convex_rounded_rectangle cr x0 y0 width height cradius pen brush vls hls corners)]
              [(< cradius 0.0) (dc_concave_rounded_rectangle cr x0 y0 width height (abs cradius) pen brush vls hls corners)]
              [else (dc_rectangle cr x0 y0 width height pen brush vls hls)])))))

(define geo-draw-chamfered-rectangle : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:chamfered-rectangle? self)
        (define xcsize (geo:chamfered-rectangle-x-chamfer-size self))
        (define ycsize (geo:chamfered-rectangle-y-chamfer-size self))
        (define corners (geo:chamfered-rectangle-corners self))
        (define vls (geo:rectangle-vlines self))
        (define hls (geo:rectangle-hlines self))
        (define pen (geo-select-stroke-paint alt-stroke))
        (define brush (geo-select-fill-source alt-fill))
        
        (cond [(not corners) (dc_rectangle cr x0 y0 width height pen brush vls hls)]
              [(and (> xcsize 0.0) (> ycsize 0.0)) (dc_chamfered_rectangle cr x0 y0 width height xcsize ycsize pen brush vls hls corners)]
              [else (dc_rectangle cr x0 y0 width height pen brush vls hls)])))))

(define geo-draw-open-rectangle : (-> Maybe-Stroke-Paint Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke line-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:open-rectangle? self)
        (define sides (geo:open-rectangle-open-sides self))
        (define border-pen (geo-select-stroke-paint alt-stroke))
        
        (dc_open_rectangle cr x0 y0 width height
                           border-pen (geo-select-stroke-paint line-stroke border-pen) (geo-select-fill-source alt-fill)
                           (geo:rectangle-vlines self) (geo:open-rectangle-vline-span self) (geo:open-rectangle-vline-pos% self)
                           (geo:rectangle-hlines self) (geo:open-rectangle-hline-span self) (geo:open-rectangle-hline-pos% self)
                           (geo:open-rectangle-open-sides self))))))
