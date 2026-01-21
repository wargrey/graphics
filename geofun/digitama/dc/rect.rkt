#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../self.rkt")
(require "../convert.rkt")

(require "../paint.rkt")
(require "../unsafe/dc/shape.rkt")
(require "../unsafe/typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:rectangle geo
  ([width : Nonnegative-Flonum]
   [height : Nonnegative-Flonum]
   [vlines : (Listof Flonum)]
   [hlines : (Listof Flonum)])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-square
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           [size : Real-Length]] : Geo:Rectangle
    (define side-len (~dimension size))
    
    (create-geometry-object geo:rectangle
                            #:with [id (geo-draw-rectangle stroke pattern)
                                       (geo-shape-extent side-len 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            side-len side-len
                            (for/list ([v (in-list vlines)]) (~placement v side-len))
                            (for/list ([h (in-list hlines)]) (~placement h side-len)))))

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

(define geo-rounded-square
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:exclude-corners [excorners : (Listof Geo-Corner-Position) null]
           [size : Real-Length] [corner-radius : Length+% 0.0]] : Geo:Rounded-Rectangle
    (define side-len (~dimension size))
    (define cr (~clamp (~distance corner-radius side-len) (* side-len 0.5)))
    (define round-lt? (not (memq 'lt excorners)))
    (define round-rt? (not (memq 'rt excorners)))
    (define round-lb? (not (memq 'lb excorners)))
    (define round-rb? (not (memq 'rb excorners)))
    
    (create-geometry-object geo:rounded-rectangle
                            #:with [id (geo-draw-rounded-rectangle stroke pattern)
                                       (geo-shape-extent side-len side-len 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            side-len side-len
                            (for/list ([v (in-list vlines)]) (~placement v side-len))
                            (for/list ([h (in-list hlines)]) (~placement h side-len))
                            cr
                            (and (or round-lt? round-rt? round-lb? round-rb?)
                                 (list round-lt? round-rt? round-lb? round-rb?)))))

(define geo-rounded-rectangle
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:exclude-corners [excorners : (Listof Geo-Corner-Position) null]
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

(define geo-chamfered-square
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:exclude-corners [excorners : (Listof Geo-Corner-Position) null]
           [edge-size : Real-Length]
           [x-chamfered-size : Length+% 0.0]
           [angle : (Option Real) #false]
           [unit : Angle-Unit 'rad]] : Geo:Chamfered-Rectangle
    (define side-len (~dimension edge-size))
    (define xc-size (~clamp (~dimension x-chamfered-size side-len) (* side-len 0.5)))
    (define yc-size (if (not angle) xc-size (~clamp (* xc-size (tan (~rad angle unit))) 0.0 (* side-len 0.5))))
    (define chamfer-lt? (not (memq 'lt excorners)))
    (define chamfer-rt? (not (memq 'rt excorners)))
    (define chamfer-lb? (not (memq 'lb excorners)))
    (define chamfer-rb? (not (memq 'rb excorners)))
    
    (create-geometry-object geo:chamfered-rectangle
                            #:with [id (geo-draw-chamfered-rectangle stroke pattern)
                                       (geo-shape-extent side-len side-len 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            side-len side-len
                            (for/list ([v (in-list vlines)]) (~placement v side-len))
                            (for/list ([h (in-list hlines)]) (~placement h side-len))
                            xc-size yc-size
                            (and (or chamfer-lt? chamfer-rt? chamfer-lb? chamfer-rb?)
                                 (list chamfer-lt? chamfer-rt? chamfer-lb? chamfer-rb?)))))

(define geo-chamfered-rectangle
  (lambda [#:id [id : (Option Symbol) #false]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:vlines [vlines : (Listof Length+%) null]
           #:hlines [hlines : (Listof Length+%) null]
           #:exclude-corners [excorners : (Listof Geo-Corner-Position) null]
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
