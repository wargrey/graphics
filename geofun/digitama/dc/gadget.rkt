#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../self.rkt")
(require "../convert.rkt")

(require "../paint.rkt")
(require "../unsafe/dc/shape.rkt")
(require "../unsafe/dc/gadget.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:gear geo
  ([teeth-number : Index]
   [pressure-angle : Flonum]
   [radius : Nonnegative-Flonum]
   [root-radius : Nonnegative-Flonum]
   [reference-ratio : Nonnegative-Flonum]
   [inner-radius : Nonnegative-Flonum]
   [rotation : Flonum])
  #:type-name Geo:Gear
  #:transparent)

(struct geo:sandglass geo
  ([neck-width : Nonnegative-Flonum]
   [neck-height : Nonnegative-Flonum]
   [tube-height : Nonnegative-Flonum])
  #:type-name Geo:Sandglass
  #:transparent)

(struct geo:bullet geo
  ([ogive-length : Nonnegative-Flonum]
   [barrel-length : Nonnegative-Flonum]
   [radius : Nonnegative-Flonum])
  #:type-name Geo:Bullet
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-gear
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false]
           #:pressure-angle [angle : Real (/ pi 9.0)]
           #:root-radius [root-radius : Length+% (&% 81)]
           #:inner-radius [inner-radius : Length+% (&% 61.8)]
           #:reference-ratio [ref-ratio : Nonnegative-Real 5/9]
           [n : Integer] [tip-radius : Real-Length]
           [rotation : Real 0.0] [unit : Angle-Unit 'rad]] : Geo:Gear
    (define Rt : Nonnegative-Flonum (~dimension tip-radius))
    (define Rr : Nonnegative-Flonum (~dimension root-radius Rt))
    (define Ri : Nonnegative-Flonum (~dimension inner-radius Rt))
    (define α : Flonum (~rad angle unit))
    (define z : Index (if (index? n) n 0))
    (define d : Nonnegative-Flonum (* 2.0 Rt))
    
    (create-geometry-object geo:gear
                            #:with [id (geo-draw-gear stroke pattern)
                                       (geo-shape-extent d d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            z α Rt Rr (~clamp ref-ratio 0.0 1.0)
                            Ri (real->double-flonum rotation))))

(define geo-sandglass : (->* (Real)
                             (Length+% #:id (Option Symbol) #:neck-width Length+% #:neck-height Length+% #:tube-height Length+%
                                     #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                             Geo:Sandglass)
  (lambda [#:neck-width [neck-width (&% 16.18)] #:neck-height [neck-height (&% 6.18)] #:tube-height [tube-height 0]
           #:stroke [stroke (void)] #:fill [pattern (void)] #:id [id #false]
           width [height (&% 161.8)]]
    (define-values (flwidth flheight) (~extent width height))
    (define neck-flwidth (~dimension neck-width flwidth))
    (define neck-flheight (~dimension neck-height flheight))
    (define tube-flheight (~dimension tube-height flheight))
    
    (create-geometry-object geo:sandglass
                            #:with [id (geo-draw-sandglass stroke pattern)
                                       (geo-shape-extent flwidth flheight 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            neck-flwidth neck-flheight tube-flheight)))

(define geo-bullet : (->* (Real-Length Length+%)
                          (Length+% #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                          Geo:Bullet)
  (lambda [ogive radius [barrel (&% 38.4)] #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define-values (flogive flbarrel) (~extent ogive barrel))
    (define flradius : Nonnegative-Flonum (~dimension radius (+ flogive flbarrel)))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:bullet
                            #:with [id (geo-draw-bullet stroke pattern)
                                       (geo-shape-extent (+ flogive flbarrel) d 0.0 0.0)
                                       (geo-shape-outline stroke)]
                            flogive flbarrel flradius)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-gear : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:gear? self)
        (define z (geo:gear-teeth-number self))
        (define Rt (geo:gear-radius self))
        (define Rr (/ (geo:gear-root-radius self) Rt))
        (define Ri (/ (geo:gear-inner-radius self) Rt))
        
        (if (and (> z 0) (< Ri Rr 1.0) (> Rr 0.0) (>= Ri 0.0))
            (dc_gear cr x0 y0 width height z (geo:gear-pressure-angle self) Rr (geo:gear-reference-ratio self)
                     Ri (geo:gear-rotation self) (geo-select-stroke-paint alt-stroke)
                     (geo-select-fill-source alt-fill))
            (dc_ellipse cr x0 y0 width height
                        (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                        null))))))

(define geo-draw-sandglass : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:sandglass? self)
        (dc_sandglass cr x0 y0 width height
                      (geo:sandglass-neck-width self) (geo:sandglass-neck-height self) (geo:sandglass-tube-height self)
                      (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))

(define geo-draw-bullet : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:bullet? self)
        (dc_bullet cr x0 y0 width height (geo:bullet-ogive-length self)
                   (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill))))))
