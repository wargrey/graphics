#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [bitmap-sandglass bitmap-hourglass]))

(require digimon/metrics)
(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/source)
(require geofun/digitama/unsafe/dc/more)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-stadium : (-> Real Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] Bitmap)
  (lambda [length radius #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (fllength flradius) (~size length radius))
    (define h : Nonnegative-Flonum (* flradius 2.0))
    (define w : Nonnegative-Flonum (+ fllength h))
    
    (draw-bitmap dc_stadium #:with [w h density #true (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern)])))

(define bitmap-lstadium : (-> Real Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] Bitmap)
  (lambda [length radius #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (fllength flradius) (~size length radius))
    (define w : Nonnegative-Flonum (+ fllength flradius))
    (define h : Nonnegative-Flonum (* flradius 2.0))
    
    (draw-bitmap dc_half_stadium #:with [w h density #true (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern) #true])))

(define bitmap-rstadium : (-> Real Real [#:stroke Maybe-Stroke-Paint] [#:fill Option-Fill-Paint] [#:density Positive-Flonum] Bitmap)
  (lambda [length radius #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]]
    (define-values (fllength flradius) (~size length radius))
    (define w : Nonnegative-Flonum (+ fllength flradius))
    (define h : Nonnegative-Flonum (* flradius 2.0))
    
    (draw-bitmap dc_half_stadium #:with [w h density #true (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern) #false])))

(define bitmap-bullet : (->* (Real Real) (Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum) Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]
           ogive radius [barrel -0.1618]]
    (define-values (flogive flbarrel) (~size ogive barrel))
    (define w : Nonnegative-Flonum (+ flogive flbarrel))
    (define h : Nonnegative-Flonum (* (~length radius w) 2.0))
    
    (draw-bitmap dc_bullet #:with [w h density #true (stroke-paint->source* outline)]
                 [flogive] [(fill-paint->source* pattern)])))

(define bitmap-sandglass : (->* (Real)
                                (Real #:neck-width Real #:neck-height Real #:tube-height Real
                                      #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum)
                                Bitmap)
  (lambda [#:neck-width [neck-width -0.1618] #:neck-height [neck-height -0.0618] #:tube-height [tube-height 0]
           #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:density [density (default-bitmap-density)]
           width [height -1.618]]
    (define-values (w h) (~size width height))
    (define neck-flwidth (~length neck-width w))
    (define neck-flheight (~length neck-height h))
    (define tube-flheight (~length tube-height h))
    
    (draw-bitmap dc_sandglass #:with [w h density #true (stroke-paint->source* outline)]
                 [neck-flwidth neck-flheight tube-flheight] [(fill-paint->source* pattern)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-document : (->* (Real Real)
                               (Real #:extra-n Index #:gapsize Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum)
                               Bitmap)
  (lambda [#:extra-n [extra-n 0] #:gapsize [gapsize -0.384]
           #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:density [density (default-bitmap-density)]
           width height [wave-height -0.1618]]
    (define-values (w h) (~size width height))
    (define flwave (~length wave-height h))
    
    (draw-bitmap dc_document #:with [w h density #true (stroke-paint->source* outline)]
                 [flwave (~length gapsize flwave) extra-n] [(fill-paint->source* pattern)])))

(define bitmap-database : (->* (Real Real)
                               (Real #:extra-n Index #:gapsize Real #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:density Positive-Flonum)
                               Bitmap)
  (lambda [#:extra-n [extra-n 2] #:gapsize [gapsize -0.618]
           #:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)]
           #:density [density (default-bitmap-density)]
           width height [bradius -0.1618]]
    (define-values (w h) (~size width height))
    (define flb (~length bradius h))
    
    (draw-bitmap dc_database #:with [w h density #true (stroke-paint->source* outline)]
                 [flb (~length gapsize flb) extra-n] [(fill-paint->source* pattern)])))
