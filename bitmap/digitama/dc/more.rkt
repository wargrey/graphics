#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [bitmap-sandglass bitmap-hourglass]))

(require digimon/metrics)
(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/paint/source)
(require geofun/digitama/unsafe/dc/more)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-stadium
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)]
           #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [length : Real] [radius : Real+%]] : Bitmap
    (define-values (fllength flradius) (~extent length radius))
    (define h : Nonnegative-Flonum (* flradius 2.0))
    (define w : Nonnegative-Flonum (+ fllength h))
    
    (draw-bitmap dc_stadium #:with [w h density #true (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern)])))

(define bitmap-lstadium
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)]
           #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [length : Real] [radius : Real+%]] : Bitmap
    (define-values (fllength flradius) (~extent length radius))
    (define w : Nonnegative-Flonum (+ fllength flradius))
    (define h : Nonnegative-Flonum (* flradius 2.0))
    
    (draw-bitmap dc_half_stadium #:with [w h density #true (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern) #true])))

(define bitmap-rstadium
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)]
           #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [length : Real] [radius : Real+%]] : Bitmap
    (define-values (fllength flradius) (~extent length radius))
    (define w : Nonnegative-Flonum (+ fllength flradius))
    (define h : Nonnegative-Flonum (* flradius 2.0))
    
    (draw-bitmap dc_half_stadium #:with [w h density #true (stroke-paint->source* outline)]
                 [] [(fill-paint->source* pattern) #false])))

(define bitmap-bullet
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)]
           #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [ogive : Real] [radius : Real+%] [barrel : Real+% '(16.18 %)]] : Bitmap
    (define-values (flogive flbarrel) (~extent ogive barrel))
    (define w : Nonnegative-Flonum (+ flogive flbarrel))
    (define h : Nonnegative-Flonum (* (~length radius w) 2.0))
    
    (draw-bitmap dc_bullet #:with [w h density #true (stroke-paint->source* outline)]
                 [flogive] [(fill-paint->source* pattern)])))

(define bitmap-sandglass
  (lambda [#:neck-width [neck-width : Real+% '(16.18 %)]
           #:neck-height [neck-height : Real+% '(6.18 %)]
           #:tube-height [tube-height : Real+% 0.0]
           #:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)]
           #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real+% '(161.8 %)]] : Bitmap
    (define-values (w h) (~extent width height))
    (define neck-flwidth (~length neck-width w))
    (define neck-flheight (~length neck-height h))
    (define tube-flheight (~length tube-height h))
    
    (draw-bitmap dc_sandglass #:with [w h density #true (stroke-paint->source* outline)]
                 [neck-flwidth neck-flheight tube-flheight] [(fill-paint->source* pattern)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-document
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)]
           #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:extra-n [extra-n : Index 0]
           #:gapsize [gapsize : Real+% '(38.4 %)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real+%] [wave-height : Real+% '(16.18 %)]] : Bitmap
    (define-values (w h) (~extent width height))
    (define flwave (~length wave-height h))
    
    (draw-bitmap dc_document #:with [w h density #true (stroke-paint->source* outline)]
                 [flwave (~length gapsize flwave) extra-n] [(fill-paint->source* pattern)])))

(define bitmap-database
  (lambda [#:extra-n [extra-n : Index 0]
           #:gapsize [gapsize : Real+% '(61.8 %)]
           #:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)]
           #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [width : Real] [height : Real+%] [bradius : Real+% '(16.18 %)]] : Bitmap
    (define-values (w h) (~extent width height))
    (define flb (~length bradius h))
    
    (draw-bitmap dc_database #:with [w h density #true (stroke-paint->source* outline)]
                 [flb (~length gapsize flb) extra-n] [(fill-paint->source* pattern)])))
