#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require bitmap/digitama/self)
(require bitmap/digitama/unsafe/image)

(require "../base.rkt")
(require "../self.rkt")
(require "../convert.rkt")

(require "../paint/pattern.rkt")
(require "../unsafe/dc/plain.rkt")

(require "../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:bitmap geo
  ([source : (U Bitmap XYWH->ARGB)]
   [filter : Geo-Pattern-Filter])
  #:type-name Geo:Bitmap
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bitmap
  (lambda [#:id [id : (Option Symbol) #false]
           #:filter [filter : Geo-Pattern-Filter (default-pattern-filter)]
           [self : Bitmap]] : Geo:Bitmap
    (define-values (w h) (bitmap-flsize self))
    (create-geometry-object geo:bitmap
                            #:with [id (geo-draw-bitmap w h) (geo-shape-extent w h) geo-zero-pads]
                            self filter)))

(define geo-rectangular
  (lambda [#:id [id : (Option Symbol) #false]
           #:filter [filter : Geo-Pattern-Filter (default-pattern-filter)]
           [width : Real] [height : Real+%] [λargb : XYWH->ARGB]] : Geo:Bitmap
    (define-values (w h) (~extent width height))
    (create-geometry-object geo:bitmap
                            #:with [id (geo-draw-bitmap w h) (geo-shape-extent w h) geo-zero-pads]
                            λargb filter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-bitmap : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Draw!)
  (lambda [flwidth flheight]
    (λ [self cr x0 y0 width height]
      (when (geo:bitmap? self)
        (define src (geo:bitmap-source self))
        (define filter (geo-pattern-filter->integer (geo:bitmap-filter self)))
        
        (if (bitmap? src)
            (dc_image cr x0 y0 width height filter (bitmap-surface src))

            (let ([bmp (λbitmap flwidth flheight (default-bitmap-density) src)])
              (dc_image cr x0 y0 width height filter (bitmap-surface bmp))))))))
