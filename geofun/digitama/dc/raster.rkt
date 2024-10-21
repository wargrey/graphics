#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require bitmap/digitama/convert)
(require bitmap/digitama/unsafe/image)

(require "../base.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/plain.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:bitmap geo
  ([source : (U Bitmap XYWH->ARGB)])
  #:type-name Geo:Bitmap
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bitmap : (-> Bitmap [#:id (Option Symbol)] Geo:Bitmap)
  (lambda [self #:id [id #false]]
    (define-values (w h) (bitmap-flsize self))
    (create-geometry-object geo:bitmap (geo-draw-bitmap w h)
                            #:extent (geo-shape-plain-extent w h)
                            #:id id
                            self)))

(define geo-rectangular : (-> Real Real XYWH->ARGB [#:id (Option Symbol)] Geo:Bitmap)
  (lambda [width height λargb #:id [id #false]]
    (define-values (w h) (~size width height))
    (create-geometry-object geo:bitmap (geo-draw-bitmap w h)
                            #:extent (geo-shape-plain-extent w h)
                            #:id id
                            λargb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-draw-bitmap : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Draw!)
  (lambda [flwidth flheight]
    (λ [self cr x0 y0 width height]
      (when (geo:bitmap? self)
        (define src (geo:bitmap-source self))
        
        (if (bitmap? src)
            (dc_image cr x0 y0 width height (bitmap-surface src))

            (let ([bmp (λbitmap flwidth flheight (default-bitmap-density) src)])
              (dc_image cr x0 y0 width height (bitmap-surface bmp))))))))
