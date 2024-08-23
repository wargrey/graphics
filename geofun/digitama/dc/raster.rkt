#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)
(require bitmap/digitama/convert)
(require bitmap/digitama/unsafe/image)

(require "../base.rkt")

(require "../convert.rkt")
(require "../unsafe/visual/abstract.rkt")
(require "../unsafe/visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:bitmap geo
  ([soul : Bitmap])
  #:type-name Geo:Bitmap
  #:transparent)

(struct geo:λbitmap geo
  ([build : XYWH->ARGB])
  #:type-name Geo:λBitmap
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bitmap : (-> Bitmap [#:id (Option Symbol)] Geo:Bitmap)
  (lambda [self #:id [id #false]]
    (define-values (w h) (bitmap-flsize self))
    (create-geometry-object geo:bitmap
                            #:surface (geo-bitmap-surface w h)
                            #:bbox (geo-shape-plain-bbox w h)
                            #:id id
                            self)))

(define geo-rectangular : (-> Real Real XYWH->ARGB [#:id (Option Symbol)] Geo:λBitmap)
  (lambda [width height λargb #:id [id #false]]
    (define-values (w h) (~size width height))
    (create-geometry-object geo:λbitmap
                            #:surface (geo-plain-argb-surface w h)
                            #:bbox (geo-shape-plain-bbox w h)
                            #:id id
                            λargb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-bitmap-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Create)
  (lambda [flwidth flheight]
    (λ [self]
      (with-asserts ([self geo:bitmap?])
        (make-abstract-surface-from-bitmap
         flwidth flheight (geo:bitmap-soul self)
         (default-geometry-density))))))

(define geo-plain-argb-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Create)
  (lambda [flwidth flheight]
    (λ [self]
      (with-asserts ([self geo:λbitmap?])
        (make-abstract-surface-from-bitmap
         flwidth flheight
         (λbitmap flwidth flheight (default-bitmap-density)
                  (geo:λbitmap-build self))
         (default-geometry-density))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-abstract-surface-from-bitmap : (-> Nonnegative-Flonum Nonnegative-Flonum Bitmap Positive-Flonum Abstract-Surface)
  (lambda [flwidth flheight bmp density]
    (create-abstract-surface-from-image-surface
     flwidth flheight (bitmap-surface bmp) density)))
