#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/unsafe/surface/image)
(require geofun/digitama/unsafe/typed/c)
(require geofun/digitama/unsafe/visual)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap_invalid : (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Bitmap)
  (lambda [flwidth flheight density]
    (define-values (img-sfc fxwidth fxheight) (cairo-create-argb-image-surface flwidth flheight density))
    
    (bitmap invalid-convert (cairo-image-shadow-size img-sfc)
            img-sfc (string->uninterned-symbol "/dev/zero")
            density
            fxwidth fxheight 4 8)))

(define bitmap-invalid? : (-> Bitmap Boolean)
  (lambda [bmp]
    (let ([src (bitmap-source bmp)])
      (not (or (symbol-interned? src)
               (symbol-unreadable? src))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define invalid-convert : Visual-Object-Convert
  (lambda [self mime fallback]
    #""))
