#lang racket/base

(provide (all-defined-out))

(require geofun/digitama/unsafe/surface/image)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define create-argb-bitmap
  (lambda [width height density scale?]
    (define-values (sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?))
    (values (make-bitmap-from-image-surface sfc density fxwidth fxheight) cr)))

(define create-invalid-bitmap
  (lambda [width height density scale?]
    (define-values (sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?))
    (values (make-bitmap-from-image-surface sfc density fxwidth fxheight
                                            invalid-convert (string->uninterned-symbol "/dev/zero"))
            cr)))
