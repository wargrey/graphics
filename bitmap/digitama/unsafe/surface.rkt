#lang racket/base

(provide (all-defined-out))

(require pangocairo/digitama/unsafe/pangocairo)
(require pangocairo/digitama/unsafe/surface/image)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define create-argb-bitmap
  (case-lambda
    [(width height density scale?)
     (let-values ([(sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?)])
       (values (make-bitmap-from-image-surface sfc density fxwidth fxheight) cr))]
    [(width height background density scale?)
     (let-values ([(img cr) (create-argb-bitmap width height density scale?)])
       (unless (not background)
         (cairo-set-source cr background)
         (cairo_paint cr))
       (values img cr))]))

(define create-invalid-bitmap
  (lambda [width height density scale?]
    (define-values (sfc cr fxwidth fxheight) (cairo-create-argb-image-surface* width height density scale?))
    (values (make-bitmap-from-image-surface sfc density fxwidth fxheight
                                            invalid-convert (string->uninterned-symbol "/dev/zero"))
            cr)))
