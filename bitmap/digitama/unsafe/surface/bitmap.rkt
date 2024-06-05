#lang racket/base

(provide (all-defined-out))

(require racket/unsafe/ops)

(require "../pangocairo.rkt")
(require "../convert.rkt")
(require "image.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define create-argb-bitmap
  (case-lambda
    [(width height density scale?)
     (let*-values ([(sfc cr width height) (cairo-create-argb-image-surface* width height density scale?)]
                   [(shadow) (make-phantom-bytes (unsafe-fx* (unsafe-fx* width height) 4))])
       (values (bitmap bitmap-convert shadow sfc '/dev/ram density width height 4 8)
               cr))]
    [(width height background density scale?)
     (let-values ([(img cr) (create-argb-bitmap width height density scale?)])
       (cairo-set-source cr background)
       (cairo_paint cr)
       (values img cr))]))

(define create-invalid-bitmap
  (lambda [width height density scale?]
    (define-values (sfc cr width height) (cairo-create-argb-image-surface* width height density scale?))
    (define shadow (make-phantom-bytes (unsafe-fx* (unsafe-fx* width height) 4)))

    (values (bitmap invalid-convert shadow sfc (string->uninterned-symbol "/dev/zero") density width height 4 8)
            cr)))
