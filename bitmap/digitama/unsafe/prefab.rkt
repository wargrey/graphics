#lang typed/racket/base

(provide (all-defined-out))

(require "../../draw.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out) is-a? bitmap%)
  
  (require "pangocairo.rkt")

  (define (bitmap-save bmp)
    (list (let-values ([(raw size) (cairo-surface-data (send bmp get-handle))]) raw)
          (send bmp get-width) (send bmp get-height)
          (send bmp get-backing-scale)))
  
  (define (bitmap-restore src width height density)
    (define bmp (make-bitmap width height #:backing-scale density))
    (define surface (send bmp get-handle))
    (define-values (dest total) (cairo-surface-data surface))
    (memcpy dest 0 src 0 total _byte)
    (cairo_surface_mark_dirty surface)
    bmp))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap-restore (-> Bytes Positive-Integer Positive-Integer Positive-Real Bitmap)])
