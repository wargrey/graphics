#lang racket/base

(require "../../digitama/unsafe/cairo.rkt")
(require (submod "../../digitama/unsafe/surface/abstract.rkt" unsafe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-cairo-abstract-surface
  (lambda [flwidth flheight density scale?]
    (define-values (surface cr width height) (cairo-create-abstract-surface* flwidth flheight density scale?))

    (cairo_move_to cr -16.0 +16.0)
    (cairo_line_to cr +16.0 +16.0)
    (cairo_stroke cr)
                                 
    surface))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define track (make-cairo-abstract-surface 64.0 0.0 1.0 #true))
  
  (cairo_recording_surface_get_extents track)
  (cairo_recording_surface_ink_extents track))
