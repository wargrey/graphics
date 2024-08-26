#lang racket

(require "../../digitama/unsafe/pangocairo.rkt")
(require "../../digitama/unsafe/surface/abstract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track
  (make-cairo-abstract-surface 64.0 0.0 1.0 #true
                               (λ [cr flwidth flheight]
                                 (cairo_move_to cr -16.0 +16.0)
                                 (cairo_line_to cr +16.0 +16.0)
                                 (cairo_stroke cr)
                                 (void))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (cairo_recording_surface_get_extents track)
  (cairo_recording_surface_ink_extents track))
