#lang racket/base

(provide cairo-mesh-pattern) ; for tamer/prefab.rkt

(require "../../digitama/unsafe/pangocairo.rkt")
(require bitmap/digitama/unsafe/surface)

;;; https://www.cairographics.org/manual/cairo-cairo-pattern-t.html#cairo-pattern-create-mesh

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cairo-mesh-pattern width height ratio density)
  (define-values (bmp cr) (create-argb-bitmap width height density #true))
  (define pattern (cairo_pattern_create_mesh))
  (define-values (side rest) (values (* height ratio) (* height (- 1.0 ratio))))
  (define-values (tx ty) (values (- width (* side 0.5)) (- height (* side 0.5))))

  ; Add a Coons patch
  (cairo_mesh_pattern_begin_patch pattern)
  (cairo_mesh_pattern_move_to pattern 0.0 0.0)
  (cairo_mesh_pattern_curve_to pattern side         (- side) (* side 2.0) side         tx  0.0)
  (cairo_mesh_pattern_curve_to pattern (* side 2.0) side     width        (* side 2.0) tx  ty)
  (cairo_mesh_pattern_curve_to pattern (* side 2.0) rest     side         height       0.0 ty)
  (cairo_mesh_pattern_curve_to pattern side         rest     (- side)     side         0.0 0.0)
  (cairo_mesh_pattern_set_corner_color_rgba pattern 0 2.0 0.0 0.0 1.0)
  (cairo_mesh_pattern_set_corner_color_rgba pattern 1 0.0 1.0 0.0 1.0)
  (cairo_mesh_pattern_set_corner_color_rgba pattern 2 0.0 0.0 1.0 1.0)
  (cairo_mesh_pattern_set_corner_color_rgba pattern 3 1.0 1.0 0.0 1.0)
  (cairo_mesh_pattern_end_patch pattern)

  ; Add a Gouraud-shaded triangle
  (cairo_mesh_pattern_begin_patch pattern)
  (cairo_mesh_pattern_move_to pattern tx ty)
  (cairo_mesh_pattern_line_to pattern width rest)
  (cairo_mesh_pattern_line_to pattern width height)
  (cairo_mesh_pattern_set_corner_color_rgba pattern 0 1.0 0.0 0.0 1.0)
  (cairo_mesh_pattern_set_corner_color_rgba pattern 1 0.0 1.0 0.0 1.0)
  (cairo_mesh_pattern_set_corner_color_rgba pattern 2 0.0 0.0 1.0 1.0)
  (cairo_mesh_pattern_end_patch pattern)

  (cairo_set_source cr pattern)
  (cairo_paint cr)
  
  bmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (cairo-mesh-pattern 256.0 256.0 0.32 2.0))
