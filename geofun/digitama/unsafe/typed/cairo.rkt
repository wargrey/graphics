#lang typed/racket/base

(provide (all-defined-out))

(require "../visual/ctype.rkt")

(require typed/racket/unsafe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 ffi/unsafe/atomic
 [start-breakable-atomic (-> Void)]
 [end-breakable-atomic (-> Void)])

(unsafe-require/typed/provide
 racket/draw/unsafe/cairo
 [CAIRO_FILTER_BILINEAR Positive-Byte]
 [CAIRO_FILTER_BEST Positive-Byte]
 [CAIRO_OPERATOR_OVER Positive-Byte]
 [CAIRO_OPERATOR_SOURCE Positive-Byte]
 [CAIRO_STATUS_SUCCESS Byte]
 [CAIRO_FORMAT_ARGB32 Byte])

(unsafe-require/typed/provide
 racket/draw/unsafe/cairo
 [cairo_save (-> Cairo-Ctx Void)]
 [cairo_restore (-> Cairo-Ctx Void)]
 [cairo_paint (-> Cairo-Ctx Void)]
 [cairo_paint_with_alpha (-> Cairo-Ctx Flonum Void)]
 [cairo_create (-> (U Cairo-Surface Cairo-Stream-Surface) Cairo-Ctx)]
 [cairo_set_operator (-> Cairo-Ctx Byte Void)]
 [cairo_set_source_surface (-> Cairo-Ctx (U Cairo-Surface Cairo-Stream-Surface) Flonum Flonum Void)]
 [cairo_destroy (-> Cairo-Ctx Void)])

(unsafe-require/typed/provide
 racket/draw/unsafe/cairo
 [cairo_surface_flush (-> (U Cairo-Surface Cairo-Stream-Surface) Void)]
 [cairo_surface_finish (-> (U Cairo-Surface Cairo-Stream-Surface) Void)]
 [cairo_surface_destroy (-> (U Cairo-Surface Cairo-Stream-Surface) Void)]
 [cairo_surface_status (-> (U Cairo-Surface Cairo-Stream-Surface) Fixnum)]
 [cairo_surface_mark_dirty (-> Bitmap-Surface Void)]
 
 [cairo_surface_write_to_png_stream (-> Bitmap-Surface (-> Any Bytes Index Index) Void)]
 [cairo_pdf_surface_create_for_stream (-> (-> Bytes Index Index) Nonnegative-Flonum Nonnegative-Flonum PDF-Surface)]
 [cairo_svg_surface_create_for_stream (-> (-> Bytes Index Index) Nonnegative-Flonum Nonnegative-Flonum SVG-Surface)])

(unsafe-require/typed/provide
 racket/draw/unsafe/cairo
 [cairo_translate (-> Cairo-Ctx Flonum Flonum Void)]
 [cairo_scale (-> Cairo-Ctx Flonum Flonum Void)]
 [cairo_rotate (-> Cairo-Ctx Flonum Void)])

(unsafe-require/typed/provide
 racket/draw/unsafe/cairo
 [cairo_new_path (-> Cairo-Ctx Void)]
 [cairo_move_to (-> Cairo-Ctx Flonum Flonum Void)]
 [cairo_line_to (-> Cairo-Ctx Flonum Flonum Void)]
 [cairo_curve_to (-> Cairo-Ctx Flonum Flonum Flonum Flonum Flonum Flonum Void)]
 [cairo_rel_move_to (-> Cairo-Ctx Flonum Flonum Void)]
 [cairo_rel_line_to (-> Cairo-Ctx Flonum Flonum Void)]
 [cairo_arc (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Flonum Flonum Void)]
 [cairo_arc_negative (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Flonum Flonum Void)]
 [cairo_rectangle (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Void)]
 [cairo_close_path (-> Cairo-Ctx Void)]
 [cairo_path_extents (-> Cairo-Ctx (Values Flonum Flonum Flonum Flonum))]
 [cairo_path_destroy (-> Cairo-Ctx Void)])

(unsafe-require/typed/provide
 racket/draw/unsafe/cairo
 [cairo_pattern_set_filter (-> (U Cairo-Surface Cairo-Stream-Surface) Byte Void)])

(unsafe-require/typed/provide
 racket/draw/unsafe/cairo
 [cairo_image_surface_create (-> Byte Nonnegative-Flonum Nonnegative-Flonum Bitmap-Surface)]
 [cairo_image_surface_get_data (-> Bitmap-Surface Bytes)]
 [cairo_image_surface_get_width (-> Bitmap-Surface Index)]
 [cairo_image_surface_get_height (-> Bitmap-Surface Index)]
 [cairo_image_surface_get_stride (-> Bitmap-Surface Index)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-backend-scale : (-> Cairo-Ctx Positive-Flonum Boolean Void)
  (lambda [cr density scale?]
    (unless (or (not scale?) (= density 1.0))
      (cairo_scale cr density density))))

(define cairo-positive-arc : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Void)
  (lambda [cr cx cy rx ry rstart rend]
    (cairo-smart-elliptical-arc cr cx cy rx ry rstart rend cairo_arc)))

(define cairo-negative-arc : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Void)
  (lambda [cr cx cy rx ry rstart rend]
    (cairo-smart-elliptical-arc cr cx cy rx ry rstart rend cairo_arc_negative)))

(define cairo-smart-elliptical-arc : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum
                                         (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Flonum Flonum Void)
                                         Void)
  (lambda [cr cx cy rx ry rstart rend add-arc]
    ;;; WARNING
    ;; For drawing elliptical arcs
    ;;   `cairo_translate` is necessary,
    ;;   or the resulting shapes will be weird.
    ;; TODO: find the reason;
    ;; TODO: why not `density` should be used to scale
    
    (cond [(< rx ry)
           (cairo_save cr)
           (cairo_translate cr cx cy)
           (cairo_scale cr 1.0 (/ ry rx))
           (add-arc cr 0.0 0.0 rx rstart rend)
           (cairo_restore cr)]
          [(> rx ry)
           (cairo_save cr)
           (cairo_translate cr cx cy)
           (cairo_scale cr (/ rx ry) 1.0)
           (add-arc cr 0.0 0.0 ry rstart rend)
           (cairo_restore cr)]
          [else ; no need to `translate` for circles
           (add-arc cr cx cy rx rstart rend)])))

(define cairo-add-line : (case-> [Cairo-Ctx Float-Complex Float-Complex -> Void]
                                 [Cairo-Ctx Float-Complex Float-Complex Float-Complex -> Void]
                                 [Cairo-Ctx Flonum Flonum Flonum Flonum -> Void]
                                 [Cairo-Ctx Flonum Flonum Flonum Flonum Flonum Flonum -> Void])
  (case-lambda
    [(cr pt1 pt2)
     (cairo_move_to cr (real-part pt1) (imag-part pt1))
     (cairo_line_to cr (real-part pt2) (imag-part pt2))]
    [(cr pt1 pt2 pt3)
     (cairo_move_to cr (real-part pt1) (imag-part pt1))
     (cairo_line_to cr (real-part pt2) (imag-part pt2))
     (cairo_line_to cr (real-part pt3) (imag-part pt3))]
    [(cr x1 y1 x2 y2)
     (cairo_move_to cr x1 y1)
     (cairo_line_to cr x2 y2)]
    [(cr x1 y1 x2 y2 x3 y3)
     (cairo_move_to cr x1 y1)
     (cairo_line_to cr x2 y2)
     (cairo_line_to cr x3 y3)]))
