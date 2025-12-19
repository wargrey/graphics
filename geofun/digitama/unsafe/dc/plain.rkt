#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/paint/self)

(require "../paint.rkt")
(require "../typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_pattern : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Brush Any)
  (lambda [cr x0 y0 width height background]
    (cairo-render-background cr background x0 y0 width height)))

(define dc_image : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Byte Bitmap-Surface Any)
  (lambda [cr x0 y0 width height filter img-sfc]
    (define img-width  (exact->inexact (cairo_image_surface_get_width  img-sfc)))
    (define img-height (exact->inexact (cairo_image_surface_get_height img-sfc)))

    (cairo-composite cr img-sfc x0 y0 width height filter
                     (/ width img-width) (/ height img-height))))

(define dc_frame : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Bitmap-Surface
                       Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                       Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                       (Option Pen) (Option Brush) Byte Positive-Flonum Any)
  (lambda [cr x0 y0 width height src
              border-x border-y border-width border-height dest-x dest-y dest-width dest-height
              border background filter density]
    (define s (/ 1.0 density))
    
    (cairo_rectangle cr (+ x0 border-x) (+ y0 border-y) border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src (+ x0 dest-x) (+ y0 dest-y) dest-width dest-height filter s s)))

(define dc_grid : (case-> [Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                     (Listof Nonnegative-Flonum) (Listof Nonnegative-Flonum) (Option Pen)
                                     -> Any]
                          [Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                     (Listof Nonnegative-Flonum) (Listof Nonnegative-Flonum) (Option Pen)
                                     (Listof Nonnegative-Flonum) (Listof Nonnegative-Flonum) (Option Pen)
                                     -> Any])
  (case-lambda
    [(cr x0 y0 width height xs ys stroke)
     (unless (not stroke)
       (define xn (+ x0 width))
       (define yn (+ y0 height))

       (cairo_new_path cr)
       (let draw-grid ([xs : (Listof Nonnegative-Flonum) xs])
         (when (pair? xs)
           (define x (+ x0 (car xs)))
           
           (cairo-add-line cr x y0 x yn)
           (draw-grid (cdr xs))))
       
       (let draw-grid ([ys : (Listof Nonnegative-Flonum) ys])
         (when (pair? ys)
           (define y (+ y0 (car ys)))
           
           (cairo-add-line cr x0 y xn y)
           (draw-grid (cdr ys))))
       
       (cairo-render cr stroke #false))]
    [(cr x0 y0 width height major-xs major-ys major-stroke minor-xs minor-ys minor-stroke)
     (dc_grid cr x0 y0 width height minor-xs minor-ys minor-stroke)
     (dc_grid cr x0 y0 width height major-xs major-ys major-stroke)]))
