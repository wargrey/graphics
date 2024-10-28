#lang typed/racket/base

(provide (all-defined-out))

(require "source.rkt")
(require "paint.rkt")

(require "typed/cairo.rkt")
(require "typed/more.rkt")
(require "visual/ctype.rkt")

(require "../convert.rkt")
(require "../layer/type.rkt")
(require "../../stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Layer (GLayerof Geo))
(define-type Geo-Layer-List (GLayer-Listof Geo))
(define-type Geo-Layer-Group (GLayer-Groupof Geo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo_composite : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Byte) Geo-Layer-Group Any)
  (lambda [cr x0 y0 width height operator group]
    (define geo-objects (glayer-group-layers group))

    (cairo_set_operator cr CAIRO_OPERATOR_SOURCE)
    (geo-composite-layer cr (car geo-objects) x0 y0)
    (cairo_set_operator cr (or operator CAIRO_OPERATOR_OVER))
    
    (let combine ([geos (cdr geo-objects)])
      (when (pair? geos)
        (geo-composite-layer cr (car geos) x0 y0)
        (combine (cdr geos))))))

(define geo_framed_composite : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   (Option Byte) Geo-Layer-Group Flonum Flonum
                                   Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   (Option Stroke) (Option Fill-Source) Any)
  (lambda [cr x0 y0 width height operator group dest-x dest-y border-x border-y border-width border-height border background]
    (define geo-objects (glayer-group-layers group))
    (define op (or operator CAIRO_OPERATOR_OVER))
    (define-values (dx dy) (values (+ x0 dest-x) (+ y0 dest-y)))
    
    (cairo_rectangle cr (+ x0 border-x) (+ y0 border-y) border-width border-height)
    (cairo-render cr border background)
    
    (when (or background)
      (cairo-render-with-fill cr background)
      (cairo_push_group cr))
    
    (when (or border)
      (cairo_rectangle cr (+ x0 border-x) (+ y0 border-y) border-width border-height)
      (cairo-render-with-stroke cr border))

    (cairo_set_operator cr CAIRO_OPERATOR_SOURCE)
    (geo-composite-layer cr (car geo-objects) dx dy)
    (cairo_set_operator cr (or operator CAIRO_OPERATOR_OVER))
    
    (let combine ([geos (cdr geo-objects)])
      (unless (null? geos)
        (geo-composite-layer cr (car geos) dx dy)
        (combine (cdr geos))))
    
    (when (and background)
      (cairo_pop_group_to_source cr)
      (cairo_paint cr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite-layer : (-> Cairo-Ctx Geo-Layer Flonum Flonum Void)
  (lambda [cr self dx dy]
    (define master (glayer-master self))

    (cairo-composite! master cr (geo<%>-draw! master)
                      (+ (glayer-x self) dx) (+ (glayer-y self) dy)
                      (glayer-width self) (glayer-height self))))
