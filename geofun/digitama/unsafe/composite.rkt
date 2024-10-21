#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "visual/ctype.rkt")
(require "source.rkt")
(require "paint.rkt")
(require "cairo.rkt")
(require "more.rkt")

(require "../base.rkt")
(require "../convert.rkt")
(require "../layer/type.rkt")
(require "../../stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Layer (GLayerof Geo))
(define-type Geo-Layer-List (GLayer-Listof Geo))
(define-type Geo-Layer-Group (GLayer-Groupof Geo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo_composite : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                            (Option Integer) Geo-Layer-Group Positive-Flonum Any)
  (lambda [cr x0 y0 width height operator group density]
    (define geo-objects (glayer-group-layers group))
    (define op (or operator CAIRO_OPERATOR_OVER))
    
    (geo-composite-layer cr (car geo-objects) CAIRO_OPERATOR_SOURCE density)
    (let combine ([geos (cdr geo-objects)])
      (when (pair? geos)
        (geo-composite-layer cr (car geos) op density)
        (combine (cdr geos))))))

(define geo_framed_composite : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   (Option Integer) Geo-Layer-Group
                                   Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   (Option Stroke) (Option Fill-Source) Positive-Flonum Any)
  (lambda [cr x0 y0 width height operator group border-x border-y border-width border-height dest-x dest-y dest-width dest-height border background density]
    (define geo-objects (glayer-group-layers group))
    (define op (or operator CAIRO_OPERATOR_OVER))
    
    (cairo_rectangle cr (+ x0 border-x) (+ y0 border-y) border-width border-height)
    (cairo-render cr border background)
    
    (when (or background)
      (cairo-render-with-fill cr background)
      (cairo_push_group cr))
    
    (when (or border)
      (cairo_rectangle cr (+ x0 border-x) (+ y0 border-y) border-width border-height)
      (cairo-render-with-stroke cr border))
    
    (geo-composite-layer cr (car geo-objects) CAIRO_OPERATOR_SOURCE dest-x dest-y density)
    
    (let combine ([geos (cdr geo-objects)])
      (unless (null? geos)
        (geo-composite-layer cr (car geos) op dest-x dest-y density)
        (combine (cdr geos))))
    
    (when (and background)
      (cairo_pop_group_to_source cr)
      (cairo_paint cr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite-layer : (case-> [Cairo-Ctx Geo-Layer Integer Positive-Flonum Void]
                                      [Cairo-Ctx Geo-Layer Integer Flonum Flonum Positive-Flonum Void])
  (case-lambda
    [(cr self op density)
     (cairo-composite cr (geo-create-surface (unsafe-struct*-ref self 0))
                      (unsafe-struct*-ref self 1) (unsafe-struct*-ref self 2)
                      (unsafe-struct*-ref self 3) (unsafe-struct*-ref self 4)
                      CAIRO_FILTER_BILINEAR op density)]
    [(cr self op dx dy density)
     (cairo-composite cr (geo-create-surface (unsafe-struct*-ref self 0))
                      (+ (unsafe-struct*-ref self 1) dx)
                      (+ (unsafe-struct*-ref self 2) dy)
                      (unsafe-struct*-ref self 3) (unsafe-struct*-ref self 4)
                      CAIRO_FILTER_BILINEAR op density)
     (void)]))
