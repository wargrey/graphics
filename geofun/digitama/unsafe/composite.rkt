#lang typed/racket/base

(provide (all-defined-out))

(require "paint.rkt")
(require "source.rkt")
(require "typed/cairo.rkt")
(require "typed/more.rkt")

(require "../convert.rkt")
(require "../layer/type.rkt")
(require "../../stroke.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Layer (GLayerof Geo))
(define-type Geo-Layer-List (GLayer-Listof Geo))
(define-type Geo-Layer-Group (GLayer-Groupof Geo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo_composite : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Byte) (Option Byte) Geo-Layer-Group Any)
  (lambda [cr x0 y0 width height base-op sibs-op group]
    (define geo-objects (glayer-group-layers group))
    (define saved-op (cairo_get_operator cr))

    (when (or base-op) (cairo_set_operator cr base-op))
    (geo-composite-layer cr (car geo-objects) x0 y0)
    
    (when (or sibs-op) (cairo_set_operator cr sibs-op))
    (let combine ([geos (cdr geo-objects)])
      (when (pair? geos)
        (geo-composite-layer cr (car geos) x0 y0)
        (combine (cdr geos))))

    (cairo_set_operator cr saved-op)))

(define geo_framed_composite : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   (Option Byte) (Option Byte) Geo-Layer-Group Flonum Flonum
                                   Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   (Option Stroke) (Option Fill-Source) Any)
  (lambda [cr x0 y0 width height base-op sibs-op group dest-x dest-y border-x border-y border-width border-height border background]
    (define geo-objects (glayer-group-layers group))
    (define-values (dest-width dest-height) (values (glayer-group-width group) (glayer-group-height group)))
    (define-values (dx dy) (values (+ x0 dest-x) (+ y0 dest-y)))
    
    (cairo_save cr)
    (when (or base-op) (cairo_set_operator cr base-op))
    (cairo_rectangle cr (+ x0 border-x) (+ y0 border-y) border-width border-height)
    (cairo-render cr border background)

    (cairo_push_group cr)
    (when (or sibs-op) (cairo_set_operator cr sibs-op))
    (let combine ([geos geo-objects])
      (when (pair? geos)
        (geo-composite-layer cr (car geos) dx dy)
        (combine (cdr geos))))
    (cairo_pop_group_to_source cr)
    (cairo_paint cr)

    (cairo_restore cr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-composite-layer : (-> Cairo-Ctx Geo-Layer Flonum Flonum Void)
  (lambda [cr self dx dy]
    (define master (glayer-master self))
    
    (cairo-composite! master cr (geo<%>-draw! master)
                      (+ (glayer-x self) dx) (+ (glayer-y self) dy)
                      (glayer-width self) (glayer-height self))))
