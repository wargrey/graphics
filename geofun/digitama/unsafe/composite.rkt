#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "visual/ctype.rkt")
(require "source.rkt")

(require "../base.rkt")
(require "../layer/type.rkt")
(require "../self.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require "pangocairo.rkt")
  (require "../convert.rkt")
  (require "../../stroke.rkt")

  (require (submod "paint.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (geo_composite operator layers density)
    (define width (unsafe-vector*-ref layers 0))
    (define height (unsafe-vector*-ref layers 1))
    (define geo-objects (unsafe-vector*-ref layers 2))
    (define-values (geo cr) (create-abstract-surface width height density #true))
    (define op (or operator CAIRO_OPERATOR_OVER))

    (geo-composite-layer cr (unsafe-car geo-objects) CAIRO_OPERATOR_SOURCE density)
    (let combine ([geos (unsafe-cdr geo-objects)])
      (unless (null? geos)
        (geo-composite-layer cr (unsafe-car geos) op density)
        (combine (unsafe-cdr geos))))
    (cairo_destroy cr)
    
    geo)

  (define (geo_framed_composite operator layers mtop mright mbottom mleft ptop pright pbottom pleft border background density)
    (define dest-width (unsafe-vector*-ref layers 0))
    (define dest-height (unsafe-vector*-ref layers 1))
    (define geo-objects (unsafe-vector*-ref layers 2))
    (define line-width (stroke-maybe-width border))
    (define line-inset (unsafe-fl* line-width 0.5))
    (define-values (width  border-x border-width  dest-x) (frame-metrics line-width line-inset mleft mright pleft pright dest-width))
    (define-values (height border-y border-height dest-y) (frame-metrics line-width line-inset mtop mbottom ptop pbottom dest-height))
    (define-values (geo cr) (create-abstract-surface width height density #true))
    (define op (or operator CAIRO_OPERATOR_OVER))

    (cairo_rectangle cr border-x border-y border-width border-height)
    (cairo-render cr border background)

    (when (or background)
      (cairo-set-source cr background)
      (cairo_paint cr) 
      (cairo_push_group cr))

    (when (or border)
      (cairo_rectangle cr border-x border-y border-width border-height)
      (cairo-render-with-stroke cr border))
      
    (geo-composite-layer cr (unsafe-car geo-objects) CAIRO_OPERATOR_SOURCE dest-x dest-y density)

    (let combine ([geos (unsafe-cdr geo-objects)])
      (unless (null? geos)
        (geo-composite-layer cr (unsafe-car geos) op dest-x dest-y density)
        (combine (unsafe-cdr geos))))

    (when (and background)
      (cairo_pop_group_to_source cr)
      (cairo_paint cr))
    
    (cairo_destroy cr)
    geo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define geo-composite-layer
    (case-lambda
      [(cr self op density)
       (cairo-composite cr (geo-create-surface (unsafe-struct*-ref self 0))
                        (unsafe-struct*-ref self 1) (unsafe-struct*-ref self 2)
                        (unsafe-struct*-ref self 3) (unsafe-struct*-ref self 4)
                        CAIRO_FILTER_BILINEAR op density)]
      [(cr self op dx dy density)
       (cairo-composite cr (geo-create-surface (unsafe-struct*-ref self 0))
                        (unsafe-fl+ (unsafe-struct*-ref self 1) dx)
                        (unsafe-fl+ (unsafe-struct*-ref self 2) dy)
                        (unsafe-struct*-ref self 3) (unsafe-struct*-ref self 4)
                        CAIRO_FILTER_BILINEAR op density)]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_frame_size dest-width dest-height mtop mright mbottom mleft ptop pright pbottom pleft border)
    (define line-width (stroke-maybe-width border))
    (define line-inset (unsafe-fl* line-width 0.5))
    (define-values (width border-x border-width dest-x) (frame-metrics line-width line-inset mleft mright pleft pright dest-width))
    (define-values (height border-y border-height dest-y) (frame-metrics line-width line-inset mtop mbottom ptop pbottom dest-height))

    (values width height border-x border-y border-width border-height dest-x dest-y))
  
  (define (frame-metrics line-width line-inset flmopen flmclose flpopen flpclose size)
    (define border-position (unsafe-fl+ flmopen line-inset))
    (define body-position (unsafe-fl+ (unsafe-fl+ flmopen flpopen) line-width))
    (define border-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ flpopen flpclose) size) line-width))
    (define frame-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ border-position border-size) flmclose) line-inset))
    
    (values frame-size border-position border-size body-position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [geo_composite (-> (Option Integer) Geo-Layer-Group Positive-Flonum Abstract-Surface)]
 [geo_framed_composite (-> (Option Integer) Geo-Layer-Group
                           Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                           Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                           (Option Paint) (Option Fill-Source) Positive-Flonum Abstract-Surface)]

 [dc_frame_size (-> Nonnegative-Flonum Nonnegative-Flonum
                    Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                    Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                    (Option Paint)
                    (Values Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum))])

(define-type Geo-Layer (GLayerof Geo))
(define-type Geo-Layer-List (GLayer-Listof Geo))
(define-type Geo-Layer-Group (GLayer-Groupof Geo))
