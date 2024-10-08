#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "visual/ctype.rkt")
(require "source.rkt")
(require "../layer/type.rkt")
(require "../convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require "pangocairo.rkt")
  (require "paint.rkt")
  (require "../convert.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (geo_composite operator layers background density)
    (define width (unsafe-vector*-ref layers 0))
    (define height (unsafe-vector*-ref layers 1))
    (define geo-objects (unsafe-vector*-ref layers 2))
    (define-values (geo cr) (create-abstract-surface width height density #true))
    (define op (or operator CAIRO_OPERATOR_OVER))

    (when (or background)
      (cairo-set-source cr background)
      (cairo_paint cr) 
      (cairo_push_group cr))
      
    (geo-composite-layer cr (unsafe-car geo-objects) CAIRO_OPERATOR_SOURCE density)

    (let combine ([geos (unsafe-cdr geo-objects)])
      (unless (null? geos)
        (geo-composite-layer cr (unsafe-car geos) op density)
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
                        CAIRO_FILTER_BILINEAR op density)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [geo_composite (-> (Option Integer) Geo-Layer-Group (Option Fill-Source) Positive-Flonum Abstract-Surface)])

(define-type Geo-Layer (GLayerof Geo))
(define-type Geo-Layer-List (GLayer-Listof Geo))
(define-type Geo-Layer-Group (GLayer-Groupof Geo))
