#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require geofun/digitama/unsafe/visual/ctype)
(require "../convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require geofun/digitama/unsafe/pangocairo)
  (require geofun/digitama/unsafe/paint)

  (require "../convert.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (geo_composite operator layers density)
    (define width (unsafe-vector*-ref layers 0))
    (define height (unsafe-vector*-ref layers 1))
    (define geometries (unsafe-vector*-ref layers 2))
    (define-values (geo cr) (create-abstract-surface width height density #true))
    (define op (or operator CAIRO_OPERATOR_OVER))

    (geo-composite cr (unsafe-car geometries) CAIRO_OPERATOR_SOURCE density)

    (let combine ([geos (unsafe-cdr geometries)])
      (unless (null? geos)
        (geo-composite cr (unsafe-car geos) op density)
        (combine (unsafe-cdr geos))))
    
    (cairo_destroy cr)
    geo)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (geo-composite cr child op density)
    (cairo-composite cr (geo-create-surface (unsafe-vector*-ref child 0))
                     (unsafe-vector*-ref child 1) (unsafe-vector*-ref child 2)
                     (unsafe-vector*-ref child 3) (unsafe-vector*-ref child 4)
                     CAIRO_FILTER_BILINEAR op density)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [geo_composite (-> (Option Integer) Geo-Layers Flonum Abstract-Surface)])

(define-type Geo-Layer (Immutable-Vector Geo<%> Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
(define-type Geo-Layers (Immutable-Vector Nonnegative-Flonum Nonnegative-Flonum (Pairof Geo-Layer (Listof Geo-Layer))))
