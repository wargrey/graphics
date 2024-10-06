#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require geofun/digitama/unsafe/visual/ctype)
(require geofun/digitama/unsafe/source)

(require geofun/digitama/base)
(require geofun/digitama/geometry/footprint)

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require geofun/digitama/unsafe/pangocairo)
  (require geofun/digitama/unsafe/paint)
  
  (require (submod geofun/digitama/unsafe/path unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_edge create-surface flwidth flheight footprints offset x-stroke? y-stroke? stroke source target adjust-offset density)
    (define-values (dx dy) (values (unsafe-flreal-part offset) (unsafe-flimag-part offset)))
    (define-values (flw flh tx ty) (dc-path-window flwidth flheight dx dy stroke x-stroke? y-stroke?))
    (define-values (sfc cr) (create-surface flw flh density #true))
    
    (cairo_new_path cr)
    (cairo_clean_path cr footprints tx ty (unsafe-car adjust-offset) (unsafe-cdr adjust-offset))
    (cairo-render cr stroke #false)
    (dc-edge-draw-shape cr source)
    (dc-edge-draw-shape cr target)
    (cairo_destroy cr)
    
    sfc)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc-edge-draw-shape cr shape)
    (define prints (unsafe-vector*-ref shape 0))

    (when (pair? prints)
      (define offset (unsafe-vector*-ref shape 1))

      ;(cairo_set_operator cr CAIRO_OPERATOR_SOURCE)
      (cairo_new_path cr)
      (cairo_path cr prints 0.0 0.0)
      (cairo-render cr (unsafe-vector*-ref shape 1) (unsafe-vector*-ref shape 2)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_edge (All (S) (-> (Cairo-Surface-Create S) 
                       Nonnegative-Flonum Nonnegative-Flonum (Listof Geo-Path-Clean-Print) Float-Complex Boolean Boolean (Option Paint)
                       (Immutable-Vector (Listof Geo-Path-Print) (Option Paint) (Option Fill-Source))
                       (Immutable-Vector (Listof Geo-Path-Print) (Option Paint) (Option Fill-Source))
                       (Pairof (Option Float-Complex) (Option Float-Complex))
                       Positive-Flonum
                       S))])
