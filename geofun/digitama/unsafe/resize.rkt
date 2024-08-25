#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require geofun/digitama/unsafe/visual/ctype)

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require "surface/abstract.rkt")
  
  (require (submod "visual/abstract.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (geo_section src x y width height density)
    (define-values (sfc cr _w _h) (cairo-create-abstract-surface* (unsafe-fl/ width density) (unsafe-fl/ height density) density #false))
    (cairo_set_source_surface cr src (unsafe-fl- 0.0 x) (unsafe-fl- 0.0 y))
    (cairo_paint cr)
    (cairo_destroy cr)
    sfc)

  (define (geo_scale src xscale yscale density)
    (define-values (width height) (abstract-surface-content-size src))
    (define flwidth (unsafe-fl* width (unsafe-flabs xscale)))
    (define flheight (unsafe-fl* height (unsafe-flabs yscale)))
    (define-values (sfc cr actual-flwidth actual-flheight) (cairo-create-abstract-surface* flwidth flheight density #false))
    (define tx (if (unsafe-fl< xscale 0.0) actual-flwidth 0.0))
    (define ty (if (unsafe-fl< yscale 0.0) actual-flheight 0.0))

    ; order matters
    (cairo_translate cr tx ty)
    (cairo_scale cr xscale yscale)
    
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_paint cr)
    (cairo_destroy cr)
    
    sfc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [geo_section (-> Abstract-Surface Flonum Flonum Flonum Flonum Flonum Abstract-Surface)]
 [geo_scale (-> Abstract-Surface Flonum Flonum Flonum Abstract-Surface)])
