#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "visual/ctype.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/draw/unsafe/cairo)
  (require racket/unsafe/ops)
  
  (require "../geometry/affine.rkt")

  (require (submod "visual/abstract.rkt" unsafe))
  (require (submod "surface/abstract.rkt" unsafe))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (geo_section src x y width height density)
    (define-values (sfc cr _w _h) (cairo-create-abstract-surface* (unsafe-fl/ width density) (unsafe-fl/ height density) density #false))
    (cairo_set_source_surface cr src (unsafe-fl- 0.0 x) (unsafe-fl- 0.0 y))
    (cairo_paint cr)
    sfc)

  (define (geo_scale src xscale yscale density)
    (define-values (_ width height) (abstract-surface-extent* src))
    (define flwidth (unsafe-fl* width (unsafe-flabs xscale)))
    (define flheight (unsafe-fl* height (unsafe-flabs yscale)))
    (define-values (sfc cr used-width used-flheight) (cairo-create-abstract-surface* flwidth flheight density #false))
    (define tx (if (unsafe-fl< xscale 0.0) used-width 0.0))
    (define ty (if (unsafe-fl< yscale 0.0) used-flheight 0.0))

    ; order matters
    (cairo_translate cr tx ty)
    (cairo_scale cr xscale yscale)
    
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_paint cr)
    
    sfc)

  (define (geo_rotate src theta.rad density)
    (define-values (_ ow oh) (abstract-surface-extent* src))
    (define-values (flwidth flheight) (geo-size-rotate ow oh theta.rad))
    (define-values (sfc cr rw rh) (cairo-create-abstract-surface* flwidth flheight density #false))

    ; order matters
    (cairo_translate cr (unsafe-fl* rw 0.5) (unsafe-fl* rh 0.5))
    (cairo_rotate cr theta.rad)
    (cairo_set_source_surface cr src (unsafe-fl* ow -0.5) (unsafe-fl* oh -0.5))
    
    (cairo_paint cr)

    sfc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [geo_section (-> Abstract-Surface Flonum Flonum Flonum Flonum Positive-Flonum Abstract-Surface)]
 [geo_scale (-> Abstract-Surface Flonum Flonum Positive-Flonum Abstract-Surface)]
 [geo_rotate (-> Abstract-Surface Flonum Positive-Flonum Abstract-Surface)])
