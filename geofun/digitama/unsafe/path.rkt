#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")

(require "source.rkt")
(require "visual/ctype.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "paint.rkt")
  (require "pangocairo.rkt")
  (require "surface/abstract.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (path_stamp footprints dx dy stroke fill-color fill-rule density)
    (define linewidth (~bdwidth stroke))
    (define offset (unsafe-fl* linewidth 0.5))
    (make-cairo-abstract-surface 0.0 0.0 density #true
                                 (λ [cr _inf.w _inf.h]
                                   (cairo_path cr footprints (unsafe-fl+ dx offset) (unsafe-fl+ dy offset))
                                   (cairo-render cr stroke fill-color fill-rule))))

  (define (cairo_path cr footprints dx dy)
    (let draw_path ([anchors footprints])
      (when (pair? anchors)
        (define op+footprint (unsafe-car anchors))
        (define footprint (unsafe-cdr op+footprint))
        (case (unsafe-car op+footprint)
          [(#\M) (cairo_move_to cr (unsafe-fl+ (unsafe-flreal-part footprint) dx) (unsafe-fl+ (unsafe-flimag-part footprint) dy))]
          [(#\L) (cairo_line_to cr (unsafe-fl+ (unsafe-flreal-part footprint) dx) (unsafe-fl+ (unsafe-flimag-part footprint) dy))]
          [(#\A) (cairo_elliptical_arc cr footprint dx dy)]
          [(#\C) (cairo_cubic_bezier cr (unsafe-struct*-ref footprint 0) (unsafe-struct*-ref footprint 1) (unsafe-struct*-ref footprint 2) dx dy)]
          [(#\Q) (cairo_quadratic_bezier cr (unsafe-struct*-ref footprint 0) (unsafe-struct*-ref footprint 1) (unsafe-struct*-ref footprint 2) dx dy)]
          [(#\Z #\z) (cairo_close_path cr)]
          [(#\m) (cairo_rel_move_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))]
          [(#\l) (cairo_rel_line_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))])

        (draw_path (unsafe-cdr anchors)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_polyline create-surface flwidth flheight footprints dx dy stroke close? density)
    (define line-width (~bdwidth stroke))
    (define-values (sfc cr) (create-surface (unsafe-fl+ flwidth line-width) (unsafe-fl+ flheight line-width) density #true))
    (define inset (unsafe-fl* line-width 0.5))
    
    (cairo_path cr footprints (unsafe-fl+ dx inset) (unsafe-fl+ dy inset))
    (when (and close?) (cairo_close_path cr))
    
    (cairo-render cr stroke #false)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_polygon create-surface flwidth flheight footprints dx dy stroke background fill-rule density)
    (define line-width (~bdwidth stroke))
    (define-values (sfc cr) (create-surface (unsafe-fl+ flwidth line-width) (unsafe-fl+ flheight line-width) density #true))
    (define inset (unsafe-fl* line-width 0.5))
    
    (cairo_path cr footprints (unsafe-fl+ dx inset) (unsafe-fl+ dy inset))  
    (cairo_close_path cr)
    (cairo-render cr stroke background fill-rule)
    (cairo_destroy cr)
    
    sfc)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (cairo_elliptical_arc cr path:arc dx dy)
    (define center (unsafe-struct*-ref path:arc 0))
    (define cx (unsafe-fl+ (unsafe-flreal-part center) dx))
    (define cy (unsafe-fl+ (unsafe-flimag-part center) dy))
    (define rx (unsafe-struct*-ref path:arc 1))
    (define ry (unsafe-struct*-ref path:arc 2))
    (define rstart (unsafe-struct*-ref path:arc 3))
    (define rend (unsafe-struct*-ref path:arc 4))
    (define cairo-arc (if (unsafe-struct*-ref path:arc 5) cairo_arc cairo_arc_negative))
    
    (cairo-smart-elliptical-arc cr cx cy rx ry rstart rend cairo-arc))
  
  ;;; https://pomax.github.io/bezierinfo/
  ; ctrl1 = (+ cpt 2/3(ctrl - cpt)) = (+ cpt ctrl ctrl)/3
  ; ctrl2 = (+ ept 2/3(ctrl - ept)) = (+ ept ctrl ctrl)/3
  (define (cairo_quadratic_bezier cr cpt ctrl ept dx dy)
    (define 2ctrl (+ ctrl ctrl))
    (define coefficient (real->double-flonum 1/3))
    
    (cairo_cubic_bezier cr (* (+ cpt 2ctrl) coefficient) (* (+ ept 2ctrl) coefficient) ept dx dy))

  (define (cairo_cubic_bezier cr ctrl1 ctrl2 endpt dx dy)
    (cairo_curve_to cr
                    (unsafe-fl+ (unsafe-flreal-part ctrl1) dx) (unsafe-fl+ (unsafe-flimag-part ctrl1) dy)
                    (unsafe-fl+ (unsafe-flreal-part ctrl2) dx) (unsafe-fl+ (unsafe-flimag-part ctrl2) dy)
                    (unsafe-fl+ (unsafe-flreal-part endpt) dx) (unsafe-fl+ (unsafe-flimag-part endpt) dy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [path_stamp (-> (Listof Geo-Path-Print) Flonum Flonum (Option Paint) (Option Fill-Source) Symbol Positive-Flonum Abstract-Surface)]
 
 [dc_polyline (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Listof Geo-Path-Print) Flonum Flonum Paint Boolean Flonum S))]
 [dc_polygon (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Listof Geo-Path-Print) Flonum Flonum
                          (Option Paint) (Option Fill-Source) Symbol Flonum
                          S))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Path-Print (Pairof Char (U Float-Complex Path-Args False)))

(struct path-args () #:type-name Path-Args #:transparent)
(struct arc path-args ([center : Float-Complex] [rx : Float] [ry : Float] [start : Float] [end : Float] [clockwise? : Boolean]) #:transparent)
(struct bezier path-args ([ctrl1 : Float-Complex] [ctrl2 : Float-Complex] [end : Float-Complex]) #:transparent)
