#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")
(require "convert.rkt")
(require "source.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require "paint.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_crawl! src footprints stroke fill-color dx dy fill-style density)
    (define cr (cairo_create src))
    (define-values (x1 y1 x2 y2) (cairo_path cr footprints dx dy fill-style density))

    (cairo-render cr stroke fill-color)
    (cairo_destroy cr)
    (values (make-rectangular x1 y1)
            (make-rectangular x2 y2)))

  (define (bitmap_crawl footprints flwidth flheight dx dy stroke fill-color fill-style density)
    (define line-width (if (struct? stroke) (unsafe-struct-ref stroke 1) 0.0))
    (define inset (unsafe-fl* line-width 0.5))
    (define-values (img cr) (make-cairo-image (unsafe-fl+ flwidth line-width) (unsafe-fl+ flheight line-width) density #false))
    (define-values (x1 y1 x2 y2) (cairo_path cr footprints (unsafe-fl+ dx inset) (unsafe-fl+ dy inset) fill-style density))

    (cairo-render cr stroke fill-color)
    (cairo_destroy cr)
    (values img (make-rectangular x1 y1) (make-rectangular x2 y2)))

  (define (cairo_path cr footprints dx dy fill-style density)
    (cairo_scale cr density density)
    
    (for ([op+footprint (in-list (reverse footprints))])
      (define footprint (unsafe-cdr op+footprint))
      (case (unsafe-car op+footprint)
        [(#\M) (cairo_move_to cr (unsafe-fl+ (unsafe-flreal-part footprint) dx) (unsafe-fl+ (unsafe-flimag-part footprint) dy))]
        [(#\m) (cairo_rel_move_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))]
        [(#\L) (cairo_line_to cr (unsafe-fl+ (unsafe-flreal-part footprint) dx) (unsafe-fl+ (unsafe-flimag-part footprint) dy))]
        [(#\l) (cairo_rel_line_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))]
        [(#\A) (cairo_elliptical_arc cr footprint #true dx dy density)]
        [(#\C) (cairo_cubic_bezier cr (unsafe-struct*-ref footprint 0) (unsafe-struct*-ref footprint 1) (unsafe-struct*-ref footprint 2) dx dy)]
        [(#\Q) (cairo_quadratic_bezier cr (unsafe-struct*-ref footprint 0) (unsafe-struct*-ref footprint 1) (unsafe-struct*-ref footprint 2) dx dy)]
        [(#\Z #\z) (cairo_close_path cr)]))

    (unless (eq? fill-style 'winding)
      (cairo_set_fill_rule cr CAIRO_FILL_RULE_EVEN_ODD))
    
    (cairo_path_extents cr))

  (define (cairo_elliptical_arc cr path:arc radian? dx dy density)
    (define center (unsafe-struct*-ref path:arc 0))
    (define cx (unsafe-fl+ (unsafe-flreal-part center) dx))
    (define cy (unsafe-fl+ (unsafe-flimag-part center) dy))
    (define rx (unsafe-struct*-ref path:arc 1))
    (define ry (unsafe-struct*-ref path:arc 2))
    (define start (unsafe-struct*-ref path:arc 3))
    (define end (unsafe-struct*-ref path:arc 4))
    (define cairo-arc (if (unsafe-struct*-ref path:arc 5) cairo_arc cairo_arc_negative))
    (define-values (rstart rend) (if radian? (values start end) (values (~radian start) (~radian end))))

    ;;; WARNING
    ;; For drawing elliptical arcs
    ;;   `cairo_translate` is necessary,
    ;;   or the resulting shapes will be weird.
    ;; TODO: find the reason;
    ;; TODO: why not `density` should be used to scale
    
    (cond [(unsafe-fl< rx ry)
           (cairo_save cr)
           (cairo_translate cr cx cy)
           (cairo_scale cr 1.0 (unsafe-fl/ ry rx))
           (cairo-arc cr 0.0 0.0 rx rstart rend)
           (cairo_restore cr)]
          [(unsafe-fl> rx ry)
           (cairo_save cr)
           (cairo_translate cr cx cy)
           (cairo_scale cr (unsafe-fl/ rx ry) 1.0)
           (cairo-arc cr 0.0 0.0 ry rstart rend)
           (cairo_restore cr)]
          [else ; no need to `translate` first for circles
           (cairo-arc cr cx cy rx rstart rend)]))
  
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
 [bitmap_crawl! (-> Bitmap-Surface (Listof Track-Print) (Option Paint) (Option Bitmap-Source) Flonum Flonum Symbol Flonum
                    (Values Float-Complex Float-Complex))]
 [bitmap_crawl (-> (Listof Track-Print) Flonum Flonum Flonum Flonum (Option Paint) (Option Bitmap-Source) Symbol Flonum
                   (Values Bitmap Float-Complex Float-Complex))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Track-Print (Pairof Char (U Float-Complex Path-Args False)))

(struct path-args () #:type-name Path-Args #:transparent)
(struct arc path-args ([center : Float-Complex] [rx : Float] [ry : Float] [start : Float] [end : Float] [clockwise? : Boolean]) #:transparent)
(struct bezier path-args ([ctrl1 : Float-Complex] [ctrl2 : Float-Complex] [end : Float-Complex]) #:transparent)
