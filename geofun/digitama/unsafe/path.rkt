#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")
(require "../geometry/footprint.rkt")

(require "source.rkt")
(require "visual/ctype.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/case)
  
  (require "paint.rkt")
  (require "pangocairo.rkt")
  (require "surface/abstract.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (path_stamp footprints dx dy stroke fill-color fill-rule density)
    (define thickness (~bdwidth stroke))
    (define inset (unsafe-fl* thickness 0.5))
    
    (make-cairo-abstract-surface 0.0 0.0 density #true
                                 (λ [cr inf.w inf.h]
                                   (cairo_path cr footprints (unsafe-fl+ dx inset) (unsafe-fl+ dy inset))
                                   (cairo-render cr stroke fill-color fill-rule))))

  (define (dc_polyline create-surface flwidth flheight footprints dx dy x-stroke? y-stroke? stroke close? density)
    (define-values (flw flh tx ty) (dc-path-window flwidth flheight dx dy stroke x-stroke? y-stroke?))
    (define-values (sfc cr) (create-surface flw flh density #true))
    
    (cairo_path cr footprints tx ty)
    (when (and close?) (cairo_close_path cr))
    
    (cairo-render cr stroke #false)
    (cairo_destroy cr)
    
    sfc)

  (define (dc_polygon create-surface flwidth flheight footprints dx dy x-stroke? y-stroke? stroke background fill-rule density)
    (define-values (flw flh tx ty) (dc-path-window flwidth flheight dx dy stroke x-stroke? y-stroke?))
    (define-values (sfc cr) (create-surface flw flh density #true))

    (cairo_path cr footprints tx ty)  
    (cairo_close_path cr)
    (cairo-render cr stroke background fill-rule)
    (cairo_destroy cr)
    
    sfc)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (cairo_path cr footprints dx dy)
    (cairo_translate cr dx dy)
    
    (let draw_path ([anchors footprints])
      (when (pair? anchors)
        (define op+footprint (unsafe-car anchors))
        (define footprint (unsafe-cdr op+footprint))
        (case/eq (unsafe-car op+footprint)
          [(#\M) (cairo_move_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))]
          [(#\L) (cairo_line_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))]
          [(#\A) (cairo_elliptical_arc cr footprint)]
          [(#\C) (cairo_cubic_bezier cr (unsafe-struct*-ref footprint 1) (unsafe-struct*-ref footprint 2) (unsafe-struct*-ref footprint 0))]
          [(#\Q) (cairo_quadratic_bezier cr (unsafe-struct*-ref footprint 1) (unsafe-struct*-ref footprint 2) (unsafe-struct*-ref footprint 0))]
          [(#\Z #\z) (cairo_close_path cr)]
          [(#\m) (cairo_rel_move_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))]
          [(#\l) (cairo_rel_line_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))])

        (draw_path (unsafe-cdr anchors)))))

  (define (cairo_clean_path cr footprints dx dy src-adjust tgt-adjust)
    (cairo_translate cr dx dy)

    (define source-adjusted-footprints
      (if (and src-adjust (pair? footprints))
          (let ([source (unsafe-car footprints)])
            (case/eq (unsafe-car source)
              [(#\M) (cons (cons #\M (+ (unsafe-cdr source) src-adjust)) (unsafe-cdr footprints))]
              [(#\L) (cons (cons #\L (+ (unsafe-cdr source) src-adjust)) (unsafe-cdr footprints))]
              [else footprints]))
          footprints))
    
    (let draw_clean_path ([anchors source-adjusted-footprints])
      (when (pair? anchors)
        (define op+footprint (unsafe-car anchors))
        (define rest (unsafe-cdr anchors))
        (define footprint (unsafe-cdr op+footprint))
        (case/eq (unsafe-car op+footprint)
          [(#\M) (cairo_move_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint))]
          [(#\L)
           (if (and (null? rest) tgt-adjust)
               (cairo_line_to cr
                              (unsafe-fl- (unsafe-flreal-part footprint) (unsafe-flreal-part tgt-adjust))
                              (unsafe-fl- (unsafe-flimag-part footprint) (unsafe-flimag-part tgt-adjust)))
               (cairo_line_to cr (unsafe-flreal-part footprint) (unsafe-flimag-part footprint)))]
          [(#\A) (cairo_elliptical_arc cr footprint)]
          [(#\C) (cairo_cubic_bezier cr (unsafe-struct*-ref footprint 1) (unsafe-struct*-ref footprint 2) (unsafe-struct*-ref footprint 0))]
          [(#\Q) (cairo_quadratic_bezier cr (unsafe-struct*-ref footprint 1) (unsafe-struct*-ref footprint 2) (unsafe-struct*-ref footprint 0))])

        (draw_clean_path rest))))
  
  (define (cairo_elliptical_arc cr gpath:arc)
    (define center (unsafe-struct*-ref gpath:arc 1))
    (define cx (unsafe-flreal-part center))
    (define cy (unsafe-flimag-part center))
    (define rx (unsafe-struct*-ref gpath:arc 2))
    (define ry (unsafe-struct*-ref gpath:arc 3))
    (define rstart (unsafe-struct*-ref gpath:arc 4))
    (define rend (unsafe-struct*-ref gpath:arc 5))
    (define cairo-arc (if (unsafe-struct*-ref gpath:arc 6) cairo_arc cairo_arc_negative))
    
    (cairo-smart-elliptical-arc cr cx cy rx ry rstart rend cairo-arc))
  
  ;;; https://pomax.github.io/bezierinfo/
  ; ctrl1 = (+ cpt 2/3(ctrl - cpt)) = (+ cpt ctrl ctrl)/3
  ; ctrl2 = (+ ept 2/3(ctrl - ept)) = (+ ept ctrl ctrl)/3
  (define (cairo_quadratic_bezier cr cpt ctrl ept)
    (define 2ctrl (+ ctrl ctrl))
    (define coefficient (real->double-flonum 1/3))
    
    (cairo_cubic_bezier cr (* (+ cpt 2ctrl) coefficient) (* (+ ept 2ctrl) coefficient) ept))

  (define (cairo_cubic_bezier cr ctrl1 ctrl2 endpt)
    (cairo_curve_to cr
                    (unsafe-flreal-part ctrl1) (unsafe-flimag-part ctrl1)
                    (unsafe-flreal-part ctrl2) (unsafe-flimag-part ctrl2)
                    (unsafe-flreal-part endpt) (unsafe-flimag-part endpt)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc-path-window flw flh dx dy stroke x-stroke? y-stroke?)
    (if (or x-stroke? y-stroke?)
        (let* ([thickness (~bdwidth stroke)]
               [inset (unsafe-fl* thickness 0.5)])
          (values (if (and x-stroke?) (unsafe-fl+ flw thickness) flw)
                  (if (and y-stroke?) (unsafe-fl+ flh thickness) flh)
                  (if (and x-stroke?) (unsafe-fl+ dx inset) dx)
                  (if (and y-stroke?) (unsafe-fl+ dy inset) dy)))
        (values flw flh dx dy))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [path_stamp (-> (Listof Geo-Path-Print) Flonum Flonum (Option Paint) (Option Fill-Source) Symbol Positive-Flonum Abstract-Surface)]

 [dc_polyline
  (All (S) (-> (Cairo-Surface-Create S)
               Nonnegative-Flonum Nonnegative-Flonum (Listof Geo-Path-Print) Flonum Flonum Boolean Boolean
               Paint Boolean Positive-Flonum
               S))]
 
 [dc_polygon
  (All (S) (-> (Cairo-Surface-Create S)
               Nonnegative-Flonum Nonnegative-Flonum (Listof Geo-Path-Print) Flonum Flonum Boolean Boolean
               (Option Paint) (Option Fill-Source) Symbol Positive-Flonum
               S))])
