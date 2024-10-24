#lang typed/racket/base

(provide (all-defined-out))

(require "../../base.rkt")
(require "../../geometry/footprint.rkt")

(require "../paint.rkt")
(require "../source.rkt")

(require "../typed/cairo.rkt")
(require "../visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_path : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Geo-Path-Prints (Option Paint) (Option Fill-Source) Fill-Rule Any)
  (lambda [cr x0 y0 width height footprints stroke fill-color fill-rule]
    (cairo_path cr footprints x0 y0)
    (cairo-render cr stroke fill-color fill-rule)))

(define dc_polyline : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Listof Float-Complex) Paint Boolean Any)
  (lambda [cr x0 y0 flwidth flheight vertices stroke close?]
    (cairo_new_path cr)
    (cairo_simple_path cr vertices x0 y0)

    (when (and close?)
      (cairo_close_path cr))
    
    (cairo-render cr stroke #false)))

(define dc_polygon : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Listof Float-Complex) (Option Paint) (Option Fill-Source) Fill-Rule Any)
  (lambda [cr x0 y0 flwidth flheight vertices stroke background fill-rule]
    (cairo_new_path cr)
    (cairo_simple_path cr vertices x0 y0)
    (cairo_close_path cr)
    (cairo-render cr stroke background fill-rule)))


(define dc_polyline* : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Geo-Path-Prints Paint Boolean Any)
  (lambda [cr x0 y0 flwidth flheight footprints stroke close?]
    (cairo_new_path cr)
    (cairo_path cr footprints x0 y0)

    (when (and close?)
      (cairo_close_path cr))
    
    (cairo-render cr stroke #false)))

(define dc_polygon* : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Geo-Path-Prints (Option Paint) (Option Fill-Source) Fill-Rule Any)
  (lambda [cr x0 y0 flwidth flheight footprints stroke background fill-rule]
    (cairo_new_path cr)
    (cairo_path cr footprints x0 y0)
    (cairo_close_path cr)
    (cairo-render cr stroke background fill-rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo_simple_path : (-> Cairo-Ctx (Listof Float-Complex) Flonum Flonum Void)
  (lambda [cr vertices dx dy]
    (when (pair? vertices)
      (cairo_translate cr dx dy)
      (cairo_move_to cr (real-part (car vertices)) (imag-part (car vertices)))
      
      (let draw_path : Void ([prints (cdr vertices)])
        (when (pair? prints)
          (define self (car prints))
          
          (cairo_line_to cr (real-part self) (imag-part self))
          (draw_path (cdr prints)))))))

(define cairo_path : (-> Cairo-Ctx Geo-Path-Prints Flonum Flonum Void)
  (lambda [cr footprints dx dy]
    (cairo_translate cr dx dy)
    
    (let draw_path : Void ([prints footprints])
      (when (pair? prints)
        (define self (car prints))

        (cond [(gpp:point? self)
               (let ([cmd (gpath:datum-cmd self)]
                     [pt (gpath:print-end-here self)])
                 (cond [(eq? cmd #\L) (cairo_line_to cr (real-part pt) (imag-part pt))]
                       [(eq? cmd #\M) (cairo_move_to cr (real-part pt) (imag-part pt))]))]
              [(gpp:arc? self) #\A (cairo_elliptical_arc cr self)]
              [(gpp:bezier? self)
               (let ([cmd (gpath:datum-cmd self)]
                     [endpt (gpath:print-end-here self)])
                 (cond [(eq? cmd #\C) (cairo_cubic_bezier     cr (gpp:bezier-ctrl1 self) (gpp:bezier-ctrl2 self) endpt)]
                       [(eq? cmd #\Q) (cairo_quadratic_bezier cr (gpp:bezier-ctrl1 self) (gpp:bezier-ctrl2 self) endpt)]))]
              [(gpp:vector? self)
               (let ([cmd (gpath:datum-cmd self)]
                     [pt (gpath:print-end-here self)])
                 (cond [(eq? cmd #\l) (cairo_rel_line_to cr (real-part pt) (imag-part pt))]
                       [(eq? cmd #\m) (cairo_rel_move_to cr (real-part pt) (imag-part pt))]))]
              [(gpp:close? self) #\Z #\z (cairo_close_path cr)])

        (draw_path (cdr prints))))))

(define cairo_clean_path : (-> Cairo-Ctx Geo-Path-Clean-Prints Flonum Flonum (Option Float-Complex) (Option Float-Complex) Void)
  (lambda [cr footprints dx dy src-adjust tgt-adjust]
    (cairo_translate cr dx dy)

    (define source-adjusted-footprints
      (if (and src-adjust (pair? footprints))
          (let ([source (car footprints)])
            (cond [(gpp:point? source)
                   (let ([pt (+ (gpath:print-end-here source) src-adjust)])
                     (cons (gpp:point (gpath:datum-cmd source) pt) (cdr footprints)))]
                  [else footprints]))
          footprints))
    
    (let draw_clean_path : Void ([prints source-adjusted-footprints])
      (when (pair? prints)
        (define-values (self rest) (values (car prints) (cdr prints)))

        (cond [(gpp:point? self)
               (let ([cmd (gpath:datum-cmd self)]
                     [pt (gpath:print-end-here self)])
                 (if (and tgt-adjust (null? rest))
                     (let ([apt (- pt tgt-adjust)])
                       (cairo_line_to cr (real-part apt) (imag-part apt)))
                     (cairo_line_to cr (real-part pt) (imag-part pt))))]
              [(gpp:arc? self) #\A (cairo_elliptical_arc cr self)]
              [(gpp:bezier? self)
               (let ([cmd (gpath:datum-cmd self)]
                     [endpt (gpath:print-end-here self)])
                 (cond [(eq? cmd #\C) (cairo_cubic_bezier     cr (gpp:bezier-ctrl1 self) (gpp:bezier-ctrl2 self) endpt)]
                       [(eq? cmd #\Q) (cairo_quadratic_bezier cr (gpp:bezier-ctrl1 self) (gpp:bezier-ctrl2 self) endpt)]))])

        (draw_clean_path rest)))))
  
(define cairo_elliptical_arc : (-> Cairo-Ctx GPP:Arc Void)
  (lambda [cr gpath:arc]
    (define center (gpp:arc-center gpath:arc))
    (define cx (real-part center))
    (define cy (imag-part center))
    (define rx (gpp:arc-rx gpath:arc))
    (define ry (gpp:arc-ry gpath:arc))
    (define rstart (gpp:arc-start gpath:arc))
    (define rend (gpp:arc-end gpath:arc))

    (if (gpp:arc-clockwise? gpath:arc)
        (cairo-positive-arc cr cx cy rx ry rstart rend)
        (cairo-negative-arc cr cx cy rx ry rstart rend))))
  
  ;;; https://pomax.github.io/bezierinfo/
  ; ctrl1 = (+ cpt 2/3(ctrl - cpt)) = (+ cpt ctrl ctrl)/3
  ; ctrl2 = (+ ept 2/3(ctrl - ept)) = (+ ept ctrl ctrl)/3
(define cairo_quadratic_bezier : (-> Cairo-Ctx Float-Complex Float-Complex Float-Complex Void)
  (lambda [cr cpt ctrl ept]
    (define 2ctrl (+ ctrl ctrl))
    (define coefficient (real->double-flonum 1/3))
    
    (cairo_cubic_bezier cr (* (+ cpt 2ctrl) coefficient) (* (+ ept 2ctrl) coefficient) ept)))

(define cairo_cubic_bezier : (-> Cairo-Ctx Float-Complex Float-Complex Float-Complex Void)
  (lambda [cr ctrl1 ctrl2 endpt]
    (cairo_curve_to cr
                    (real-part ctrl1) (imag-part ctrl1)
                    (real-part ctrl2) (imag-part ctrl2)
                    (real-part endpt) (imag-part endpt))))
  