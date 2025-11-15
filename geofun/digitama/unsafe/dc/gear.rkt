#lang typed/racket/base

(provide (all-defined-out))

(require "../paint.rkt")
(require "../typed/cairo.rkt")
(require "../typed/more.rkt")

(require "../../paint/self.rkt")
(require "../../geometry/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_gear : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                      Positive-Index Flonum Nonnegative-Flonum Nonnegative-Flonum
                      Nonnegative-Flonum Flonum (Option Pen) (Option Brush)
                      Any)
  (lambda [cr x0 y0 width height z pressure-angle root-ratio ref-ratio inner-ratio rotation pen brush]
    (define rTip (* width 0.5))
    (define rRoot (* rTip root-ratio))
    (define teeth-depth (- rTip rRoot))
    (define rRef (+ (* teeth-depth ref-ratio) rRoot))
    (define-values (addendum dedendum) (values (- rTip rRef) (- rRef rRoot)))

    (define flz (exact->inexact z))
    (define delta (/ 2pi flz))(define-values (delta/4 delta/2) (values (* delta 0.25) (* delta 0.50)))
    (define delta/tip  (atan (* addendum (tan pressure-angle)) rTip))
    (define delta/root (atan (* dedendum (tan (* pressure-angle 0.618))) rRef))
    
    (cairo_new_path cr)
    (cairo_translate cr (+ x0 (* width 0.5)) (+ y0 (* height 0.5)))
    (cairo_scale cr 1.0 (/ height width))
    
    (let draw-polygon ([flidx 0.0]
                       [theta rotation])
      (when (< flidx flz)
        (define-values (aRefl aRefr) (values (- theta delta/4) (+ theta delta/4)))
        (define-values (aTipl aTipr) (values (+ aRefl delta/tip) (- aRefr delta/tip)))
        (define-values (aRootl aRootr) (values (- aRefl delta/root) (+ aRefr delta/root)))
        (define next-theta (+ theta delta))
        (define aRootn (- next-theta delta/4 delta/root))

        (cairo_line_to cr (* rRoot (cos aRootl)) (* rRoot (sin aRootl)))
        (cairo_line_to cr (* rRef (cos aRefl)) (* rRef (sin aRefl)))
        (cairo_line_to cr (* rTip (cos aTipl)) (* rTip (sin aTipl)))
        (cairo_line_to cr (* rTip (cos aTipr)) (* rTip (sin aTipr)))
        (cairo_line_to cr (* rRef (cos aRefr)) (* rRef (sin aRefr)))
        (cairo_line_to cr (* rRoot (cos aRootr)) (* rRoot (sin aRootr)))
        (cairo_arc cr 0.0 0.0 rRoot aRootr aRootn)

        (draw-polygon (+ flidx 1.0) next-theta)))

    (cairo_close_path cr)

    (when (> inner-ratio 0.0)
      (cairo_new_sub_path cr)
      (cairo_arc cr 0.0 0.0 (* rTip inner-ratio) 0.0 2pi))

    (cairo_scale cr 1.0 (/ width height))
    (cairo-render/evenodd cr pen brush)))
