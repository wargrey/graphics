#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require "../paint.rkt")
(require "../typed/cairo.rkt")
(require "../typed/more.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_bullet : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight flogive stroke background]
    (define flradius (* flheight 0.5))
    (define aradius (* flradius 0.384))
    (define lcx (+ x0 flogive))
    (define rcx (+ x0 (- flwidth aradius)))
    (define ctrlx (+ x0 (* flogive 0.5)))
    (define cy (+ y0 flradius))
    (define by (+ y0 flheight))

    (cairo_new_path cr)
    (cairo-positive-arc cr rcx cy aradius flradius -pi/2 pi/2)
    (cairo_line_to cr lcx by)
    (cairo_curve_to cr ctrlx by ctrlx by  x0 cy)
    (cairo_curve_to cr ctrlx y0 ctrlx y0 lcx y0)
    (cairo_close_path cr)
    (cairo-render cr stroke background)))

(define dc_sandglass : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                           Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                           (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight neck-width neck-height tube-height stroke background]
    (define-values (cy neck-a neck-b) (values (+ y0 (* flheight 0.5)) (* neck-width 0.25) (* neck-height 0.5)))
    (define bulb-a (* (max 0.0 (- flwidth neck-width)) 0.5))
    (define bulb-b (* (max 0.0 (- flheight tube-height tube-height neck-height)) 0.5))
    (define-values (xlset ytset) (values x0 y0))
    (define-values (xrset ybset) (values (+ x0 flwidth) (+ y0 flheight)))
    (define-values (neck-lx neck-rx) (values (+ xlset bulb-a) (- xrset bulb-a)))
    (define-values (bulb-ty bulb-by) (values (+ ytset tube-height) (- ybset tube-height)))
    (define-values (neck-ty neck-by) (values (+ bulb-ty bulb-b) (- bulb-by bulb-b)))
    (define no-neck? (or (= neck-a 0.0) (= neck-b 0.0)))
    
    (cairo_new_path cr)
    (cairo_move_to cr xlset ytset)
    
    (cairo_line_to cr xrset ytset)
    (cairo_line_to cr xrset bulb-ty)
    (cairo-positive-arc cr neck-rx bulb-ty bulb-a bulb-b 0.0 pi/2)
    (when (not no-neck?)
      (cairo-negative-arc cr neck-rx cy neck-a neck-b 3pi/2 pi/2))
    (cairo-positive-arc cr neck-rx bulb-by bulb-a bulb-b -pi/2 0.0)
    (cairo_line_to cr xrset ybset)
    (cairo_line_to cr xlset ybset)
    (cairo_line_to cr xlset bulb-by)
    (cairo-positive-arc cr neck-lx bulb-by bulb-a bulb-b pi 3pi/2)
    (when (not no-neck?)
      (cairo-negative-arc cr neck-lx cy neck-a neck-b pi/2 -pi/2))
    (cairo-positive-arc cr neck-lx bulb-ty bulb-a bulb-b pi/2 pi)
    (cairo_close_path cr)
    
    (cairo-render cr stroke background)))

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
