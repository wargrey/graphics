#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../../base.rkt")
(require "../source.rkt")
(require "../visual/ctype.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "../../geometry/constants.rkt")
  (require "../pangocairo.rkt")
  (require "../paint.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_stadium create-surface fllength flradius border background density)
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ fllength flheight))
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset-radius (unsafe-fl- flradius (unsafe-fl* line-width 0.5)))

    (cairo_new_path cr)
    (cairo_arc_negative cr flradius                       flradius inset-radius -pi/2 pi/2)
    (cairo_arc_negative cr (unsafe-fl+ flradius fllength) flradius inset-radius pi/2  3pi/2)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_half_stadium create-surface fllength flradius border background density)
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ fllength flradius))
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define inset-radius (unsafe-fl- flradius inset))
    (define by (unsafe-fl- flheight inset))

    (cairo_new_path cr)
    (cairo_arc cr fllength flradius inset-radius -pi/2 pi/2)
    (cairo_line_to cr inset by)
    (cairo_line_to cr inset inset)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_bullet create-surface flogive flbarrel flradius border background density)
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ flogive flbarrel))
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define line-width (~bdwidth border))
    (define inset (unsafe-fl* line-width 0.5))
    (define tail-radius (unsafe-fl* flradius 0.384))
    (define inset-aradius (unsafe-fl- tail-radius inset))
    (define inset-bradius (unsafe-fl- flradius inset))
    (define lcx flogive)
    (define rcx (unsafe-fl- flwidth tail-radius))
    (define ctrlx (unsafe-fl* flogive 0.5))
    (define by (unsafe-fl- flheight inset))

    (cairo_new_path cr)
    (cairo-smart-elliptical-arc cr rcx flradius inset-aradius inset-bradius -pi/2 pi/2 cairo_arc)
    (cairo_line_to cr lcx by)
    (cairo_curve_to cr ctrlx by ctrlx by inset flradius)
    (cairo_curve_to cr ctrlx inset ctrlx inset lcx inset)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc)

  (define (dc_sandglass create-surface flwidth flheight neck-width neck-height tube-height border background density)
    (define-values (sfc cr) (create-surface flwidth flheight density #true))
    (define-values (cy neck-a neck-b) (values (unsafe-fl* flheight 0.5) (unsafe-fl* neck-width 0.25) (unsafe-fl* neck-height 0.5)))
    (define line-width (~bdwidth border))
    (define bulb-a (unsafe-fl* (unsafe-fl- (unsafe-fl- flwidth line-width) neck-width) 0.5))
    (define bulb-b (unsafe-fl* (unsafe-fl- (unsafe-fl- flheight line-width) (unsafe-fl+ (unsafe-fl* tube-height 2.0) neck-height)) 0.5))
    (define tlset (unsafe-fl* line-width 0.5))
    (define-values (xrset ybset) (values (unsafe-fl- flwidth tlset) (unsafe-fl- flheight tlset)))
    (define-values (bulb-ty bulb-by) (values (unsafe-fl+ tlset tube-height) (unsafe-fl- ybset tube-height)))
    (define-values (neck-lx neck-rx) (values (unsafe-fl+ tlset bulb-a) (unsafe-fl- xrset bulb-a)))
    (define-values (neck-ty neck-by) (values (unsafe-fl+ bulb-ty bulb-b) (unsafe-fl- bulb-by bulb-b)))
    (define no-neck? (or (unsafe-fl= neck-a 0.0) (unsafe-fl= neck-b 0.0)))
    
    (cairo_new_path cr)
    (cairo_move_to cr tlset tlset)
    
    (cairo_line_to cr xrset tlset)
    (cairo_line_to cr xrset bulb-ty)
    (cairo-smart-arc cr neck-rx bulb-ty bulb-a bulb-b 0.0 pi/2)
    (when (not no-neck?)
      (cairo-smart-arc-negative cr neck-rx cy neck-a neck-b 3pi/2 pi/2))
    (cairo-smart-arc cr neck-rx bulb-by bulb-a bulb-b (- pi/2) 0.0)
    (cairo_line_to cr xrset ybset)
    (cairo_line_to cr tlset ybset)
    (cairo_line_to cr tlset bulb-by)
    (cairo-smart-arc cr neck-lx bulb-by bulb-a bulb-b pi 3pi/2)
    (when (not no-neck?)
      (cairo-smart-arc-negative cr neck-lx cy neck-a neck-b pi/2 (- pi/2)))
    (cairo-smart-arc cr neck-lx bulb-ty bulb-a bulb-b pi/2 pi)
    (cairo_close_path cr)
    
    (cairo-render cr border background)
    (cairo_destroy cr)

    sfc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_half_stadium (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Positive-Flonum S))]
 [dc_stadium (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Positive-Flonum S))]
 [dc_bullet (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Paint) (Option Fill-Source) Positive-Flonum S))]
 
 [dc_sandglass (All (S) (-> (Cairo-Surface-Create S) Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                            (Option Paint) (Option Fill-Source) Positive-Flonum
                            S))])
