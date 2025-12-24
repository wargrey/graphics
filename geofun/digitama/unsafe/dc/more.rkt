#lang typed/racket/base

(provide (all-defined-out))

(require digimon/constant)

(require "../paint.rkt")
(require "../typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_stadium : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight stroke background]
    (define flradius (* flheight 0.5))
    (define fllength (- flwidth flheight))
    (define lx (+ x0 flradius))
    (define rx (+ lx fllength))
    (define cy (+ y0 flradius))
    
    (cairo_new_path cr)
    (cairo_arc_negative cr lx cy flradius -pi/2  pi/2)
    (cairo_arc_negative cr rx cy flradius  pi/2 3pi/2)
    (cairo_close_path cr)
    (cairo-render cr stroke background)))

(define dc_half_stadium : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) Boolean Any)
  (lambda [cr x0 y0 flwidth flheight stroke background left?]
    (define flradius (* flheight 0.5))
    (define fllength (- flwidth flradius))
    (define cy (+ y0 flradius))

    (cairo_new_path cr)

    (if (or left?)
        (let ([cx (+ x0 flradius)])
          (cairo_arc_negative cr cx cy flradius -pi/2 pi/2)
          (cairo_line_to     cr (+ x0 flwidth) (+ y0 flheight))
          (cairo_rel_line_to cr 0.0            (- flheight)))
        (let ([cx (+ x0 fllength)])
          (cairo_arc cr cx cy flradius -pi/2 pi/2)
          (cairo_line_to cr x0 (+ y0 flheight))
          (cairo_line_to cr x0 y0)))
    
    (cairo_close_path cr)
    (cairo-render cr stroke background)))

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
(define dc_document : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                          Nonnegative-Flonum Nonnegative-Flonum Index (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight flhwave gapsize extra-n stroke background]
    (define ngap (* (real->double-flonum extra-n) gapsize))
    (define ctrl-xoff (* (- flwidth ngap) 0.25))
    (define ctrl-yoff (* flhwave 2.5)) ;; TODO, calculate the exact position

    (let draw-docs ([lx (+ ngap x0)]
                    [rx (+ flwidth x0)]
                    [ty y0]
                    [by (+ y0 (- flheight ngap flhwave))]
                    [i : Nonnegative-Fixnum 0])
      (when (<= i extra-n)
        (define ctrl-x1 (+ lx ctrl-xoff))
        (define ctrl-y1 (+ by ctrl-yoff))
        (define ctrl-x2 (- rx ctrl-xoff))
        (define ctrl-y2 (- by ctrl-yoff))
        
        (cairo_new_path cr)
        (cairo_move_to cr lx ty)
        (cairo_line_to cr lx by)
        (cairo_curve_to cr ctrl-x1 ctrl-y1 ctrl-x2 ctrl-y2 rx by)
        (cairo_line_to cr rx ty)
        (cairo_close_path cr)
        (cairo-render cr stroke background)
        
        (draw-docs (- lx gapsize) (- rx gapsize)
                   (+ ty gapsize) (+ by gapsize)
                   (+ i 1))))))
  
(define dc_database : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                          Nonnegative-Flonum Nonnegative-Flonum Index (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight bradius gapsize extra-n stroke background]
    (define rx (+ x0 flwidth))
    (define ty (+ y0 bradius))
    (define by (+ y0 (- flheight bradius)))
    (define aradius (* flwidth 0.5))
    (define cx (+ x0 aradius))
    
    (cairo_new_path cr)
    (cairo_move_to cr x0 ty)
    (cairo_line_to cr x0 by)
    (cairo-negative-arc cr cx by aradius bradius pi 0.0)
    (cairo_line_to cr rx bradius)
    (cairo-negative-arc cr cx ty aradius bradius 0.0 pi)
    (cairo_close_path cr)

    (let draw-sides ([y : Flonum ty]
                     [i : Nonnegative-Fixnum 0])
      (when (<= i extra-n)
        (cairo_move_to cr x0 y)
        (cairo-negative-arc cr cx y aradius bradius pi 0.0)
        (draw-sides (+ y gapsize) (+ i 1))))
    
    (cairo-render cr stroke background)))

(define dc_general_storage : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight aradius stroke background]
    (define bradius (* flheight 0.5))
    (define cy (+ y0 bradius))
    
    (cairo_new_path cr)
    (cairo-negative-arc cr (+ x0 aradius) cy aradius bradius -pi/2  pi/2)
    (cairo-positive-arc cr (+ x0 flwidth) cy aradius bradius  pi/2 3pi/2)
    (cairo_close_path cr)
    (cairo-render cr stroke background)))
