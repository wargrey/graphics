#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

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
