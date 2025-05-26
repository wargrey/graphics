#lang typed/racket/base

(provide (all-defined-out))

(require "../paint.rkt")
(require "../source.rkt")
(require "../typed/cairo.rkt")

(require "../../paint/self.rkt")
(require "../../geometry/radius.rkt")
(require "../../geometry/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_line : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum Flonum Stroke Any)
  (lambda [cr x0 y0 width height x y dx dy stroke]
    (cairo_new_path cr)
    
    (cairo_move_to     cr (+ x0 x) (+ y0 y))
    (cairo_rel_line_to cr dx       dy)
    
    (cairo-render cr stroke #false)))

(define dc_ellipse : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Stroke) (Option Fill-Source) (Listof Flonum) Any)
  (lambda [cr x0 y0 flwidth flheight stroke background diameters]
    (define-values (aradius bradius) (values (* flwidth 0.5) (* flheight 0.5)))
    
    (cairo_translate cr (+ x0 aradius) (+ y0 bradius))
    (cairo-negative-arc cr aradius bradius (- 2pi) 0.0)
    
    (let draw-d ([as diameters])
      (when (pair? as)
        (define theta (car as))
        (define x (* aradius (cos theta)))
        (define y (* bradius (sin theta)))
        (cairo-add-line cr x y (- 0.0 x) (- 0.0 y))
        (draw-d (cdr as))))
    
    (cairo-render cr stroke background)))

(define dc_arc : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum (Option Stroke) (Option Fill-Source) Boolean Any)
  (lambda [cr x0 y0 flwidth flheight rstart rend stroke background sector?]
    (define-values (aradius bradius) (values (* flwidth 0.5) (* flheight 0.5)))

    (cairo_translate cr (+ x0 aradius) (+ y0 bradius))
    (cairo-positive-arc cr aradius bradius rstart rend)
    
    (when (and sector? (<= (abs (- rend rstart)) 2pi))
      (cairo_line_to cr 0.0 0.0)
      (cairo_close_path cr))
    
    (cairo-render cr stroke background)))

(define dc_rectangle : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Stroke) (Option Fill-Source) (Listof Flonum) (Listof Flonum) Any)
  (lambda [cr x0 y0 flwidth flheight stroke background vlines hlines]
    (cairo_rectangle cr x0 y0 flwidth flheight)
    
    (let draw-vl ([vls vlines])
      (when (pair? vls)
        (define x (+ (dc-line-position flwidth (car vls)) x0))
        (cairo-add-line cr x y0 x (+ y0 flheight))
        (draw-vl (cdr vls))))
    
    (let draw-hl ([hls hlines])
      (when (pair? hls)
        (define y (+ (dc-line-position flheight (car hls)) y0))
        (cairo-add-line cr x0 y (+ x0 flwidth) y)
        (draw-hl (cdr hls))))
    
    (cairo-render cr stroke background)))

(define dc_rounded_rectangle : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                   Nonnegative-Flonum (Option Stroke) (Option Fill-Source) (Listof Flonum) (Listof Flonum) Any)
  (lambda [cr x0 y0 flwidth flheight corner-radius stroke background vlines hlines]
    (define flradius (min corner-radius (* (min flwidth flheight) 0.5)))
    (define-values (xr yb) (values (+ x0 flwidth) (+ y0 flheight)))
    (define-values (xlset ytset) (values (+ x0 flradius) (+ y0 flradius)))
    (define-values (xrset ybset) (values (- xr flradius) (- yb flradius)))
    
    (cairo_new_path cr)
    (cairo_arc cr xrset ytset flradius -pi/2 0.0)
    (cairo_arc cr xrset ybset flradius 0.0   pi/2)
    (cairo_arc cr xlset ybset flradius pi/2  pi)
    (cairo_arc cr xlset ytset flradius pi    3pi/2)
    (cairo_close_path cr)
    
    (let draw-vl ([vls vlines])
      (when (pair? vls)
        (define x (+ x0 (dc-line-position flwidth (car vls))))
        
        (cond [(< x xlset)
               (let ([off (dc-line-rounded-length flradius (- xlset x))])
                 (cairo-add-line cr x (- ytset off) x (+ ybset off)))]
              [(> x xrset)
               (let ([off (dc-line-rounded-length flradius (- x xrset))])
                 (cairo-add-line cr x (- ytset off) x (+ ybset off)))]
              [else (cairo-add-line cr x y0 x yb)])
        
        (draw-vl (cdr vls))))
    
    (let draw-hl ([hls hlines])
      (when (pair? hls)
        (define y (+ y0 (dc-line-position flheight (car hls))))
        
        (cond [(< y ytset)
               (let ([off (dc-line-rounded-length flradius (- ytset y))])
                 (cairo-add-line cr (- xlset off) y (+ xrset off) y))]
              [(> y ybset)
               (let ([off (dc-line-rounded-length flradius (- y ybset))])
                 (cairo-add-line cr (- xlset off) y (+ xrset off) y))]
              [else (cairo-add-line cr x0 y xr y)])
        
        (draw-hl (cdr hls))))
    
    (cairo-render cr stroke background)))

(define dc_regular_polygon : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                 Positive-Index Positive-Index Flonum (Option Stroke) (Option Fill-Source)
                                 Any)
  (lambda [cr x0 y0 width height n k rotation stroke background]
    (define-values (aradius bradius) (values (* width 0.5) (* height 0.5)))
    (define fln (exact->inexact n))
    (define delta
      (if (= k 1)
          (/ 2pi fln)
          (/ (* 2pi (exact->inexact k)) fln)))
    
    (cairo_new_path cr)
    (cairo_translate cr (+ x0 aradius) (+ y0 bradius))
    
    (let draw-polygon ([flidx 0.0]
                       [theta rotation])
      (when (< flidx fln)
        (cairo_line_to cr (* aradius (cos theta)) (* bradius (sin theta)))
        (draw-polygon (+ flidx 1.0) (+ theta delta))))
    
    (cairo_close_path cr)
    (cairo-render cr stroke background)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define regular-polygon-radius->circumsphere-radius : (-> Positive-Index Nonnegative-Flonum 2D-Radius-Type Nonnegative-Flonum)
  (lambda [n R type]
    ; r = Rcos(pi/n)
    (cond [(eq? type 'vertex) R]
          [else (max (/ R (cos (/ pi (exact->inexact n)))) 0.0)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc-line-position : (-> Flonum Flonum Flonum)
  (lambda [length pos]
    (cond [(>= pos 1.0) pos]
          [(>= pos 0.0) (* length pos)]
          [(<= pos -1.0) (+ length pos)]
          [else (* length (+ 1.0 pos))])))

(define dc-line-rounded-length : (-> Flonum Flonum Flonum)
  (lambda [r d]
    (sqrt (max 0.0
               (- (* r r)
                  (* d d))))))
