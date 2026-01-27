#lang typed/racket/base

(provide (all-defined-out))

(require "border.rkt")

(require "../paint.rkt")

(require "../typed/cairo.rkt")
(require "../typed/more.rkt")

(require "../../paint/self.rkt")
(require "../../geometry/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_line : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum Flonum Flonum Pen Any)
  (lambda [cr x0 y0 width height x y dx dy stroke]
    (cairo_move_to     cr (+ x0 x) (+ y0 y))
    (cairo_rel_line_to cr dx       dy)
    (cairo-render cr stroke #false)))

(define dc_ellipse : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) (Listof Flonum) Any)
  (lambda [cr x0 y0 flwidth flheight stroke fill diameters]
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
    
    (cairo-render cr stroke fill)))

(define dc_ring : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                      (Option Pen) (Option Brush) Nonnegative-Flonum
                      Any)
  (lambda [cr x0 y0 flwidth flheight stroke fill inner%]
    (define-values (oaradius obradius) (values (* flwidth 0.5) (* flheight 0.5)))
    (define-values (iaradius ibradius) (values (* oaradius inner%) (* obradius inner%)))
    
    (cairo_translate cr (+ x0 oaradius) (+ y0 obradius))

    (cairo-positive-arc cr oaradius obradius 0.0 2pi)
    (cairo_new_sub_path cr)
    (cairo-negative-arc cr iaradius ibradius -2pi 0.0)
    (cairo-render/evenodd cr stroke fill)))

(define dc_bullseye : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                          (Option Pen) (Option Brush) (Option Pen) (Option Brush)
                          Nonnegative-Flonum (Listof Nonnegative-Flonum)
                          Any)
  (lambda [cr x0 y0 flwidth flheight stroke fill eye-stroke eye-fill r% rings%]
    (define-values (oaradius obradius) (values (* flwidth 0.5) (* flheight 0.5)))
    (define-values (iaradius ibradius) (values (* oaradius r%) (* obradius r%)))
    (define eye-ring-width (pen-maybe-width eye-stroke))
    
    (cairo_translate cr (+ x0 oaradius) (+ y0 obradius))

    (cairo-positive-arc cr oaradius obradius 0.0 2pi)
    (cairo_new_sub_path cr)
    (cairo-negative-arc cr (max (- iaradius eye-ring-width) 0.0) (max (- ibradius eye-ring-width) 0.0) -2pi 0.0)
    (cairo-render/evenodd cr #false fill)

    (cairo_new_path cr)
    (cairo-positive-arc cr iaradius ibradius 0.0 2pi)
    (cairo-render cr eye-stroke eye-fill)

    (cairo_new_path cr)
    (cairo-positive-arc cr oaradius obradius 0.0 2pi)
    
    (let draw-r ([rs rings%])
      (when (pair? rs)
        (define-values (r% rest) (values (car rs) (cdr rs)))

        (cairo_new_sub_path cr)
        (cairo-positive-arc cr (* oaradius r%) (* obradius r%) 0.0 2pi)
        (draw-r rest)))

    (cairo-render cr stroke)))

(define dc_arc : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum (Option Pen) (Option Brush) Boolean Any)
  (lambda [cr x0 y0 flwidth flheight rstart rend stroke fill sector?]
    (define-values (aradius bradius) (values (* flwidth 0.5) (* flheight 0.5)))
    
    (cairo_translate cr (+ x0 aradius) (+ y0 bradius))
    (cairo-positive-arc cr aradius bradius rstart rend)
    
    (when (and sector? (<= (abs (- rend rstart)) 2pi))
      (cairo_line_to cr 0.0 0.0)
      (cairo_close_path cr))
    
    (cairo-render cr stroke fill)))

(define dc_rectangle : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) (Listof Flonum) (Listof Flonum) Any)
  (lambda [cr x0 y0 flwidth flheight stroke fill vlines hlines]
    (cairo_rectangle cr x0 y0 flwidth flheight)
    
    (let draw-vl ([vls vlines])
      (when (pair? vls)
        (define x (+ (car vls) x0))
        (cairo-add-line cr x y0 x (+ y0 flheight))
        (draw-vl (cdr vls))))
    
    (let draw-hl ([hls hlines])
      (when (pair? hls)
        (define y (+ (car hls) y0))
        (cairo-add-line cr x0 y (+ x0 flwidth) y)
        (draw-hl (cdr hls))))
    
    (cairo-render cr stroke fill)))

(define dc_regular_polygon : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                 Positive-Index Positive-Index Flonum (Option Pen) (Option Brush)
                                 Any)
  (lambda [cr x0 y0 width height n k rotation stroke fill]
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
    (cairo-render cr stroke fill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_convex_rounded_rectangle : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                          Nonnegative-Flonum (Option Pen) (Option Brush) (Listof Flonum) (Listof Flonum)
                                          (List Boolean Boolean Boolean Boolean) Any)
  (lambda [cr x0 y0 flwidth flheight flradius stroke fill vlines hlines corners]
    (define-values (rlt? rrt? rlb? rrb?) (values (car corners) (cadr corners) (caddr corners) (cadddr corners)))
    (define-values (xr yb) (values (+ x0 flwidth) (+ y0 flheight)))
    (define-values (xlset ytset) (values (+ x0 flradius) (+ y0 flradius)))
    (define-values (xrset ybset) (values (- xr flradius) (- yb flradius)))
    
    (cairo_new_path cr)
    (if (and rrt?) (cairo_arc cr xrset ytset flradius -pi/2  0.0)  (cairo_move_to cr xr y0))
    (if (and rrb?) (cairo_arc cr xrset ybset flradius   0.0  pi/2) (cairo_line_to cr xr yb))
    (if (and rlb?) (cairo_arc cr xlset ybset flradius  pi/2  pi)   (cairo_line_to cr x0 yb))
    (if (and rlt?) (cairo_arc cr xlset ytset flradius  pi   3pi/2) (cairo_line_to cr x0 y0))
    (cairo_close_path cr)
    
    (let draw-vl ([vls vlines])
      (when (pair? vls)
        (define x (+ x0 (car vls)))
        
        (cond [(< x xlset)
               (let ([off (dc-line-rounded-offset flradius (- xlset x))])
                 (cairo-add-line cr
                                 x (if (and rlt?) (- ytset off) y0)
                                 x (if (and rlb?) (+ ybset off) yb)))]
              [(> x xrset)
               (let ([off (dc-line-rounded-offset flradius (- x xrset))])
                 (cairo-add-line cr
                                 x (if (and rrt?) (- ytset off) y0)
                                 x (if (and rrb?) (+ ybset off) yb)))]
              [else (cairo-add-line cr x y0 x yb)])
        
        (draw-vl (cdr vls))))
    
    (let draw-hl ([hls hlines])
      (when (pair? hls)
        (define y (+ y0 (car hls)))
        
        (cond [(< y ytset)
               (let ([off (dc-line-rounded-offset flradius (- ytset y))])
                 (cairo-add-line cr
                                 (if (and rlt?) (- xlset off) x0) y
                                 (if (and rrt?) (+ xrset off) xr) y))]
              [(> y ybset)
               (let ([off (dc-line-rounded-offset flradius (- y ybset))])
                 (cairo-add-line cr
                                 (if (and rlb?) (- xlset off) x0) y
                                 (if (and rrb?) (+ xrset off) xr) y))]
              [else (cairo-add-line cr x0 y xr y)])
        
        (draw-hl (cdr hls))))
    
    (cairo-render cr stroke fill)))

(define dc_concave_rounded_rectangle : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                           Nonnegative-Flonum (Option Pen) (Option Brush) (Listof Flonum) (Listof Flonum)
                                           (List Boolean Boolean Boolean Boolean) Any)
  (lambda [cr x0 y0 flwidth flheight flradius stroke fill vlines hlines corners]
    (define-values (rlt? rrt? rlb? rrb?) (values (car corners) (cadr corners) (caddr corners) (cadddr corners)))
    (define-values (xr yb) (values (+ x0 flwidth) (+ y0 flheight)))
    (define-values (xlset ytset) (values (+ x0 flradius) (+ y0 flradius)))
    (define-values (xrset ybset) (values (- xr flradius) (- yb flradius)))
    
    (cairo_new_path cr)
    (if (and rrt?) (cairo_arc cr xr y0 flradius  pi/2  pi)   (cairo_move_to cr xr y0))
    (if (and rlt?) (cairo_arc cr x0 y0 flradius  0.0   pi/2) (cairo_line_to cr x0 y0))
    (if (and rlb?) (cairo_arc cr x0 yb flradius -pi/2  0.0)  (cairo_line_to cr x0 yb))
    (if (and rrb?) (cairo_arc cr xr yb flradius  pi   3pi/2) (cairo_line_to cr xr yb))
    (cairo_close_path cr)
    
    (let draw-vl ([vls vlines])
      (when (pair? vls)
        (define x (+ x0 (car vls)))
        
        (cond [(< x xlset)
               (let ([off (dc-line-rounded-offset flradius (- x x0))])
                 (cairo-add-line cr
                                 x (if (and rlt?) (+ y0 off) y0)
                                 x (if (and rlb?) (- yb off) yb)))]
              [(> x xrset)
               (let ([off (dc-line-rounded-offset flradius (- xr x))])
                 (cairo-add-line cr
                                 x (if (and rrt?) (+ y0 off) y0)
                                 x (if (and rrb?) (- yb off) yb)))]
              [else (cairo-add-line cr x y0 x yb)])
        
        (draw-vl (cdr vls))))
    
    (let draw-hl ([hls hlines])
      (when (pair? hls)
        (define y (+ y0 (car hls)))
        
        (cond [(< y ytset)
               (let ([off (dc-line-rounded-offset flradius (- y y0))])
                 (cairo-add-line cr
                                 (if (and rlt?) (+ x0 off) x0) y
                                 (if (and rrt?) (- xr off) xr) y))]
              [(> y ybset)
               (let ([off (dc-line-rounded-offset flradius (- yb y))])
                 (cairo-add-line cr
                                 (if (and rlb?) (+ x0 off) x0) y
                                 (if (and rrb?) (- xr off) xr) y))]
              [else (cairo-add-line cr x0 y xr y)])
        
        (draw-hl (cdr hls))))
    
    (cairo-render cr stroke fill)))

(define dc_chamfered_rectangle : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                     Nonnegative-Flonum Nonnegative-Flonum (Option Pen) (Option Brush) (Listof Flonum) (Listof Flonum)
                                     (List Boolean Boolean Boolean Boolean) Any)
  (lambda [cr x0 y0 flwidth flheight flxcsize flycsize stroke fill vlines hlines corners]
    (define-values (clt? crt? clb? crb?) (values (car corners) (cadr corners) (caddr corners) (cadddr corners)))
    (define-values (xr yb) (values (+ x0 flwidth) (+ y0 flheight)))
    (define-values (xlset ytset) (values (+ x0 flxcsize) (+ y0 flycsize)))
    (define-values (xrset ybset) (values (- xr flxcsize) (- yb flycsize)))
    
    (cairo_new_path cr)
    (if (and crt?) (cairo-add-line cr xrset y0 xr ytset)  (cairo_move_to cr xr y0))
    (if (and crb?) (cairo-cont-line cr xr ybset xrset yb) (cairo_line_to cr xr yb))
    (if (and clb?) (cairo-cont-line cr xlset yb x0 ybset) (cairo_line_to cr x0 yb))
    (if (and clt?) (cairo-cont-line cr x0 ytset xlset y0) (cairo_line_to cr x0 y0))
    (cairo_close_path cr)
    
    (let draw-vl ([vls vlines])
      (when (pair? vls)
        (define x (+ x0 (car vls)))
        
        (cond [(< x xlset)
               (let ([off (dc-line-chamfered-offset flxcsize (- xlset x) flycsize)])
                 (cairo-add-line cr
                                 x (if (and clt?) (+ y0 off) y0)
                                 x (if (and clb?) (- yb off) yb)))]
              [(> x xrset)
               (let ([off (dc-line-chamfered-offset flxcsize (- x xrset) flycsize)])
                 (cairo-add-line cr
                                 x (if (and crt?) (+ y0 off) y0)
                                 x (if (and crb?) (- yb off) yb)))]
              [else (cairo-add-line cr x y0 x yb)])
        
        (draw-vl (cdr vls))))
    
    (let draw-hl ([hls hlines])
      (when (pair? hls)
        (define y (+ y0 (car hls)))
        
        (cond [(< y ytset)
               (let ([off (dc-line-chamfered-offset flycsize (- ytset y) flxcsize)])
                 (cairo-add-line cr
                                 (if (and clt?) (+ x0 off) x0) y
                                 (if (and crt?) (- xr off) xr) y))]
              [(> y ybset)
               (let ([off (dc-line-chamfered-offset flycsize (- y ybset) flxcsize)])
                 (cairo-add-line cr
                                 (if (and clb?) (+ x0 off) x0) y
                                 (if (and crb?) (- xr off) xr) y))]
              [else (cairo-add-line cr x0 y xr y)])
        
        (draw-hl (cdr hls))))
    
    (cairo-render cr stroke fill)))

(define dc_open_rectangle : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                (Option Pen) (Option Pen) (Option Brush)
                                (Listof Flonum) Nonnegative-Flonum Nonnegative-Flonum
                                (Listof Flonum) Nonnegative-Flonum Nonnegative-Flonum
                                Geo-Border-Open-Sides
                                Any)
  (lambda [cr x0 y0 flwidth flheight stroke line-stroke fill vlines vspan vpos% hlines hspan hpos% open-sides]
    (define-values (xr yb) (values (+ x0 flwidth) (+ y0 flheight)))
    (define xlset (+ x0 (* (-  flwidth hspan) hpos%)))
    (define ytset (+ y0 (* (- flheight vspan) vpos%)))
    (define-values (xrset ybset) (values (+ xlset hspan) (+ ytset vspan)))
    
    (cairo_rectangle cr x0 y0 flwidth flheight)
    
    (when (and fill)
      (cairo-render-with-fill cr fill))

    (when (and line-stroke (or (pair? vlines) (pair? hlines)))
      (cairo_new_path cr)
      
      (let draw-vl ([vls vlines])
        (when (pair? vls)
          (define x (+ (car vls) x0))
          (cairo-add-line cr x ytset x ybset)
          (draw-vl (cdr vls))))
      
      (let draw-hl ([hls hlines])
        (when (pair? hls)
          (define y (+ (car hls) y0))
          (cairo-add-line cr xlset y xrset y)
          (draw-hl (cdr hls))))
      
      (cairo-render cr line-stroke))

    (dc_border cr x0 y0 flwidth flheight stroke open-sides)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc-line-rounded-offset : (-> Flonum Flonum Flonum)
  (lambda [r t]
    (sqrt (max 0.0
               (- (* r r)
                  (* t t))))))

(define dc-line-chamfered-offset : (-> Flonum Flonum Flonum Flonum)
  (lambda [s t ps]
    (* ps (/ t s))))
