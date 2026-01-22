#lang typed/racket/base

(provide (all-defined-out))

(require "../paint.rkt")
(require "../typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Border-Open-Side-Datum (Pairof Nonnegative-Flonum Nonnegative-Flonum))

(define-type Geo-Border-Open-Sides
  (List Geo-Border-Open-Side-Datum Geo-Border-Open-Side-Datum
        Geo-Border-Open-Side-Datum Geo-Border-Open-Side-Datum))

(define geo-border-no-open-sides
  (list (cons 0.0 0.5)
        (cons 0.0 0.5)
        (cons 0.0 0.5)
        (cons 0.0 0.5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_border : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Option Pen) Geo-Border-Open-Sides Any)
  (lambda [cr x0 y0 flwidth flheight stroke open-sides]
    (define-values (xr yb) (values (+ x0 flwidth) (+ y0 flheight)))
    
    (when (and stroke)
      (define cx (+ x0 (* flwidth 0.5)))
      
      (cairo_new_path cr)
      
      (define maybe-xterm
        (let-values ([(tspan tpos%) (values (caar open-sides) (cdar open-sides))])
          (cond [(= tspan 0.0) (cairo_move_to cr cx y0) (cairo_line_to cr xr y0) cx]
                [(= tspan 1.0) (cairo_move_to cr xr y0) #false]
                [(= tpos% 0.0) (cairo_move_to cr (+ x0 tspan) y0) (cairo_line_to cr xr y0) #false]
                [(= tpos% 1.0) (cairo_move_to cr xr y0) (- xr tspan)]
                [else (let ([toffset (* (- flwidth tspan) tpos%)])
                        (cairo_move_to cr (+ x0 toffset tspan) y0)
                        (cairo_line_to cr xr y0)
                        (+ x0 toffset))])))

      (let-values ([(rspan rpos%) (values (caadr open-sides) (cdadr open-sides))])
        (cond [(= rspan 0.0) (cairo_line_to cr xr yb)]
              [(= rspan 1.0) (cairo_move_to cr xr yb)]
              [(= rpos% 0.0) (cairo_move_to cr xr (+ y0 rspan)) (cairo_line_to cr xr yb)]
              [(= rpos% 1.0) (cairo_line_to cr xr (- yb rspan)) (cairo_move_to cr xr yb)]
              [else (let ([roffset (* (- flheight rspan) rpos%)])
                      (cairo_line_to cr xr (+ y0 roffset))
                      (cairo_move_to cr xr (+ y0 roffset rspan))
                      (cairo_line_to cr xr yb))]))

      (let-values ([(bspan bpos%) (values (caaddr open-sides) (cdaddr open-sides))])
        (cond [(= bspan 0.0) (cairo_line_to cr x0 yb)]
              [(= bspan 1.0) (cairo_move_to cr x0 yb)]
              [(= bpos% 0.0) (cairo_line_to cr (+ x0 bspan) yb) (cairo_move_to cr x0 yb)]
              [(= bpos% 1.0) (cairo_move_to cr (- xr bspan) yb) (cairo_line_to cr x0 yb)]
              [else (let ([offset (* (- flwidth bspan) bpos%)])
                      (cairo_line_to cr (+ x0 offset bspan) yb)
                      (cairo_move_to cr (+ x0 offset) yb)
                      (cairo_line_to cr x0 yb))]))

      (let-values ([(lspan lpos%) (values (car (cadddr open-sides)) (cdr (cadddr open-sides)))])
        (cond [(= lspan 0.0) (cairo_line_to cr x0 y0)]
              [(= lspan 1.0) (cairo_move_to cr x0 y0)]
              [(= lpos% 0.0) (cairo_line_to cr x0 (+ y0 lspan)) (cairo_move_to cr x0 y0)]
              [(= lpos% 1.0) (cairo_move_to cr x0 (- yb lspan)) (cairo_line_to cr x0 y0)]
              [else (let ([offset (* (- flheight lspan) lpos%)])
                      (cairo_line_to cr x0 (+ y0 offset lspan))
                      (cairo_move_to cr x0 (+ y0 offset))
                      (cairo_line_to cr x0 y0))]))

      (when (and maybe-xterm)
        (cairo_line_to cr maybe-xterm y0))

      (cairo-render cr stroke))))
