#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "../../paint/self.rkt")
(require "../../geometry/footprint.rkt")
(require "../../geometry/bezier.rkt")

(require "../paint.rkt")
(require "../typed/cairo.rkt")
(require "../typed/more.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_path : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Geo-Path-Prints (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 width height footprints stroke brush]
    (cairo_path cr footprints x0 y0 #false)
    (cairo-render cr stroke brush)))

(define dc_polyline : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Listof Float-Complex) Pen Boolean Any)
  (lambda [cr x0 y0 flwidth flheight vertices stroke close?]
    (cairo_new_path cr)
    (cairo_simple_path cr vertices x0 y0 #false)

    (when (and close?)
      (cairo_close_path cr))
    
    (cairo-render cr stroke #false)))

(define dc_polygon : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum (Listof Float-Complex) (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight vertices stroke background]
    (cairo_new_path cr)
    (cairo_simple_path cr vertices x0 y0 #true)
    (cairo_close_path cr)
    (cairo-render cr stroke background)))

(define dc_polyline* : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Geo-Path-Prints (Option Pen) (Option Brush) Boolean Any)
  (lambda [cr x0 y0 flwidth flheight footprints stroke background close?]
    (cairo_new_path cr)

    (define M : Natural
      (if (or background)
          (let ([M (cairo_path cr footprints x0 y0 #true)])
            (cairo-render-with-fill cr background)
            (cairo_translate cr (- x0) (- y0))
            M)
          2))

    (when (and stroke)
      (when (> M 1)
        (cairo_new_path cr)
        (cairo_path cr footprints x0 y0 #false))

      (when (and close?)
        (cairo_close_path cr))

      (cairo-render-with-stroke cr stroke))))

(define dc_polygon* : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Geo-Path-Prints (Option Pen) (Option Brush) Any)
  (lambda [cr x0 y0 flwidth flheight footprints stroke background]
    (cairo_new_path cr)
    (cairo_path cr footprints x0 y0 #false)
    (cairo_close_path cr)
    (cairo-render cr stroke background)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo_simple_path : (-> Cairo-Ctx (Listof Float-Complex) Flonum Flonum Boolean Void)
  (lambda [cr vertices dx dy ignore-nan?]
    (when (pair? vertices)
      (cairo_translate cr dx dy)
      
      (for ([pt (in-list vertices)])
        (define x (real-part pt))
        (define y (imag-part pt))

        (cond [(not (or (nan? x) (nan? y))) (cairo_line_to cr (real-part pt) (imag-part pt))]
              [(not ignore-nan?) (cairo_new_sub_path cr)])))))

(define cairo_path : (-> Cairo-Ctx Geo-Path-Prints Flonum Flonum Boolean Natural)
  (lambda [cr footprints dx dy treat-M-as-L?]
    (cairo_translate cr dx dy)
    
    (let draw_path : Natural ([prints footprints]
                              [M : Natural 0])
      (if (pair? prints)
          (draw_path (cdr prints)
                     (let ([self (car prints)])
                       (cond [(gpp:point? self) (cairo_straight-line cr (gpath:datum-cmd self) (gpath:print-end-here self) treat-M-as-L? M)]
                             [(gpp:arc? self) #\A (cairo_elliptical_arc cr self) M]
                             [(gpp:bezier? self) (cairo_bezier_curve cr self 0.0+0.0i 0.0+0.0i) M]
                             [(gpp:vector? self)
                              (let ([cmd (gpath:datum-cmd self)]
                                    [pt (gpp:vector-rel-end self)])
                                (cond [(eq? cmd #\l) (cairo_rel_line_to cr (real-part pt) (imag-part pt))]
                                      [(eq? cmd #\m) (cairo_rel_move_to cr (real-part pt) (imag-part pt))])
                                M)]
                             [(gpp:close? self) #\Z #\z (cairo_close_path cr) M]
                             [else M])))
          M))))

(define cairo_clean_path : (-> Cairo-Ctx Geo-Path-Clean-Prints Flonum Flonum (Option Float-Complex) (Option Float-Complex) Boolean Void)
  (lambda [cr footprints dx dy src-adjust tgt-adjust treat-M-as-L?]
    (cairo_translate cr dx dy)

    (define source-adjusted-footprints : Geo-Path-Clean-Prints
      (if (and src-adjust (pair? footprints))
          (let ([source (car footprints)])
            (cond [(gpp:point? source)
                   (cairo_straight-line cr (gpath:datum-cmd source) (+ (gpath:print-end-here source) src-adjust) treat-M-as-L? 0)
                   (cdr footprints)]
                  ; meanwhile, the head point of a bezier curve is also taken out of the storage
                  ; so there is no need to deal with it.
                  [else footprints]))
          footprints))
    
    (let draw_clean_path : Void ([prints : Geo-Path-Clean-Prints source-adjusted-footprints]
                                 [head? : Boolean #true])
      (when (pair? prints)
        (define-values (self rest) (values (car prints) (cdr prints)))

        (cond [(gpp:point? self)
               (let ([cmd (gpath:datum-cmd self)]
                     [pt (gpath:print-end-here self)])
                 (if (and (null? rest) tgt-adjust)
                     (cairo_straight-line cr cmd (+ pt tgt-adjust) treat-M-as-L? 0)
                     (cairo_straight-line cr cmd pt treat-M-as-L? 0)))]
              [(gpp:arc? self) #\A (cairo_elliptical_arc cr self)]
              [(gpp:bezier? self) (cairo_bezier_curve cr self
                                                      (or (and head? src-adjust) 0.0+0.0i)
                                                      (or (and (null? rest) tgt-adjust) 0.0+0.0i))])
        
        (draw_clean_path rest #false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo_straight-line : (-> Cairo-Ctx Char Float-Complex Boolean Natural Natural)
  (lambda [cr cmd pt treat-M-as-L? M]
    (cond [(eq? cmd #\L)
           (cairo_line_to cr (real-part pt) (imag-part pt)) M]
          [(eq? cmd #\M)
           (if (not treat-M-as-L?)
               (cairo_move_to cr (real-part pt) (imag-part pt))
               (cairo_line_to cr (real-part pt) (imag-part pt)))
           (+ M 1)]
          [else M])))

(define cairo_elliptical_arc : (-> Cairo-Ctx GPP:Arc Void)
  (lambda [cr self]
    (define center (gpp:arc-center self))
    (define cx (real-part center))
    (define cy (imag-part center))
    (define rx (gpp:arc-rx self))
    (define ry (gpp:arc-ry self))
    (define rstart (gpp:arc-start self))
    (define rend (gpp:arc-end self))

    (if (gpp:arc-clockwise? self)
        (cairo-positive-arc cr cx cy rx ry rstart rend)
        (cairo-negative-arc cr cx cy rx ry rstart rend))))

;;; https://pomax.github.io/bezierinfo/#reordering
(define cairo_bezier_curve : (-> Cairo-Ctx GPP:Bezier Float-Complex Float-Complex Void)
  (lambda [cr self src-offset end-offset]
    (define srcpt (gpp:bezier-start-here self))
    (define endpt (gpath:print-end-here self))
    (define samples (gpp:bezier-samples self))
    (define no-offset? (and (= src-offset 0.0+0.0i) (= end-offset 0.0+0.0i)))

    (cond [(gpp:bezier:cubic? self)
           (if (or no-offset?)
               (cairo_cubic_bezier cr (gpp:bezier:cubic-ctrl1 self) (gpp:bezier:cubic-ctrl2 self) endpt)
               (cairo_nth_bezier cr srcpt (list (gpp:bezier:cubic-ctrl1 self) (gpp:bezier:cubic-ctrl2 self) endpt)
                                 samples src-offset end-offset))]
          [(gpp:bezier:quadratic? self)
           (if (or no-offset?)
               (cairo_quadratic_bezier cr (gpp:bezier-start-here self) (gpp:bezier:quadratic-ctrl self) endpt)
               (cairo_nth_bezier cr srcpt (list (gpp:bezier:quadratic-ctrl self) endpt)
                                 samples src-offset end-offset))]
          [(gpp:bezier:nth? self)
           (if (or no-offset?)
               (cairo_nth_bezier cr srcpt (gpp:bezier:nth-ctrls+endpoint self) samples)
               (cairo_nth_bezier cr srcpt (gpp:bezier:nth-ctrls+endpoint self)
                                 samples src-offset end-offset))])))

; ctrl1 = (+ spt 2/3(ctrl - spt)) = (+ spt ctrl ctrl)/3
; ctrl2 = (+ ept 2/3(ctrl - ept)) = (+ ept ctrl ctrl)/3
(define cairo_quadratic_bezier : (-> Cairo-Ctx Float-Complex Float-Complex Float-Complex Void)
  (let ([coefficient (real->double-flonum 1/3)])
    (lambda [cr spt ctrl ept]
      (define 2ctrl (+ ctrl ctrl))
      
      (cairo_cubic_bezier cr (* (+ spt 2ctrl) coefficient) (* (+ ept 2ctrl) coefficient) ept))))

(define cairo_cubic_bezier : (-> Cairo-Ctx Float-Complex Float-Complex Float-Complex Void)
  (lambda [cr ctrl1 ctrl2 endpt]
    (cairo_curve_to cr
                    (real-part ctrl1) (imag-part ctrl1)
                    (real-part ctrl2) (imag-part ctrl2)
                    (real-part endpt) (imag-part endpt))))

(define cairo_nth_bezier : (case-> [Cairo-Ctx Float-Complex (Listof Float-Complex) Index -> Void]
                                   [Cairo-Ctx Float-Complex (Listof Float-Complex) Index Float-Complex Float-Complex -> Void])
  (case-lambda
    [(cr head tail samples)
     (let ([fbezier (bezier-function head tail #:derivative 0)])
       (unless (not fbezier)
         (define delta (real->double-flonum (/ 1.0 (exact->inexact (max samples 1)))))
         
         (for ([t (in-range 0.0 (+ 1.0 delta) delta)])
           (define dot (fbezier t))
           (cairo_line_to cr (real-part dot) (imag-part dot)))))]
    [(cr head tail samples src-offset end-offset)
     (let ([fbezier (bezier-function head tail #:derivative 0)])
       (unless (not fbezier)
         (define t0 (exact->inexact (bezier-reparameterize-by-length head tail (magnitude src-offset) samples #:t0 0 #:tn 1)))
         (define tn (exact->inexact (bezier-reparameterize-by-length head tail (magnitude end-offset) samples #:t0 1 #:tn 0)))

         (define delta (/ 1.0 (exact->inexact (max samples 1))))
         (for ([t (in-range t0 (+ tn delta) delta)])
           (define dot (fbezier t))
           (cairo_line_to cr (real-part dot) (imag-part dot)))))]))
