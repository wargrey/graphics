#lang typed/racket/base

(provide (all-defined-out))

(require "paint.rkt")
(require "typed/cairo.rkt")
(require "typed/affine.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Master) geo_section : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum
                                            (Cairo-Surface-Draw! Master) Master Nonnegative-Flonum Nonnegative-Flonum
                                            Any)
  (lambda [cr x0 y0 width height x y draw! master owidth oheight]
    (cairo_save cr)
    (cairo-clip cr x0 y0 width height)
    (draw! master cr (- x0 x) (- y0 y) owidth oheight)
    (cairo_restore cr)))

(define #:forall (Master) geo_scale : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum
                                          (Cairo-Surface-Draw! Master) Master Nonnegative-Flonum Nonnegative-Flonum
                                          Any)
  (lambda [cr x0 y0 width height xscale yscale draw! master owidth oheight]
    (define tx (if (< xscale 0.0) (+ x0 width)  x0))
    (define ty (if (< yscale 0.0) (+ y0 height) y0))

    (cairo_save cr)
    ; order matters
    (cairo_translate cr tx ty)
    (cairo_scale cr xscale yscale)
    (draw! master cr 0.0 0.0 owidth oheight)
    (cairo_restore cr)))

(define #:forall (Master) geo_rotate : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum
                                           (Cairo-Surface-Draw! Master) Master Nonnegative-Flonum Nonnegative-Flonum
                                           Any)
  (lambda [cr x0 y0 width height theta.rad draw! master owidth oheight]
    (define-values (ow oh) (values (* width 0.5) (* height 0.5)))
    
    (cairo_save cr)
    ; order matters
    (cairo_translate cr (+ x0 ow) (+ y0 oh))
    (cairo_rotate cr theta.rad)
    (draw! master cr (* owidth -0.5) (* oheight -0.5) owidth oheight)
    (cairo_restore cr)))

(define #:forall (Master) geo_shear : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Flonum Flonum
                                          (Cairo-Surface-Draw! Master) Master Nonnegative-Flonum Nonnegative-Flonum
                                          Any)
  (lambda [cr x0 y0 width height shx shy draw! master owidth oheight]
    (define-values (dx dy) (values (* shx oheight) (* shy owidth)))
    
    (cairo_save cr)
    (cairo-transform cr 1.0 shy shx 1.0 (- x0 (min dx 0.0)) (- y0 (min dy 0.0)))
    (draw! master cr 0.0 0.0 owidth oheight)
    (cairo_restore cr)))
