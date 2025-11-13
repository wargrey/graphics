#lang typed/racket/base

(provide (all-defined-out))

(require digimon/constant)

(require "../source.rkt")
(require "../paint.rkt")

(require "../../paint/self.rkt")
(require "../../skeleton/stickman/self.rkt")

(require "../typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dc_stickman : (-> Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                          Geo-Stickman-Skeleton Nonnegative-Flonum Nonnegative-Flonum
                          (Option Pen) (Option Fill-Source) (Option Fill-Source) (Option Fill-Source)
                          Any)
  (lambda [cr x0 y0 flwidth flheight self scale fallback-thickness stroke head-fill body-fill arm-fill]
    (define thickness (* (pen-maybe-width stroke fallback-thickness) scale))
    (define rhead : Nonnegative-Flonum (* (geo-stickman-skeleton-head-radius self) scale))
    (define torso-width : Nonnegative-Flonum (* (geo-stickman-skeleton-torso-width self) scale))
    (define leg-width : Nonnegative-Flonum (* (geo-stickman-skeleton-leg-width self) scale))
    (define arm-width : Nonnegative-Flonum (* (geo-stickman-skeleton-arm-width self) scale))
    (define origin : Float-Complex (make-rectangular (+ x0 (* flwidth 0.5)) (+ y0 (* thickness 0.5))))

    (define (transform [pt : Float-Complex]) : Float-Complex
      (+ (* pt scale) origin))

    (define head : Float-Complex (transform (geo-stickman-skeleton-head self)))
    (define neck : Float-Complex (transform (geo-stickman-skeleton-neck self)))
    (define hip : Float-Complex  (transform (geo-stickman-skeleton-hip  self)))
    (define lft-arm-pts : (Listof Float-Complex) (stickman-arm-points (geo-stickman-skeleton-left-arm self) transform))
    (define rgt-arm-pts : (Listof Float-Complex) (stickman-arm-points (geo-stickman-skeleton-right-arm self) transform))
    (define lft-leg-pts : (Listof Float-Complex) (stickman-leg-points (geo-stickman-skeleton-left-leg self) transform))
    (define rgt-leg-pts : (Listof Float-Complex) (stickman-leg-points (geo-stickman-skeleton-right-leg self) transform))

    (define (draw-body [thickness : Nonnegative-Flonum] [scale : Nonnegative-Flonum] [pen : (Option Pen)]) : Void
      (cairo_new_path cr)
      (cairo-set-thickline-stroke cr pen body-fill (+ torso-width thickness) scale #true)
      (cairo-add-line cr neck hip)
      (cairo_stroke cr)
      
      (cairo-set-thickline-stroke cr pen body-fill (+ leg-width thickness) scale #true)
      (cairo-add-lines cr hip lft-leg-pts)
      (cairo-add-lines cr hip rgt-leg-pts)
      (cairo_stroke cr))

    (define (draw-arm [pts : (Listof Float-Complex)] [thickness : Nonnegative-Flonum] [scale : Nonnegative-Flonum] [pen : (Option Pen)]) : Void
      (cairo_new_path cr)
      (cairo-set-thickline-stroke cr pen arm-fill (+ arm-width thickness) scale #true)
      (cairo-add-lines cr pts)
      (cairo_stroke cr))

    (let ([actual-thickness (* thickness 2.0)])
      (draw-arm rgt-arm-pts actual-thickness 0.5 stroke)
      (draw-arm rgt-arm-pts 0.0 1.0 #false)
      
      (draw-body actual-thickness 0.5 stroke)
      (draw-body 0.0 1.0 #false)
      
      (draw-arm lft-arm-pts actual-thickness 0.5 stroke)
      (draw-arm lft-arm-pts 0.0 1.0 #false))
      
    (cairo_new_path cr)
    (cairo-positive-arc cr head rhead rhead 0.0 2pi)
    (cairo-render cr #false head-fill)
    (cairo-set-thickline-stroke cr stroke head-fill thickness 0.5 #true)
    (cairo_stroke_preserve cr)))
