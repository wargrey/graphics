#lang typed/racket/base

(provide (all-defined-out))

(require "../base.rkt")
(require "convert.rkt")
(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require "paint.rkt")

  (require (submod "pixman.rkt" unsafe))
  (require (submod "convert.rkt" unsafe))
  
  (define (λbitmap width height density λargb)
    (define-values (img cr) (make-cairo-image width height density #false))
    (define surface (cairo_get_target cr))
    (define-values (pixels total stride w h) (cairo-surface-metrics surface 4))
    (let y-loop ([y 0])
      (when (unsafe-fx< y h)
        (let x-loop ([x 0] [idx (unsafe-fx* y stride)])
          (when (unsafe-fx< x w)
            (define-values (a r g b) (λargb x y w h))
            (pixels-set-argb-reals pixels idx a r g b)
            (x-loop (unsafe-fx+ x 1) (unsafe-fx+ idx 4))))
        (y-loop (unsafe-fx+ y 1))))
    (cairo_surface_mark_dirty surface)
    (cairo_destroy cr)
    img)
  
  (define (bitmap_blank width height density)
    (define-values (img cr) (make-cairo-image width height density #true))
    (cairo_destroy cr)
    img)

  (define (bitmap_pattern width height background density)
    (define-values (img cr) (make-cairo-image* width height background density #true))
    (cairo_destroy cr)
    img)

  (define (bitmap_frame src mtop mright mbottom mleft ptop pright pbottom pleft border background density)
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define line-inset (unsafe-fl/ line-width 2.0))
    (define-values (dest-width dest-height) (cairo-surface-size src density))
    (define-values (width border-x border-width dest-x) (frame-metrics line-width line-inset mleft mright pleft pright dest-width))
    (define-values (height border-y border-height dest-y) (frame-metrics line-width line-inset mtop mbottom ptop pbottom dest-height))
    (define-values (img cr) (make-cairo-image width height density #true))
    (cairo_rectangle cr border-x border-y border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src dest-x dest-y dest-width dest-height CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density #false)
    (cairo_destroy cr)
    img)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (frame-metrics line-width line-inset mopen mclose popen pclose size)
    (define-values (flmopen flmclose) (values (~length mopen size) (~length mclose size)))
    (define-values (flpopen flpclose) (values (~length popen size) (~length pclose size)))
    (define border-position (unsafe-fl+ flmopen line-inset))
    (define position (unsafe-fl+ (unsafe-fl+ flmopen flpopen) line-width))
    (define border-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ flpopen flpclose) size) line-width))
    (define frame-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ border-position border-size) flmclose) line-inset))
    (values frame-size border-position border-size position))

  (define (list->4:values ls defval)
    (case (length ls)
      [(0) (values defval defval defval defval)]
      [(1) (let ([top (unsafe-car ls)]) (values top top top top))]
      [(2) (let ([top (unsafe-car ls)] [right (unsafe-car (unsafe-cdr ls))]) (values top right top right))]
      [(3) (let ([top (unsafe-car ls)] [right (unsafe-list-ref ls 1)] [bottom (unsafe-list-ref ls 2)]) (values top right bottom right))]
      [else (values (unsafe-car ls) (unsafe-list-ref ls 1) (unsafe-list-ref ls 2) (unsafe-list-ref ls 3))])))

(define-type XYWH->ARGB (-> Index Index Index Index (Values Real Real Real Real)))

(unsafe/require/provide
 (submod "." unsafe)
 [list->4:values (All (a) (-> (Listof a) a (Values a a a a)))]
 [λbitmap (-> Flonum Flonum Flonum XYWH->ARGB Bitmap)]
 [bitmap_blank (-> Flonum Flonum Flonum Bitmap)]
 [bitmap_pattern (-> Flonum Flonum Bitmap-Source Flonum Bitmap)]
 [bitmap_frame (-> Bitmap-Surface Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum
                   (Option Paint) (Option Bitmap-Source) Flonum Bitmap)])
