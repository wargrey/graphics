#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require "paint.rkt")
  
  (define (λbitmap width height density λargb)
    (define-values (img cr w h) (make-cairo-image width height density))
    (define surface (cairo_get_target cr))
    (define buffer (cairo_image_surface_get_data surface))
    (define stride (cairo_image_surface_get_stride surface))
    (define total (unsafe-bytes-length buffer))
    (define W (unsafe-fxquotient stride 4))
    (define H (unsafe-fxquotient total stride))
    (let y-loop ([y 0])
      (when (unsafe-fx< y H)
        (let x-loop ([x 0] [idx (unsafe-fx* y stride)])
          (when (unsafe-fx< x W)
            (define-values (a r g b) (λargb x y W H))
            (unsafe-bytes-set! buffer (unsafe-fx+ idx A) (argb->datum a))
            (unsafe-bytes-set! buffer (unsafe-fx+ idx R) (argb->datum r))
            (unsafe-bytes-set! buffer (unsafe-fx+ idx G) (argb->datum g))
            (unsafe-bytes-set! buffer (unsafe-fx+ idx B) (argb->datum b))
            (x-loop (unsafe-fx+ x 1) (unsafe-fx+ idx 4))))
        (y-loop (unsafe-fx+ y 1))))
    (cairo_destroy cr)
    img)
  
  (define (bitmap_blank width height density)
    (define-values (img cr w h) (make-cairo-image width height density))
    (cairo_destroy cr)
    img)

  (define (bitmap_pattern width height background density)
    (define-values (img cr w h) (make-cairo-image* width height background density))
    (cairo_destroy cr)
    img)

  (define (bitmap_frame src mtop mright mbottom mleft ptop pright pbottom pleft border background density)
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define line-inset (unsafe-fl/ line-width 2.0))
    (define 1/density (unsafe-fl/ 1.0 density))
    (define border-x (unsafe-fl+ mleft line-inset))
    (define border-y (unsafe-fl+ mtop line-inset))
    (define mask-x (unsafe-fl+ (unsafe-fl+ mleft pleft) line-width))
    (define mask-y (unsafe-fl+ (unsafe-fl+ mtop ptop) line-width))
    (define mask-width (unsafe-fl* (unsafe-fx->fl (cairo_image_surface_get_width src)) 1/density))
    (define mask-height (unsafe-fl* (unsafe-fx->fl (cairo_image_surface_get_height src)) 1/density))
    (define border-width (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ pleft pright) mask-width) line-width))
    (define border-height (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ ptop pbottom) mask-height) line-width))
    (define width (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ border-x border-width) mright) line-inset))
    (define height (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ border-y border-height) mbottom) line-inset))
    (define-values (img cr w h) (make-cairo-image width height density))

    (cairo_rectangle cr border-x border-y border-width border-height)
    (cairo-render cr border background)
    
    (cairo_translate cr mask-x mask-y) ; translating must before scaling
    (cairo_scale cr 1/density 1/density)
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_pattern_set_filter (cairo_get_source cr) CAIRO_FILTER_NEAREST)
    #;(cairo_set_operator cr CAIRO_OPERATOR_OVER) ; default
    (cairo_paint cr)
    (cairo_destroy cr)
    img)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (argb->datum v)
    (unsafe-fxmin
     (unsafe-fxmax
      (unsafe-fl->fx
       (unsafe-fl* (real->double-flonum v) 255.0))
      #x00)
     #xFF)))

(define-type XYWH->ARGB (-> Nonnegative-Fixnum Nonnegative-Fixnum Positive-Fixnum Positive-Fixnum (Values Real Real Real Real)))

(unsafe/require/provide
 (submod "." unsafe)
 [λbitmap (-> Flonum Flonum Flonum XYWH->ARGB Bitmap)]
 [bitmap_blank (-> Flonum Flonum Flonum Bitmap)]
 [bitmap_pattern (-> Flonum Flonum Bitmap-Source Flonum Bitmap)]
 [bitmap_frame (-> #|Bitmap-Surface|# Any Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum
                   (Option Paint) (Option Bitmap-Source) Flonum Bitmap)])
