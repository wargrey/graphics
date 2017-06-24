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
    (define-values (width border-x border-width dest-x dest-width)
      (frame-metrics line-width line-inset mleft mright pleft pright (cairo_image_surface_get_width src) density))
    (define-values (height border-y border-height dest-y dest-height)
      (frame-metrics line-width line-inset mtop mbottom ptop pbottom (cairo_image_surface_get_height src) density))
    (define-values (img cr w h) (make-cairo-image width height density))
    (cairo_rectangle cr border-x border-y border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src dest-x dest-y dest-width dest-height CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density)
    (cairo_destroy cr)
    img)

  (define (bitmap_alter_density src density)
    (define flwidth (unsafe-fl/ (unsafe-fx->fl (cairo_image_surface_get_width src)) density))
    (define flheight (unsafe-fl/ (unsafe-fx->fl (cairo_image_surface_get_height src)) density))
    (define-values (img cr w h) (make-cairo-image flwidth flheight density))
    (cairo-composite cr src 0.0 0.0 flwidth flheight CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density)
    (cairo_destroy cr)
    img)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (argb->datum v)
    (unsafe-fxmin
     (unsafe-fxmax
      (unsafe-fl->fx
       (unsafe-fl* (real->double-flonum v) 255.0))
      #x00)
     #xFF))

  (define (frame-metrics line-width line-inset mopen mclose popen pclose fxsize density)
    (define border-position (unsafe-fl+ mopen line-inset))
    (define position (unsafe-fl+ (unsafe-fl+ mopen popen) line-width))
    (define size (unsafe-fl/ (unsafe-fx->fl fxsize) density))
    (define border-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ popen pclose) size) line-width))
    (define frame-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ border-position border-size) mclose) line-inset))
    (values frame-size border-position border-size position size)))

(define-type XYWH->ARGB (-> Nonnegative-Fixnum Nonnegative-Fixnum Positive-Fixnum Positive-Fixnum (Values Real Real Real Real)))

(unsafe/require/provide
 (submod "." unsafe)
 [λbitmap (-> Flonum Flonum Flonum XYWH->ARGB Bitmap)]
 [bitmap_blank (-> Flonum Flonum Flonum Bitmap)]
 [bitmap_pattern (-> Flonum Flonum Bitmap-Source Flonum Bitmap)]
 [bitmap_alter_density (-> #|Bitmap-Surface|# Any Flonum Bitmap)]
 [bitmap_frame (-> #|Bitmap-Surface|# Any Flonum Flonum Flonum Flonum Flonum Flonum Flonum Flonum
                   (Option Paint) (Option Bitmap-Source) Flonum Bitmap)])
