#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  
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
 [bitmap_pattern (-> Flonum Flonum Bitmap-Source Flonum Bitmap)])
