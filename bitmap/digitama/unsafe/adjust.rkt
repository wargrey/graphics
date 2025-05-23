#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require geofun/digitama/unsafe/typed/c)

(require "../convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require geofun/digitama/geometry/affine)

  (require racket/draw/unsafe/cairo)
  (require racket/unsafe/ops)
  
  (require "../convert.rkt")
  (require "pixman.rkt")
  
  (require (submod "bitmap.rkt" unsafe))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_section src x y width height density)
    (define-values (img cr) (create-argb-bitmap (unsafe-fl/ width density) (unsafe-fl/ height density) density #false))
    (cairo_set_source_surface cr src (unsafe-fl- 0.0 x) (unsafe-fl- 0.0 y))
    (cairo_paint cr)
    img)

  (define (bitmap_scale src xscale yscale density)
    (define-values (width height) (bitmap-surface-rendered-size src density))
    (define flwidth (unsafe-fl* width (unsafe-flabs xscale)))
    (define flheight (unsafe-fl* height (unsafe-flabs yscale)))
    (define-values (img cr) (create-argb-bitmap flwidth flheight density #false))
    (define tx (if (unsafe-fl< xscale 0.0) (unsafe-fl* flwidth density) 0.0))
    (define ty (if (unsafe-fl< yscale 0.0) (unsafe-fl* flheight density) 0.0))

    ; order matters
    (cairo_translate cr tx ty)
    (cairo_scale cr xscale yscale)
    
    (cairo_set_source_surface cr src 0.0 0.0)
    (cairo_paint cr)
    img)

  (define (bitmap_rotate src theta.rad density)
    (define-values (ow oh) (bitmap-surface-rendered-size src 1.0))
    (define-values (rw rh) (geo-size-rotate ow oh theta.rad))
    (define-values (img cr) (create-argb-bitmap (unsafe-fl/ rw density) (unsafe-fl/ rh density) density #false))
    
    (cairo_translate cr (unsafe-fl* rw 0.5) (unsafe-fl* rh 0.5))
    (cairo_rotate cr theta.rad)
    (cairo_set_source_surface cr src (unsafe-fl* ow -0.5) (unsafe-fl* oh -0.5))
    (cairo_paint cr)
    img)

  (define (bitmap_bounding_box src just-alpha?)
    (define-values (pixels total stride w h) (bitmap-surface-metrics src 4))
    (define-values (zero-dot? dotoff) (if just-alpha? (values pixel-alpha-zero? A) (values pixel-zero? 0)))
    
    (let ([x w] [y h] [X 0] [Y 0])
      (let y-loop ([yn 0] [idx dotoff])
        (when (unsafe-fx< yn h)
          (let x-loop ([xn 0] [idx idx])
            (cond [(unsafe-fx< xn w)
                   (unless (zero-dot? pixels idx)
                     (set! x (unsafe-fxmin x xn))
                     (set! y (unsafe-fxmin y yn))
                     (set! X (unsafe-fxmax X (unsafe-fx+ 1 xn)))
                     (set! Y (unsafe-fxmax Y (unsafe-fx+ 1 yn))))
                   (x-loop (unsafe-fx+ xn 1) (unsafe-fx+ idx 4))]
                  [else (y-loop (unsafe-fx+ yn 1) idx)]))))
      (values x y X Y)))

  (define (bitmap_bounding_box* src just-alpha? density)
    (define-values (x y X Y) (bitmap_bounding_box src just-alpha?))
    (values (unsafe-fl/ (unsafe-fx->fl x) density)
            (unsafe-fl/ (unsafe-fx->fl y) density)
            (unsafe-fl/ (unsafe-fx->fl (unsafe-fx- X 1)) density)
            (unsafe-fl/ (unsafe-fx->fl (unsafe-fx- Y 1)) density))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap_section (-> Bitmap-Surface Flonum Flonum Flonum Flonum Positive-Flonum Bitmap)]
 [bitmap_scale (-> Bitmap-Surface Flonum Flonum Positive-Flonum Bitmap)]
 [bitmap_rotate (-> Bitmap-Surface Flonum Positive-Flonum Bitmap)]
 [bitmap_bounding_box (-> Bitmap-Surface Boolean (Values Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum Nonnegative-Fixnum))]
 [bitmap_bounding_box* (-> Bitmap-Surface Boolean Flonum
                           (Values Nonnegative-Flonum Nonnegative-Flonum
                                   Nonnegative-Flonum Nonnegative-Flonum))])
