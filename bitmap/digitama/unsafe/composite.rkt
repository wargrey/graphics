#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "draw.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out) cairo-image-size)
  
  (require "pangocairo.rkt")
  (require "paint.rkt")
  
  (define (bitmap_composite operator sfc1 x1 y1 sfc2 x2 y2 density)
    (define-values (w1 h1) (cairo-image-size sfc1 density))
    (define-values (w2 h2) (cairo-image-size sfc2 density))
    (define-values (dx dy) (values (unsafe-fl- x1 x2) (unsafe-fl- y1 y2)))
    (define-values (dx1 dy1) (values (unsafe-flmax (unsafe-fl- 0.0 dx) 0.0) (unsafe-flmax (unsafe-fl- 0.0 dy) 0.0)))
    (define-values (dx2 dy2) (values (unsafe-flmax dx 0.0) (unsafe-flmax dy 0.0)))
    (define-values (img cr w h)
      (make-cairo-image (unsafe-flmax (unsafe-fl+ dx1 w1) (unsafe-fl+ dx2 w2))
                        (unsafe-flmax (unsafe-fl+ dy1 h1) (unsafe-fl+ dy2 h2))
                        density #true)) 
    (cairo-composite cr sfc1 dx1 dy1 w1 h1 CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_SOURCE density #true)
    (cairo-composite cr sfc2 dx2 dy2 w2 h2 CAIRO_FILTER_BILINEAR operator density #false)
    (cairo_destroy cr)
    img)

  (define (bitmap_pin x1% y1% x2% y2% sfc1 sfc2 density)
    (define-values (w1 h1) (cairo-image-size sfc1 density))
    (define-values (w2 h2) (cairo-image-size sfc2 density))
    (bitmap_composite CAIRO_OPERATOR_OVER sfc1
                      (unsafe-fl- (unsafe-fl* x1% w1) (unsafe-fl* x2% w2))
                      (unsafe-fl- (unsafe-fl* y1% h1) (unsafe-fl* y2% h2))
                      sfc2 0.0 0.0 density))

  (define (bitmap_append alignment base others gapsize density)
    (define-values (min-width min-height) (cairo-image-size base density))
    (define-values (flwidth flheight children)
      (let compose ([width min-width] [height min-height] [nerdlihc (list (vector base min-width min-height))] [children others])
        (cond [(null? children) (values width height (reverse nerdlihc))]
              [else (let-values ([(child rest) (values (unsafe-car children) (unsafe-cdr children))])
                      (define-values (chwidth chheight) (cairo-image-size child density))
                      (define sllec++ (cons (vector child chwidth chheight) nerdlihc))
                      (case alignment
                        [(vl vc vr) (compose (unsafe-flmax width chwidth) (unsafe-fl+ gapsize (unsafe-fl+ height chheight)) sllec++ rest)]
                        [(ht hc hb) (compose (unsafe-fl+ gapsize (unsafe-fl+ width chwidth)) (unsafe-flmax height chheight) sllec++ rest)]
                        [else #|unreachable|# (compose (unsafe-flmax width chwidth) (unsafe-flmax height chheight) sllec++ rest)]))])))
    
    (define-values (bmp cr w h) (make-cairo-image flwidth flheight density #true))
    (let combine ([others children] [maybe-used-xoff (unsafe-fl- 0.0 gapsize)] [maybe-used-yoff (unsafe-fl- 0.0 gapsize)])
      (unless (null? others)
        (define-values (child rest) (values (unsafe-car others) (unsafe-cdr others)))
        (define-values (maybe-used-x maybe-used-y) (values (unsafe-fl+ maybe-used-xoff gapsize) (unsafe-fl+ maybe-used-yoff gapsize)))
        (define-values (chwidth chheight) (values (unsafe-vector-ref child 1) (unsafe-vector-ref child 2)))
        (define-values (dest-x dest-y)
          (case alignment
            [(vl) (values 0.0                                           maybe-used-y)]
            [(vc) (values (unsafe-fl/ (unsafe-fl- flwidth chwidth) 2.0) maybe-used-y)]
            [(vr) (values (unsafe-fl- flwidth chwidth)                  maybe-used-y)]
            [(ht) (values maybe-used-x                                  0.0)]
            [(hc) (values maybe-used-x                                  (unsafe-fl/ (unsafe-fl- flheight chheight) 2.0))]
            [(hb) (values maybe-used-x                                  (unsafe-fl- flheight chheight))]
            [else #|unreachable|# (values maybe-used-x                  maybe-used-y)]))
        (cairo-composite cr (unsafe-vector-ref child 0) dest-x dest-y chwidth chheight
                         CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density #true)
        (combine rest (unsafe-fl+ maybe-used-x chwidth) (unsafe-fl+ maybe-used-y chheight))))
    bmp))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_composite (-> Integer Bitmap-Surface Flonum Flonum Bitmap-Surface Flonum Flonum Flonum Bitmap)]
 [bitmap_pin (-> Flonum Flonum Flonum Flonum Bitmap-Surface Bitmap-Surface Flonum Bitmap)]
 [bitmap_append (-> Symbol Bitmap-Surface (Listof Bitmap-Surface) Flonum Flonum Bitmap)])
