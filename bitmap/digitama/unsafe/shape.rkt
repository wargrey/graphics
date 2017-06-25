#lang typed/racket/base

(provide (all-defined-out))

(require "../draw.rkt")
(require "source.rkt")
(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require "paint.rkt")
  
  (define (bitmap_arc radius border background density [start 0.0] [end 2pi])
    (define fllength (unsafe-fl* radius 2.0))
    (define-values (img cr w h) (make-cairo-image fllength fllength density #true))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (cairo_translate cr radius radius)
    (cairo_arc cr 0.0 0.0 (unsafe-fl- radius (unsafe-fl/ line-width 2.0)) start end)
    (cairo-render cr border background)
    (cairo_destroy cr)
    img)

  (define (bitmap_elliptical_arc width height border background density [start 0.0] [end 2pi])
    (define-values (img cr w h) (make-cairo-image width height density #true))
    (define-values (width/2 height/2) (values (unsafe-fl/ width 2.0) (unsafe-fl/ height 2.0)))
    (define radius (unsafe-flmin width/2 height/2))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (cairo_translate cr width/2 height/2)
    (cairo_save cr)
    (if (unsafe-fl= radius height/2)
        (cairo_scale cr (unsafe-fl/ width height) 1.0)
        (cairo_scale cr 1.0 (unsafe-fl/ height width)))
    (cairo_arc cr 0.0 0.0 (unsafe-fl- radius (unsafe-fl/ line-width 2.0)) start end)
    (cairo_restore cr)
    (cairo-render cr border background)
    (cairo_destroy cr)
    img)
  
  (define (bitmap_rectangle flwidth flheight border background density)
    (define-values (img cr w h) (make-cairo-image flwidth flheight density #true))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define inset (unsafe-fl/ line-width 2.0))
    (cairo_rectangle cr inset inset (unsafe-fl- flwidth line-width) (unsafe-fl- flheight line-width))
    (cairo-render cr border background)
    (cairo_destroy cr)
    img)

  (define (bitmap_rounded_rectangle flwidth flheight radius border background density)
    (define-values (img cr w h) (make-cairo-image flwidth flheight density #true))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define inset (unsafe-fl/ line-width 2.0))
    (define flradius
      (let ([short (unsafe-flmin flwidth flheight)])
        (unsafe-flmin (unsafe-fl/ short 2.0) (radius-normalize radius short))))
    (define tlset (unsafe-fl+ inset flradius))
    (define xrset (unsafe-fl- (unsafe-fl- flwidth inset) flradius))
    (define ybset (unsafe-fl- (unsafe-fl- flheight inset) flradius))
    (cairo_new_sub_path cr) ; not neccessary
    (cairo_arc cr xrset tlset flradius -pi/2 0.0)
    (cairo_arc cr xrset ybset flradius 0.0   pi/2)
    (cairo_arc cr tlset ybset flradius pi/2  pi)
    (cairo_arc cr tlset tlset flradius pi    3pi/2)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)
    img)

  (define (bitmap_stadium fllength radius border background density)
    (define flradius (radius-normalize radius fllength))
    (define flheight (unsafe-fl* flradius 2.0))
    (define flwidth (unsafe-fl+ fllength flheight))
    (define-values (img cr w h) (make-cairo-image flwidth flheight density #true))
    (define line-width (if (struct? border) (unsafe-struct-ref border 1) 0.0))
    (define inset-radius (unsafe-fl- flradius (unsafe-fl/ line-width 2.0)))
    (cairo_new_sub_path cr) ; not neccessary
    (cairo_arc_negative cr flradius                       flradius inset-radius -pi/2 pi/2)
    (cairo_arc_negative cr (unsafe-fl+ flradius fllength) flradius inset-radius pi/2  3pi/2)
    (cairo_close_path cr)
    (cairo-render cr border background)
    (cairo_destroy cr)
    img)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (radius-normalize radius 100%)
    (define flradius (real->double-flonum radius))
    (if (single-flonum? radius) (unsafe-fl* flradius 100%) flradius)))

(unsafe/require/provide
 (submod "." unsafe)
 [bitmap_arc (->* (Flonum (Option Paint) (Option Bitmap-Source) Flonum) (Real Real) Bitmap)]
 [bitmap_elliptical_arc (->* (Flonum Flonum (Option Paint) (Option Bitmap-Source) Flonum) (Real Real) Bitmap)]
 [bitmap_rectangle (-> Flonum Flonum (Option Paint) (Option Bitmap-Source) Flonum Bitmap)]
 [bitmap_rounded_rectangle (-> Flonum Flonum Real (Option Paint) (Option Bitmap-Source) Flonum Bitmap)]
 [bitmap_stadium (-> Flonum Real (Option Paint) (Option Bitmap-Source) Flonum Bitmap)])
