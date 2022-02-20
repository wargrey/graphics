#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")
(require "convert.rkt")
(require "source.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
  (require "constants.rkt")
  (require "paint.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_crawl! src key-nodes stroke fill-color dx dy fill-style density)
    (define cr (cairo_create src))
    (define-values (x1 y1 x2 y2) (cairo_path cr key-nodes dx dy fill-style density))

    (cairo-render cr stroke fill-color)
    (cairo_destroy cr)
    (values (make-rectangular x1 y1)
            (make-rectangular x2 y2)))

  (define (bitmap_crawl key-nodes flwidth flheight dx dy stroke fill-color fill-style density)
    (define line-width (if (struct? stroke) (unsafe-struct-ref stroke 1) 0.0))
    (define inset (unsafe-fl* line-width 0.5))
    (define-values (img cr) (make-cairo-image (unsafe-fl+ flwidth line-width) (unsafe-fl+ flheight line-width) density #false))
    (define-values (x1 y1 x2 y2) (cairo_path cr key-nodes (unsafe-fl+ dx inset) (unsafe-fl+ dy inset) fill-style density))

    (cairo-render cr stroke fill-color)
    (cairo_destroy cr)
    (values img (make-rectangular x1 y1) (make-rectangular x2 y2)))

  (define (cairo_path cr key-nodes dx dy fill-style density)
    (cairo_scale cr density density)
    
    (for ([op+node (in-list (reverse key-nodes))])
      (define node (cdr op+node))
      (case (car op+node)
        [(#\M) (cairo_move_to cr (unsafe-fl+ (real-part node) dx) (unsafe-fl+ (imag-part node) dy))]
        [(#\m) (cairo_rel_move_to cr (real-part node) (imag-part node))]
        [(#\L) (cairo_line_to cr (unsafe-fl+ (real-part node) dx) (unsafe-fl+ (imag-part node) dy))]
        [(#\l) (cairo_rel_line_to cr (real-part node) (imag-part node))]
        [(#\A) (cairo_elliptical_arc cr node #true dx dy)]
        [(#\Z #\z) (cairo_close_path cr)]))

    (unless (eq? fill-style 'winding)
      (cairo_set_fill_rule cr CAIRO_FILL_RULE_EVEN_ODD))
    
    (cairo_path_extents cr))

  (define (cairo_elliptical_arc cr path:arc radian? dx dy)
    (define cx (unsafe-fl+ (unsafe-struct*-ref path:arc 0) dx))
    (define cy (unsafe-fl+ (unsafe-struct*-ref path:arc 1) dy))
    (define rx (unsafe-struct*-ref path:arc 2))
    (define ry (unsafe-struct*-ref path:arc 3))
    (define start (unsafe-struct*-ref path:arc 4))
    (define end (unsafe-struct*-ref path:arc 5))
    (define path-arc (if (unsafe-struct*-ref path:arc 6) cairo_arc cairo_arc_negative))
    (define-values (rstart rend) (if radian? (values start end) (values (~radian start) (~radian end))))
    
    (cond [(unsafe-fl< rx ry)
           (cairo_save cr)
           (cairo_scale cr 1.0 (unsafe-fl/ ry rx))
           (path-arc cr cx cy rx rstart rend)
           (cairo_restore cr)]
          [(unsafe-fl> rx ry)
           (cairo_save cr)
           (cairo_scale cr (unsafe-fl/ rx ry) 1.0)
           (path-arc cr cx cy ry rstart rend)
           (cairo_restore cr)]
          [else (path-arc cr cx cy rx rstart rend)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap_crawl! (-> Bitmap-Surface (Listof (Pairof Char (U Float-Complex Path-Args))) (Option Paint) (Option Bitmap-Source) Flonum Flonum Symbol Flonum
                    (Values Float-Complex Float-Complex))]
 [bitmap_crawl (-> (Listof (Pairof Char (U Float-Complex Path-Args))) Flonum Flonum Flonum Flonum (Option Paint) (Option Bitmap-Source) Symbol Flonum
                   (Values Bitmap Float-Complex Float-Complex))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct path-args () #:type-name Path-Args #:transparent)
(struct path-none path-args () #:transparent)

(struct path-arc path-args
  ([cx : Float]
   [cy : Float]
   [rx : Float]
   [ry : Float]
   [start : Float]
   [end : Float]
   [clockwise? : Boolean])
  #:transparent)

(define path:none : Path-Args (path-none))
