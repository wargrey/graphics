#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../base.rkt")
(require "convert.rkt")
(require "source.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "pangocairo.rkt")
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
        [(#\Z #\z) (cairo_close_path cr)]))

    (unless (eq? fill-style 'winding)
      (cairo_set_fill_rule cr CAIRO_FILL_RULE_EVEN_ODD))
    
    (cairo_path_extents cr)))

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

(struct path-arc
  ([cx : Float]
   [cy : Float]
   [r : Float]
   [start : Float]
   [end : Float])
  #:transparent)

(define path:none : Path-Args (path-none))
