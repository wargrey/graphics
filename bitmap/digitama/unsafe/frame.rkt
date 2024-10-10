#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require geofun/digitama/base)
(require geofun/digitama/unsafe/source)
(require geofun/digitama/unsafe/visual/ctype)

(require "../convert.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require geofun/digitama/unsafe/pangocairo)
  (require geofun/digitama/unsafe/paint)
  (require (submod geofun/digitama/unsafe/composite unsafe))

  (require "../convert.rkt")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (bitmap_frame src dest-width dest-height mtop mright mbottom mleft ptop pright pbottom pleft border background density)
    (define line-width (~bdwidth border))
    (define line-inset (unsafe-fl* line-width 0.5))
    (define-values (width border-x border-width dest-x) (frame-metrics line-width line-inset mleft mright pleft pright dest-width))
    (define-values (height border-y border-height dest-y) (frame-metrics line-width line-inset mtop mbottom ptop pbottom dest-height))
    (define-values (img cr) (create-argb-bitmap width height density #true))
    
    (cairo_rectangle cr border-x border-y border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src dest-x dest-y dest-width dest-height CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density)
    (cairo_destroy cr)

    img))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [bitmap_frame (All (S) (-> Bitmap-Surface
                            Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                            (Option Paint) (Option Fill-Source) Flonum Bitmap))])
