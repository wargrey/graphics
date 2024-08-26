#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../../base.rkt")
(require "../source.rkt")
(require "../visual/ctype.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "../pangocairo.rkt")
  (require "../paint.rkt")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (dc_blank create-surface width height density)
    (define-values (img cr) (create-surface width height density #true))
    (cairo_destroy cr)
    img)

  (define (dc_pattern create-surface width height background density)
    (define-values (img cr) (create-surface width height density #true))
    (cairo-render-background cr background)
    (cairo_destroy cr)
    img)

  (define (dc_frame create-surface src dest-width dest-height mtop mright mbottom mleft ptop pright pbottom pleft border background density)
    (define line-width (~bdwidth border))
    (define line-inset (unsafe-fl* line-width 0.5))
    (define-values (width border-x border-width dest-x) (frame-metrics line-width line-inset mleft mright pleft pright dest-width))
    (define-values (height border-y border-height dest-y) (frame-metrics line-width line-inset mtop mbottom ptop pbottom dest-height))
    (define-values (img cr) (create-surface width height density #true))
    (cairo_rectangle cr border-x border-y border-width border-height)
    (cairo-render cr border background)
    (cairo-composite cr src dest-x dest-y dest-width dest-height CAIRO_FILTER_BILINEAR CAIRO_OPERATOR_OVER density)
    (cairo_destroy cr)
    img)

  (define (dc_frame_size dest-width dest-height mtop mright mbottom mleft ptop pright pbottom pleft border)
    (define line-width (~bdwidth border))
    (define line-inset (unsafe-fl* line-width 0.5))
    (define-values (width border-x border-width dest-x) (frame-metrics line-width line-inset mleft mright pleft pright dest-width))
    (define-values (height border-y border-height dest-y) (frame-metrics line-width line-inset mtop mbottom ptop pbottom dest-height))

    (values width height border-x border-y border-width border-height))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (frame-metrics line-width line-inset flmopen flmclose flpopen flpclose size)
    (define border-position (unsafe-fl+ flmopen line-inset))
    (define position (unsafe-fl+ (unsafe-fl+ flmopen flpopen) line-width))
    (define border-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ flpopen flpclose) size) line-width))
    (define frame-size (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ border-position border-size) flmclose) line-inset))
    (values frame-size border-position border-size position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [dc_blank (All (S) (-> (Cairo-Surface-Create S) Flonum Flonum Flonum S))]
 [dc_pattern (All (S) (-> (Cairo-Surface-Create S) Flonum Flonum Fill-Source Flonum S))]
 [dc_frame (All (S) (-> (Cairo-Surface-Create S) (U Bitmap-Surface Abstract-Surface)
                        Nonnegative-Flonum Nonnegative-Flonum
                        Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                        Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                        (Option Paint) (Option Fill-Source) Flonum S))]

 [dc_frame_size (-> Nonnegative-Flonum Nonnegative-Flonum
                    Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                    Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum
                    (Option Paint)
                    (Values Nonnegative-Flonum Nonnegative-Flonum
                            Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum))])
