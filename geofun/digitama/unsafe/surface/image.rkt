#lang racket/base

(provide (all-defined-out))

(require "../pangocairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-create-image-surface
  ; NOTE: (cairo_image_surface_create_for_data) does not work here since Racket bytes might be moved by GC.
  ; NOTE: CAIRO_FORMAT_ARGB32 is alpha-multiplied.
  (lambda [flwidth flheight density]
    (define width (unsafe-fxmax (~fx (unsafe-fl* flwidth density)) 1))
    (define height (unsafe-fxmax (~fx (unsafe-fl* flheight density)) 1))
    (define surface (cairo_image_surface_create CAIRO_FORMAT_ARGB32 width height))
    
    (let ([status (cairo_surface_status surface)])
      (unless (unsafe-fx= status CAIRO_STATUS_SUCCESS)
        (raise-arguments-error 'cairo-create-image-surface
                               (cairo_status_to_string status)
                               "width" flwidth "height" flheight
                               "density" density)))
    
    (values surface width height)))

(define cairo-create-argb-image-surface*
  (lambda [flwidth flheight density scale?]
    (define-values (surface width height) (cairo-create-image-surface flwidth flheight density))
    (define cr (cairo_create surface))
    (unless (or (not scale?) (unsafe-fl= density 1.0))
      (cairo_scale cr density density))
    (values surface cr width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (the-image-surface the-image-cairo)
  (let-values ([(sfc cr w h) (cairo-create-argb-image-surface* 1.0 1.0 1.0 #false)])
    (values sfc cr)))
