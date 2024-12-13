#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../typed/cairo.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/unsafe/ops)

  (require "../cairo.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define cairo-create-argb-image-surface
    ; NOTE: (cairo_image_surface_create_for_data) does not work here since Racket bytes might be moved by GC.
    ; NOTE: CAIRO_FORMAT_ARGB32 is alpha-multiplied.
    (lambda [flwidth flheight density]
      (define width (unsafe-fxmax (unsafe-fl->fx (unsafe-flceiling (unsafe-fl* flwidth density))) 1))
      (define height (unsafe-fxmax (unsafe-fl->fx (unsafe-flceiling (unsafe-fl* flheight density))) 1))
      (define surface (cairo_image_surface_create CAIRO_FORMAT_ARGB32 width height))
      
      (let ([status (cairo_surface_status surface)])
        (unless (unsafe-fx= status CAIRO_STATUS_SUCCESS)
          (raise-arguments-error 'cairo-create-argb-image-surface
                                 (cairo_status_to_string status)
                                 "width" flwidth "height" flheight
                                 "density" density)))
      
      (values surface width height)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-values (the-image-surface the-image-cairo)
    (let-values ([(sfc w h) (cairo-create-argb-image-surface 1.0 1.0 1.0)])
      (values sfc (cairo_create sfc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [cairo-create-argb-image-surface
  (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum
      (Values Bitmap-Surface Positive-Index Positive-Index))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-create-argb-image-surface* : (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean
                                               (Values Bitmap-Surface Cairo-Ctx Positive-Index Positive-Index))
  (lambda [flwidth flheight density scale?]
    (define-values (surface width height) (cairo-create-argb-image-surface flwidth flheight density))
    (define cr (cairo_create surface))
    
    (cairo-backend-scale cr density scale?)
    (values surface cr width height)))
