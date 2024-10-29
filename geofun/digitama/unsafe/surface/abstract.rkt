#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [cairo_recording_surface_get_extents abstract-surface-extent]
                     [cairo_recording_surface_ink_extents abstract-surface-bbox]))

(require typed/racket/unsafe)
(require "../typed/cairo.rkt")

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require "../cairo.rkt")

  (require racket/unsafe/ops)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define cairo-create-abstract-surface
    (lambda [flwidth flheight density]
      (define bounded? (and (unsafe-fl> flwidth 0.0) (unsafe-fl> flheight 0.0)))
      (define-values (width height)
        (cond [(not bounded?) (values +inf.0 +inf.0)]
              [else (values (unsafe-fl* flwidth density)
                            (unsafe-fl* flheight density))]))
      
      (define surface
        (cairo_recording_surface_create CAIRO_CONTENT_COLOR_ALPHA
                                        (and bounded? (make-cairo_rectangle_t 0.0 0.0 width height))))
      
      (let ([status (cairo_surface_status surface)])
        (unless (unsafe-fx= status CAIRO_STATUS_SUCCESS)
          (raise-arguments-error 'cairo-create-abstract-surface (cairo_status_to_string status)
                                 "width" flwidth "height" flheight "density" density)))
      
      (values surface width height)))

  (define cairo-create-abstract-surface*
    (lambda [flwidth flheight density scale?]
      (define-values (surface width height) (cairo-create-abstract-surface flwidth flheight density))
      (define cr (cairo_create surface))
      (unless (or (not scale?) (unsafe-fl= density 1.0))
        (cairo_scale cr density density))
      (values surface cr width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [cairo-create-abstract-surface (Cairo-Create-Surface Abstract-Surface)]
 [cairo-create-abstract-surface* (Cairo-Create-Surface+Ctx Abstract-Surface)])

(unsafe-require/typed/provide
 "../cairo.rkt"
 [cairo_recording_surface_get_extents (-> Abstract-Surface (Values (Option Flonum) Flonum Nonnegative-Flonum Nonnegative-Flonum))]
 [cairo_recording_surface_ink_extents (-> Abstract-Surface (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define abstract-surface-extent* : (-> Abstract-Surface (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [sfc]
    (define-values (?x y w h) (cairo_recording_surface_get_extents sfc))
  
    (if (not ?x)
        (cairo_recording_surface_ink_extents sfc)
        (values ?x y w h))))
