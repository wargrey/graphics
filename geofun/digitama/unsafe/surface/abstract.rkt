#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../typed/cairo.rkt")
(require "../visual/ctype.rkt")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-cairo-abstract-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean (Gairo-Surface-Draw! False) Abstract-Surface)
  (lambda [flwidth flheight density scale? λmake]
    (define-values (surface cr width height) (cairo-create-abstract-surface* flwidth flheight density scale?))

    (start-breakable-atomic)
    (λmake #false cr 0.0 0.0 flwidth flheight)
    (end-breakable-atomic)

    surface))
