#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/convert)

(require "base.rkt")
(require "source.rkt")
(require "convert.rkt")

(require "unsafe/visual/abstract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-freeze : (->* (Geo<%>)
                          (#:stroke (Option Stroke-Paint) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:fill-rule Symbol #:density Positive-Flonum)
                          Bitmap)
  (lambda [#:stroke [stroke (default-stroke)] #:border [border (default-border)] #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:density [density (default-bitmap-density)]
           self]
    (parameterize ([default-stroke (or (stroke-paint->source* stroke) (default-stroke))]
                   [default-border (or (stroke-paint->source* border) (default-border))]
                   [default-fill-rule rule]
                   [default-fill-paint fill])
      (define-values (self-sfc width height)
        (abstract-surface->image-surface
         ((geo<%>-surface self) self)
         density))
    
      (make-bitmap-from-image-surface self-sfc density width height))))

(define geo-freeze! : (->* (Bitmap Geo<%>)
                           (Real Real #:stroke (Option Stroke-Paint) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:fill-rule Symbol)
                           Void)
  (lambda [#:stroke [stroke (default-stroke)] #:border [border (default-border)] #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           target self [dx 0.0] [dy 0.0]]
    (parameterize ([default-stroke (or (stroke-paint->source* stroke) (default-stroke))]
                   [default-border (or (stroke-paint->source* border) (default-border))]
                   [default-fill-rule rule]
                   [default-fill-paint fill])
      (abstract-surface-stamp-onto-bitmap-surface
       (bitmap-surface target)
       ((geo<%>-surface self) self)
       (real->double-flonum dx) (real->double-flonum dy)
       (bitmap-density target)))))
