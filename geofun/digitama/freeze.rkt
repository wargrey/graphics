#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/convert)

(require "base.rkt")
(require "source.rkt")
(require "convert.rkt")

(require "unsafe/visual/abstract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-freeze : (->* (Geo<%>)
                          (#:border Stroke-Paint #:fill (Option Fill-Paint) #:fill-rule Symbol
                           #:option Any #:density Positive-Flonum)
                          Bitmap)
  (lambda [#:border [paint (default-stroke)] #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:option [opt (void)] #:density [density (default-bitmap-density)]
           self]
    (parameterize ([default-stroke (stroke-paint->source paint)]
                   [default-fill-rule rule]
                   [default-fill-paint fill])
      (define-values (self-sfc width height)
        (abstract-surface->image-surface
         (if (void? opt)
             ((geo<%>-surface self) self)
             ((geo<%>-surface self) self opt))
         density))
    
      (make-bitmap-from-image-surface self-sfc density width height))))

(define geo-freeze! : (->* (Bitmap Geo<%>)
                           (Real Real #:border Stroke-Paint #:fill (Option Fill-Paint) #:fill-rule Symbol #:option Any)
                           Void)
  (lambda [#:border [paint (default-stroke)] #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)] #:option [opt (void)]
           target self [dx 0.0] [dy 0.0]]
    (parameterize ([default-stroke (stroke-paint->source paint)]
                   [default-fill-rule rule]
                   [default-fill-paint fill])
      (abstract-surface-stamp-onto-bitmap-surface
       (bitmap-surface target)
       (if (void? opt)
           ((geo<%>-surface self) self)
           ((geo<%>-surface self) self opt))
       (real->double-flonum dx) (real->double-flonum dy)
       (bitmap-density target)))))
