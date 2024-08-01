#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/unsafe/convert)
(require bitmap/digitama/unsafe/visual/abstract)

(require bitmap/digitama/base)
(require bitmap/digitama/source)

(require "unsafe/convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-freeze : (->* (Geo<%>)
                          (#:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol #:density Positive-Flonum)
                          Bitmap)
  (lambda [#:color [paint (default-stroke)] #:fill [fill #false] #:fill-style [fstyle 'winding] #:density [density (default-bitmap-density)]
           self]
    (define abs-sfc ((geo<%>-surface self) self paint fill fstyle))
    (define-values (self-sfc width height) (abstract-surface->image-surface abs-sfc density))
    
    (make-bitmap-from-image-surface self-sfc density width height)))

(define geo-freeze! : (->* (Bitmap Geo<%>)
                           (Real Real #:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-style Symbol)
                           Void)
  (lambda [#:color [paint (default-stroke)] #:fill [fill #false] #:fill-style [fstyle 'winding]
           target self [dx 0.0] [dy 0.0]]
    (define abs-sfc ((geo<%>-surface self) self paint fill fstyle))
    (abstract-surface-stamp-onto-bitmap-surface
     (bitmap-surface target) abs-sfc
     (real->double-flonum dx) (real->double-flonum dy)
     (bitmap-density target))))
