#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/convert)

(require "base.rkt")
(require "source.rkt")
(require "convert.rkt")
(require "composite.rkt")

(require "../stroke.rkt")
(require "../paint.rkt")

(require "unsafe/visual/abstract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-freeze : (->* (Geo<%>)
                          (#:stroke Maybe-Stroke-Paint #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:fill-rule Symbol
                           #:foreground Maybe-Fill-Paint #:background Maybe-Fill-Paint #:operator Geo-Pin-Operator
                           #:density Positive-Flonum)
                          Bitmap)
  (lambda [#:stroke [stroke (void)] #:border [border (void)] #:fill [fill (void)] #:fill-rule [rule (default-fill-rule)]
           #:foreground [fgc (void)] #:background [bgc (void)] #:operator [op (default-pin-operator)]
           #:density [density (default-bitmap-density)]
           self]
    (parameterize ([default-stroke-paint (stroke-paint->source* stroke)]
                   [default-border-paint (border-paint->source* border)]
                   ;[default-foreground-paint fgc]
                   ;[default-background-paint bgc]
                   [default-fill-rule rule]
                   [default-fill-paint fill]
                   [default-pin-operator op])
      (define-values (self-sfc width height)
        (abstract-surface->image-surface
         ((geo<%>-surface self) self)
         density))
    
      (make-bitmap-from-image-surface self-sfc density width height))))

(define geo-freeze! : (->* (Bitmap Geo<%>)
                           (Real Real
                                 #:stroke Maybe-Stroke-Paint #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:fill-rule Symbol
                                 #:foreground Maybe-Fill-Paint #:background Maybe-Fill-Paint #:operator Geo-Pin-Operator)
                           Void)
  (lambda [#:stroke [stroke (void)] #:border [border (void)] #:fill [fill (void)] #:fill-rule [rule (default-fill-rule)]
           #:foreground [fgc (void)] #:background [bgc (void)] #:operator [op (default-pin-operator)]
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
