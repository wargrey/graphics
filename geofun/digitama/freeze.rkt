#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/convert)

(require "../paint.rkt")
(require "../font.rkt")

(require "base.rkt")
(require "source.rkt")
(require "convert.rkt")
(require "composite.rkt")
(require "dc/paint.rkt")
(require "unsafe/visual/abstract.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-freeze : (->* (Geo<%>)
                          (#:stroke Option-Stroke-Paint #:border Option-Stroke-Paint #:fill Option-Fill-Paint #:fill-rule Symbol
                           #:foreground Option-Fill-Paint #:background Option-Fill-Paint #:font Font #:operator Geo-Pin-Operator
                           #:density Positive-Flonum)
                          Bitmap)
  (lambda [#:stroke [stroke (default-stroke-paint)] #:border [border (default-border-paint)]
           #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:foreground [fgc (default-foreground-paint)] #:background [bgc (default-background-paint)]
           #:font [font (default-font)] #:operator [op (default-pin-operator)] #:density [density (default-bitmap-density)]
           self]
    (parameterize ([default-stroke-source (stroke-paint->source* stroke)]
                   [default-border-source (border-paint->source* border)]
                   [default-foreground-source (foreground->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pin-operator op]
                   [default-font font])
      (define-values (self-sfc width height)
        (abstract-surface->image-surface
         ((geo<%>-surface self) self)
         density))
    
      (make-bitmap-from-image-surface self-sfc density width height))))

(define geo-freeze! : (->* (Bitmap Geo<%>)
                           (Real Real
                                 #:stroke Option-Stroke-Paint #:border Option-Stroke-Paint #:fill Option-Fill-Paint #:fill-rule Symbol
                                 #:foreground Option-Fill-Paint #:background Option-Fill-Paint #:operator Geo-Pin-Operator)
                           Void)
  (lambda [#:stroke [stroke (default-stroke-paint)] #:border [border (default-border-paint)]
           #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:foreground [fgc (default-foreground-paint)] #:background [bgc (default-background-paint)]
           #:font [font (default-font)] #:operator [op (default-pin-operator)]
           target self [dx 0.0] [dy 0.0]]
    (parameterize ([default-stroke-source (stroke-paint->source* stroke)]
                   [default-border-source (border-paint->source* border)]
                   [default-foreground-source (foreground->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pin-operator op]
                   [default-font font])
      (abstract-surface-stamp-onto-bitmap-surface
       (bitmap-surface target)
       ((geo<%>-surface self) self)
       (real->double-flonum dx) (real->double-flonum dy)
       (bitmap-density target)))))
