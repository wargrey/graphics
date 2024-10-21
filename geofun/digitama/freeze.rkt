#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/convert)

(require "../paint.rkt")
(require "../stroke.rkt")
(require "../font.rkt")

(require "base.rkt")
(require "source.rkt")
(require "convert.rkt")
(require "composite.rkt")
(require "dc/paint.rkt")
(require "unsafe/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-freeze : (->* (Geo<%>)
                          (#:stroke Option-Stroke-Paint #:border Option-Stroke-Paint #:fill Option-Fill-Paint #:fill-rule Symbol
                           #:font-paint Option-Fill-Paint #:background Option-Fill-Paint #:font Font #:operator Geo-Pin-Operator
                           #:density Positive-Flonum)
                          Bitmap)
  (lambda [#:stroke [stroke (default-stroke-paint)] #:border [border (default-border-paint)]
           #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:font-paint [fgc (default-font-paint)] #:background [bgc (default-background-paint)]
           #:font [font (default-font)] #:operator [op (default-pin-operator)] #:density [density (default-bitmap-density)]
           self]
    (define s : (Option Stroke) (stroke-paint->source* stroke))
    (parameterize ([default-stroke-source s]
                   [default-border-source (border-paint->source* border)]
                   [default-font-source (font-paint->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pin-operator op]
                   [default-font font])
      (define thickness (stroke-maybe-width s))
      (define offset (* thickness 0.5))
      (define-values (flwidth flheight _ink) ((geo<%>-extent self) self))
      (define-values (bmp cr) (create-argb-bitmap (+ flwidth thickness) (+ flheight thickness) density #true))
      
      ((geo<%>-draw! self) self cr offset offset flwidth flheight)
      (cairo_destroy cr)

      bmp)))

(define geo-freeze! : (->* (Bitmap Geo<%>)
                           (Real Real
                                 #:stroke Option-Stroke-Paint #:border Option-Stroke-Paint #:fill Option-Fill-Paint #:fill-rule Symbol
                                 #:fontcolor Option-Fill-Paint #:background Option-Fill-Paint #:operator Geo-Pin-Operator)
                           Void)
  (lambda [#:stroke [stroke (default-stroke-paint)] #:border [border (default-border-paint)]
           #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:fontcolor [fgc (default-font-paint)] #:background [bgc (default-background-paint)]
           #:font [font (default-font)] #:operator [op (default-pin-operator)]
           target self [dx 0.0] [dy 0.0]]
    (parameterize ([default-stroke-source (stroke-paint->source* stroke)]
                   [default-border-source (border-paint->source* border)]
                   [default-font-source (font-paint->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pin-operator op]
                   [default-font font])
      (define-values (flwidth flheight _ink) ((geo<%>-extent self) self))
      (define cr (cairo_create (bitmap-surface target)))

      (cairo-backend-scale cr (bitmap-density target) #true)
      ((geo<%>-draw! self) self cr (real->double-flonum dx) (real->double-flonum dy) flwidth flheight)
      (cairo_destroy cr))))
