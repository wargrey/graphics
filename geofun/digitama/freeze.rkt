#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/convert)

(require "../paint.rkt")
(require "../stroke.rkt")
(require "../font.rkt")

(require "base.rkt")
(require "paint.rkt")
(require "source.rkt")
(require "convert.rkt")
(require "composite.rkt")

(require "unsafe/typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-freeze
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)] #:border [border : Maybe-Stroke-Paint (default-border-paint)]
           #:fill [fill : Option-Fill-Paint (default-fill-paint)] #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:font [font : Font (default-font)] #:font-paint [fgc : Option-Fill-Paint (default-font-paint)]
           #:background [bgc : Option-Fill-Paint (default-background-paint)]
           #:filter [filter : Geo-Pattern-Filter (default-pattern-filter)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [self : Geo<%>]] : Bitmap
    (define s : (Option Stroke) (stroke-paint->source* stroke))
    (define b : (Option Stroke) (border-paint->source* border))
    (parameterize ([default-stroke-source s]
                   [default-border-source b]
                   [default-font-source (font-paint->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pattern-filter filter]
                   [default-font font])
      (define-values (xoff yoff width height Width Height) (geo-surface-region self))
      (define-values (bmp cr) (create-argb-bitmap Width Height density #true))

      ((geo<%>-draw! self) self cr xoff yoff width height)
      (cairo_destroy cr)
      bmp)))

(define geo-freeze!
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)] #:border [border : Maybe-Stroke-Paint (default-border-paint)]
           #:fill [fill : Option-Fill-Paint (default-fill-paint)] #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:font [font : Font (default-font)] #:font-paint [fgc : Option-Fill-Paint (default-font-paint)]
           #:background [bgc : Option-Fill-Paint (default-background-paint)]
           #:filter [filter : Geo-Pattern-Filter (default-pattern-filter)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [target : Bitmap] [self : Geo<%>] [dx : Real 0.0] [dy : Real 0.0]] : Void
    (parameterize ([default-stroke-source (stroke-paint->source* stroke)]
                   [default-border-source (border-paint->source* border)]
                   [default-font-source (font-paint->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pattern-filter filter]
                   [default-font font])
      (define-values (flwidth flheight _ink) ((geo<%>-extent self) self))
      (define cr (cairo_create (bitmap-surface target)))

      (cairo-backend-scale cr (bitmap-density target) #true)
      ((geo<%>-draw! self) self cr (real->double-flonum dx) (real->double-flonum dy) flwidth flheight)
      (cairo_destroy cr))))
