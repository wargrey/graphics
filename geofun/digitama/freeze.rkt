#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/convert)

(require "../paint.rkt")
(require "../stroke.rkt")
(require "../font.rkt")

(require "base.rkt")
(require "source.rkt")
(require "convert.rkt")
(require "pattern.rkt")
(require "composite.rkt")
(require "paint.rkt")

(require "unsafe/source.rkt")
(require "unsafe/typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-freeze
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)] #:border [border : Maybe-Stroke-Paint (default-border-paint)]
           #:fill [fill : Option-Fill-Paint (default-fill-paint)] #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:font [font : Font (default-font)] #:font-paint [fgc : Option-Fill-Paint (default-font-paint)]
           #:background [bgc : Option-Fill-Paint (default-background-paint)]
           #:filter [filter : Geo-Pattern-Filter (default-pattern-filter)] #:operator [op : Geo-Pin-Operator (default-pin-operator)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [self : Geo<%>]] : Bitmap
    (define s : (Option Stroke) (stroke-paint->source* stroke))
    (parameterize ([default-stroke-source s]
                   [default-border-source (border-paint->source* border)]
                   [default-font-source (font-paint->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pattern-filter filter]
                   [default-pin-operator op]
                   [default-font font])
      (define thickness (stroke-maybe-width s))
      (define offset (* thickness 0.5))
      (define-values (flwidth flheight _ink) ((geo<%>-extent self) self))
      (define-values (bmp cr) (create-argb-bitmap (+ flwidth thickness) (+ flheight thickness) density #true))
      
      ((geo<%>-draw! self) self cr offset offset flwidth flheight)
      bmp)))

(define geo-freeze!
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)] #:border [border : Maybe-Stroke-Paint (default-border-paint)]
           #:fill [fill : Option-Fill-Paint (default-fill-paint)] #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:font [font : Font (default-font)] #:font-paint [fgc : Option-Fill-Paint (default-font-paint)]
           #:background [bgc : Option-Fill-Paint (default-background-paint)]
           #:filter [filter : Geo-Pattern-Filter (default-pattern-filter)] #:operator [op : Geo-Pin-Operator (default-pin-operator)]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [target : Bitmap] [self : Geo<%>] [dx : Real 0.0] [dy : Real 0.0]] : Void
    (parameterize ([default-stroke-source (stroke-paint->source* stroke)]
                   [default-border-source (border-paint->source* border)]
                   [default-font-source (font-paint->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pattern-filter filter]
                   [default-pin-operator op]
                   [default-font font])
      (define-values (flwidth flheight _ink) ((geo<%>-extent self) self))
      (define cr (cairo_create (bitmap-surface target)))

      (cairo-backend-scale cr (bitmap-density target) #true)
      ((geo<%>-draw! self) self cr (real->double-flonum dx) (real->double-flonum dy) flwidth flheight)
      (void))))
