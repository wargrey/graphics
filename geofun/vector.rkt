#lang typed/racket/base

(provide (all-defined-out) 2D-Radius-Type 3D-Radius-Type)

(require "digitama/base.rkt")
(require "digitama/misc.rkt")
(require "digitama/source.rkt")
(require "digitama/paint.rkt")
(require "digitama/convert.rkt")
(require "digitama/geometry/radius.rkt")

(require/provide "digitama/convert.rkt" "digitama/freeze.rkt")
(require/provide "path.rkt" "constructor.rkt" "composite.rkt" "resize.rkt")
(require/provide "color.rkt" "font.rkt" "paint.rkt" "stroke.rkt")
(require/provide "digitama/geometry/dot.rkt" "digitama/geometry/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-save
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (default-stroke-paint)]
           #:border [border : Maybe-Stroke-Paint (default-border-paint)]
           #:fill [fill : Option-Fill-Paint (default-fill-paint)]
           #:fill-rule [rule : Fill-Rule (default-fill-rule)]
           #:font [font : Font (default-font)]
           #:font-paint [fgc : Option-Fill-Paint (default-font-paint)]
           #:background [bgc : Option-Fill-Paint (default-background-paint)]
           #:filter [filter : Geo-Pattern-Filter (default-pattern-filter)]
           #:operator [op : Geo-Pin-Operator (default-pin-operator)]
           #:format [format : Symbol 'pdf]
           #:bitmap-density [density : Positive-Flonum (default-bitmap-density)]
           [self : Geo<%>] [/dev/geoout : (U Path-String Output-Port)]] : Void
    (parameterize ([default-stroke-source (stroke-paint->source* stroke)]
                   [default-border-source (border-paint->source* border)]
                   [default-font-source (font-paint->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pattern-filter filter]
                   [default-font font])
      (geo-object-save self /dev/geoout format density))))
