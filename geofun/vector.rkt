#lang typed/racket/base

(provide (all-defined-out) 2D-Radius-Type 3D-Radius-Type)

(require "digitama/base.rkt")
(require "digitama/misc.rkt")
(require "digitama/source.rkt")
(require "digitama/dc/paint.rkt")
(require "digitama/geometry/radius.rkt")
(require "digitama/unsafe/visual/abstract.rkt")

(require/provide "digitama/convert.rkt" "digitama/freeze.rkt")
(require/provide "path.rkt" "constructor.rkt" "composite.rkt" "resize.rkt")
(require/provide "color.rkt" "font.rkt" "paint.rkt" "stroke.rkt")
(require/provide "digitama/geometry/dot.rkt" "digitama/geometry/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-save : (->* (Geo<%> (U Path-String Output-Port))
                        (#:stroke Option-Stroke-Paint #:border Option-Stroke-Paint #:fill Option-Fill-Paint #:fill-rule Symbol
                         #:font-paint Option-Fill-Paint #:background Option-Fill-Paint #:font Font #:operator Geo-Pin-Operator
                         #:format Symbol #:bitmap-density Positive-Flonum)
                        Void)
  (lambda [#:stroke [stroke (default-stroke-paint)] #:border [border (default-border-paint)]
           #:fill [fill (default-fill-paint)] #:fill-rule [rule (default-fill-rule)]
           #:font-paint [fgc (default-font-paint)] #:background [bgc (default-background-paint)]
           #:font [font (default-font)] #:operator [op (default-pin-operator)]
           #:format [format 'pdf] #:bitmap-density [density (default-bitmap-density)]
           self /dev/geoout]
    (parameterize ([default-stroke-source (stroke-paint->source* stroke)]
                   [default-border-source (border-paint->source* border)]
                   [default-font-source (font-paint->source fgc)]
                   [default-background-source (fill-paint->source* bgc)]
                   [default-fill-source (fill-paint->source* fill)]
                   [default-fill-rule rule]
                   [default-pin-operator op]
                   [default-font font])
      (abstract-surface-save
       ((geo<%>-surface self) self)
       /dev/geoout format density))))
