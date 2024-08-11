#lang typed/racket

(provide (all-defined-out) Stroke-Paint Fill-Paint)
(provide default-fill-paint default-fill-rule)
(provide default-composition-operator)

(require "digitama/base.rkt")
(require "digitama/misc.rkt")
(require "digitama/source.rkt")
(require "digitama/unsafe/visual/abstract.rkt")

(require/provide "digitama/convert.rkt" "digitama/freeze.rkt")
(require/provide "track.rkt" "constructor.rkt" "composite.rkt" "resize.rkt")
(require/provide "color.rkt" "font.rkt" "paint.rkt")
(require/provide "digitama/dot.rkt" "digitama/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-save : (->* (Geo<%> (U Path-String Output-Port))
                        (#:stroke (Option Stroke-Paint) #:border (Option Stroke-Paint) #:fill (Option Fill-Paint) #:fill-rule Symbol
                         #:format Symbol #:bitmap-density Positive-Flonum)
                        Void)
  (lambda [#:stroke [stroke (default-stroke)] #:border [border (default-border)]
           #:fill [fill (default-fill-paint)] #:fill-rule [fill-rule (default-fill-rule)]
           #:format [format 'pdf] #:bitmap-density [density (default-bitmap-density)]
           self /dev/geoout]
    (parameterize ([default-stroke (or (stroke-paint->source* stroke) (default-stroke))]
                   [default-border (or (stroke-paint->source* border) (default-border))]
                   [default-fill-paint fill]
                   [default-fill-rule fill-rule])
      (abstract-surface-save
       ((geo<%>-surface self) self)
       /dev/geoout format density))))
