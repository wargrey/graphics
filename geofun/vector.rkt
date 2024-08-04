#lang typed/racket

(provide (all-defined-out) Stroke-Paint Fill-Paint)

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
                        (#:color Stroke-Paint #:fill (Option Fill-Paint) #:fill-rule Symbol
                         #:format Symbol #:bitmap-density Positive-Flonum)
                        Void)
  (lambda [#:color [stroke (default-stroke)] #:fill [fill #false] #:fill-rule [fill-rule 'winding]
           #:format [format 'pdf] #:bitmap-density [density (default-bitmap-density)]
           geo /dev/geoout]
    (abstract-surface-save ((geo<%>-surface geo) geo stroke fill fill-rule)
                           /dev/geoout format density)))
