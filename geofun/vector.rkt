#lang typed/racket

(provide (all-defined-out) Stroke-Paint Fill-Paint)

(require bitmap/digitama/misc)
(require bitmap/digitama/base)
(require bitmap/digitama/source)
(require bitmap/digitama/unsafe/visual/abstract)

(require/provide "digitama/unsafe/convert.rkt" "digitama/freeze.rkt")
(require/provide "track.rkt" "constructor.rkt" "composite.rkt" "resize.rkt")
(require/provide bitmap/color bitmap/font bitmap/paint)
(require/provide bitmap/digitama/dot bitmap/digitama/unsafe/constants)

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
