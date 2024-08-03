#lang typed/racket

(provide (all-defined-out) Stroke-Paint Fill-Paint)

(require pangocairo/source)
(require pangocairo/digitama/base)
(require pangocairo/digitama/misc)
(require pangocairo/digitama/unsafe/visual/abstract)

(require/provide "digitama/convert.rkt" "digitama/freeze.rkt")
(require/provide "track.rkt" "constructor.rkt" "composite.rkt" "resize.rkt")
(require/provide pangocairo/color pangocairo/font pangocairo/paint)
(require/provide pangocairo/digitama/dot pangocairo/digitama/constants)

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
