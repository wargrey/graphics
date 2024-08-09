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
                        (#:border Stroke-Paint #:fill (Option Fill-Paint) #:fill-rule Symbol
                         #:option Any #:format Symbol #:bitmap-density Positive-Flonum)
                        Void)
  (lambda [#:border [stroke (default-stroke)] #:fill [fill (default-fill-paint)] #:fill-rule [fill-rule (default-fill-rule)]
           #:option [opt (void)] #:format [format 'pdf] #:bitmap-density [density (default-bitmap-density)]
           self /dev/geoout]
    (parameterize ([default-stroke (stroke-paint->source stroke)]
                   [default-fill-paint fill]
                   [default-fill-rule fill-rule])
      (abstract-surface-save
       (if (void? opt)
           ((geo<%>-surface self) self)
           ((geo<%>-surface self) self opt))
       /dev/geoout format density))))
