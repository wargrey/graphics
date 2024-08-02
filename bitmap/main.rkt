#lang typed/racket/base

(provide (all-defined-out) Stroke-Paint Fill-Paint)

(require "digitama/misc.rkt")
(require "digitama/source.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require/provide colorspace)
(require/provide geofun/misc)

(require/provide "digitama/base.rkt" "digitama/dot.rkt" "digitama/unsafe/constants.rkt")
(require/provide "base.rkt" "effect.rkt")
(require/provide "color.rkt" "font.rkt" "paint.rkt")
