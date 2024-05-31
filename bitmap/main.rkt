#lang typed/racket/base

(provide pi)

(require "digitama/misc.rkt")

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require/provide colorspace)
(require/provide "base.rkt" "effect.rkt")

(require/typed/provide
 "digitama/unsafe/constants.rkt"
 [-pi/2 Negative-Flonum]
 [pi/2 Positive-Flonum]
 [3pi/2 Positive-Flonum]
 [2pi Positive-Flonum]
 [pi/4 Positive-Flonum]
 [3pi/4 Positive-Flonum])
