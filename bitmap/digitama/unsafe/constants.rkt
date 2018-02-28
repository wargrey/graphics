#lang racket/base

(provide (all-defined-out))

(require "pangocairo.rkt")

(define-values (-pi/2 pi/2 3pi/2 2pi)
  (values (~radian -90.0)
          (~radian 90.0)
          (~radian 270.0)
          (unsafe-fl* pi 2.0)))
