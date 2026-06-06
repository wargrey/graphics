#lang typed/racket/base

(provide (all-defined-out))

(require colorspace/ok)

(require "../../color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define oklch-background-extract : (-> (Option Color) (Values Flonum Flonum Flonum))
  (lambda [bg]
    (cond [(not bg) (values 1.0 0.0 +nan.0)]
          [(rgba? bg) (rgb->oklch (rgba-red bg) (rgba-green bg) (rgba-blue bg))]
          [(oklcha? bg) (values (oklcha-lightness bg) (oklcha-chroma bg) (oklcha-hue bg))]
          [else (oklch-background-extract (rgb* bg))])))
