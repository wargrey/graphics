#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/palette/base.rkt"))
(provide (all-from-out "digitama/palette/oklch.rkt"))
;(provide (all-from-out "digitama/palette/discrete.rkt"))

(require "color.rkt")

(require "digitama/palette/base.rkt")
(require "digitama/palette/oklch.rkt")
;(require "digitama/palette/discrete.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define palette-generator-create
  (lambda [#:background [bg : (Option FlRGBA) #false]
           #:for [type : (U 'stroke 'fill) 'fill]
           #:start [start : Natural 0]
           [palette : Palette-Index->Colors the-plot-oklch-palette]] : (-> [#:scale (Option Nonnegative-Real)] FlRGBA)
    (define &idx : (Boxof Natural) (box start))

    (λ [#:scale [scale #false]]
      (define cpair (palette (unbox &idx) bg))
      (define c (if (eq? type 'fill) (cdr cpair) (car cpair)))
      
      (set-box! &idx (+ (unbox &idx) 1))
      (cond [(not scale) c]
            [(= scale 1.0) c]
            [else (rgb-transform-scale c scale)]))))
