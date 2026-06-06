#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/palette/base.rkt"))
(provide (all-from-out "digitama/palette/oklch.rkt"))
(provide (all-from-out "digitama/palette/discrete.rkt"))

(require "color.rkt")

(require "digitama/palette/base.rkt")
(require "digitama/palette/oklch.rkt")
(require "digitama/palette/discrete.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define palette-generator-create
  (lambda [#:background [bg : (Option FlRGBA) #false]
           #:for [type : (U 'stroke 'fill) 'fill]
           #:start [start : Natural 0]
           #:lightness-scale [default-scale : Real 1.0]
           [palette : Palette-Index->Colors the-plot-oklch-palette]] : (-> [#:lightness-scale Real] FlRGBA)
    (define &idx : (Boxof Natural) (box start))

    (λ [#:lightness-scale [scale default-scale]]
      (define cpair (palette (unbox &idx) bg))
      (define c (if (eq? type 'fill) (cdr cpair) (car cpair)))
      
      (set-box! &idx (+ (unbox &idx) 1))
      (cond [(= scale 1.0) c]
            [(>= scale 0.0) (rgb* (oklch-modulate c #:lightness scale))]
            [else c]))))
