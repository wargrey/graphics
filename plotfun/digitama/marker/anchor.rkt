#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)

(require geofun/digitama/layer/type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Mark-Auto-Anchor (-> Float-Complex (Option Flonum) Geo-Pin-Anchor))

(define plot-mark-auto-anchor : Plot-Mark-Auto-Anchor
  (let* ([+boundary (* pi/2 1/phi)]
         [-boundary (- +boundary)])
    (lambda [pos rad]
      (cond [(not rad) 'cb]
            [(<= +boundary rad (+ pi -boundary)) 'ct]
            [(<= (- +boundary pi) rad -boundary) 'cb]
            [(< -boundary rad +boundary) 'lc]
            [else 'rc]))))
