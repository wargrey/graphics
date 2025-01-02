#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/arc)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require "interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-plot-axis-digit->sticker : Plot-Axis-Digit->Sticker
  (lambda [id digit font color]
    (define label ((default-plot-axis-digit-filter) digit))

    (and label
         (geo-text label font #:color color))))

(define default-plot-axis-real->sticker : Plot-Axis-Real->Sticker
  (lambda [id real datum font color]
    (geo-text datum font #:color color)))

(define default-plot-axis-real->dot : Plot-Axis-Real->Dot
  (lambda [id real datum axis-thickness color]
    (geo-circle axis-thickness #:fill color #:stroke #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-sticker-cons : (-> (U Geo Void False) Geo-Pin-Anchor Float-Complex Float-Complex (Listof (GLayerof Geo))
                                     Nonnegative-Flonum Nonnegative-Flonum
                                     (Option (Pairof Geo Geo-Pin-Anchor))
                                     (Listof (GLayerof Geo)))
  (lambda [sticker anchor pos offset digits tick-min tick-max maybe-tick]
    (if (and (geo? sticker) (<= tick-min (real-part pos) tick-max))
        (cons (geo-own-pin-layer anchor pos sticker offset)
              (cond [(not maybe-tick) digits]
                    [else (cons (geo-own-pin-layer (cdr maybe-tick) pos
                                                   (car maybe-tick) 0.0+0.0i)
                                digits)]))
        digits)))
