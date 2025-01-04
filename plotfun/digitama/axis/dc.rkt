#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require geofun/font)
(require geofun/color)

(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/arc)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require "interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-plot-axis-real-dot-radius : (Parameterof Real) (make-parameter -0.05))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-plot-axis-digit->sticker : Plot-Axis-Digit->Sticker
  (lambda [id digit font color]
    (define label ((default-plot-axis-digit-filter) digit))

    (and label
         (geo-text label font #:color color))))

(define default-plot-axis-real->sticker : Plot-Axis-Real->Sticker
  (lambda [id real datum unit font color]
    (geo-text datum font #:color color)))

(define default-plot-axis-real->dot : Plot-Axis-Real->Dot
  (lambda [id real datum flunit color]
    (geo-circle #:fill color #:stroke #false
                (~length (default-plot-axis-real-dot-radius) flunit))))

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

(define plot-axis-real-label-values : (-> (Option Symbol) Flonum Any Geo-Pin-Anchor
                                       Plot-Axis-Real->Sticker Plot-Axis-Real->Dot Nonnegative-Flonum Font (Option Color) Color
                                       (Values (Pairof (U Geo Void False) Geo-Pin-Anchor)
                                               (Option (Pairof Geo Geo-Pin-Anchor))))
  (lambda [id val obj real-anchor real->label real->dot flunit font color axis-color]
    (define maybe-label
      (cond [(geo? obj) obj]
            [(and (pair? obj) (geo? (car obj)) (geo-pin-anchor? (cdr obj))) obj]
            [else (real->label id val obj flunit font (or color axis-color))]))
    (define dot (real->dot id val obj flunit (or color axis-color)))
    
    (values (cond [(pair? maybe-label) maybe-label]
                  [else (cons maybe-label real-anchor)])
            (cond [(geo? dot) (cons dot 'cc)]
                  [(pair? dot) dot]
                  [else #false]))))
