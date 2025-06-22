#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/color)
(require geofun/composite)

(require geofun/digitama/markup)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/edge)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require "interface.rkt")
(require "../arithmetics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-plot-axis-tick->sticker : Plot-Axis-Tick->Sticker
  (lambda [id label font color]
    (geo-text label font #:color color)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-x-axis-label : (-> DC-Markup-Text Font Color (Option DC-Markup-Text) (Option Font) (Option Color) Flonum Geo)
  (lambda [name font color desc desc-font desc-color gapsize]
    (cond [(zero? gapsize) (plot-y-axis-label name font color desc desc-font desc-color)]
          [else (geo-hc-append #:gapsize (- gapsize 1.0)
                               (geo-blank)
                               (plot-y-axis-label name font color desc desc-font desc-color)
                               (geo-blank))])))

(define plot-y-axis-label : (-> DC-Markup-Text Font Color (Option DC-Markup-Text) (Option Font) (Option Color) Geo)
  (lambda [name font color desc desc-font desc-color]
    (if (or desc)
        (let ([dfont (or desc-font font)]
              [dcolor (or desc-color color)])
          (geo-hc-append (geo-markup name font #:color color #:error-color 'GhostWhite #:error-background 'Firebrick)
                         (geo-text " (" dfont #:color dcolor)
                         (geo-markup desc dfont #:color dcolor #:error-color 'GhostWhite #:error-background 'Firebrick)
                         (geo-text ")" dfont #:color dcolor)))
        (geo-markup name font #:color color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-sticker-cons : (case-> [(U Geo Void False) Geo-Pin-Anchor Float-Complex Float-Complex (Listof (GLayerof Geo))
                                                             Flonum (-> Float-Complex Flonum) Flonum
                                                             (Option (Pairof Geo Geo-Pin-Anchor))
                                                             -> (Listof (GLayerof Geo))]
                                         [(GLayerof Geo) Float-Complex (Listof (GLayerof Geo)) Flonum (-> Float-Complex Flonum) Flonum
                                                         -> (Listof (GLayerof Geo))]
                                         [(GLayerof Geo) Float-Complex (Listof (GLayerof Geo)) (Option (Pairof Geo Geo-Pin-Anchor))
                                                         -> (Listof (GLayerof Geo))])
  (case-lambda
    [(self anchor pos offset stickers tick-min part tick-max maybe-tick)
     (if (and (geo? self) (<= (scaled-round tick-min) (scaled-round (part (+ pos offset))) (scaled-round tick-max)))
         (plot-axis-sticker-cons (geo-own-pin-layer anchor pos self offset) pos stickers maybe-tick)
         stickers)]
    [(self pos stickers tick-min part tick-max)
     (if (<= (scaled-round tick-min) (scaled-round (part pos)) (scaled-round tick-max))
         (plot-axis-sticker-cons self pos stickers #false)
         stickers)]
    [(self pos stickers maybe-tick)
     (cons self
           (cond [(not maybe-tick) stickers]
                 [else (cons (geo-own-pin-layer (cdr maybe-tick) pos
                                                (car maybe-tick) 0.0+0.0i)
                             stickers)]))]))
