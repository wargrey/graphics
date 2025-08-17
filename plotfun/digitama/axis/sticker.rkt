#lang typed/racket/base

(provide (all-defined-out))

(require digimon/flonum)

(require geofun/font)
(require geofun/color)
(require geofun/paint)
(require geofun/composite)

(require geofun/digitama/markup)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/path)
(require geofun/digitama/path/tick)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require geofun/digitama/geometry/computation/line)

(require "interface.rkt")
(require "../marker/dc.rkt")

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
(define plot-tick-db : (Weak-HashTable Any Geo:Path:Self) (make-weak-hash))

(define plot-axis-xtick-sticker : (->* (Flonum Geo-Tick-Placement Maybe-Stroke-Paint) (Flonum) (Option Geo:Path:Self))
  (lambda [size placement pen [α 0.0]]
    (and (> size 0.0)
         (hash-ref plot-tick-db (list 'x pen size α placement)
                    (λ [] (geo-path* #:stroke pen
                                     (geo-xtick-footprints size α
                                                           placement)))))))

(define plot-axis-ytick-sticker : (->* (Flonum Geo-Tick-Placement Maybe-Stroke-Paint) (Flonum) (Option Geo:Path:Self))
  (lambda [size placement pen [α 0.0]]
    (and (> size 0.0)
         (hash-ref! plot-tick-db (list 'y pen size α placement)
                    (λ [] (geo-path* #:stroke pen
                                     (geo-ytick-footprints size α
                                                           placement)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-sticker-cons
  : (case-> [(GLayerof Geo) Float-Complex (Listof (GLayerof Geo)) Flonum (-> Float-Complex Flonum) Flonum -> (Listof (GLayerof Geo))]
            [Plot:Marker (Listof (GLayerof Geo)) Flonum (-> Float-Complex Flonum) Flonum -> (Listof (GLayerof Geo))]
            [Plot:Marker (Listof (GLayerof Geo)) Float-Complex Float-Complex -> (Listof (GLayerof Geo))]
            
            [(U Geo Void False 'minor) Geo-Pin-Anchor Float-Complex Float-Complex (Listof (GLayerof Geo))
                                       Flonum (-> Float-Complex Flonum) Flonum (Option Geo-Path)
                                       -> (Listof (GLayerof Geo))]

            [(U Geo Void False 'minor) Geo-Pin-Anchor Float-Complex Float-Complex (Listof (GLayerof Geo)) Float-Complex Float-Complex (Option Geo-Path)
                                       -> (Listof (GLayerof Geo))])
  (case-lambda
    [(self anchor pos offset stickers tick-min part tick-max maybe-tick)
     (if (sfl<= tick-min (part pos) tick-max)
         (plot-axis-sticker-cons* self anchor pos offset stickers maybe-tick)
         stickers)]
    [(self pos stickers tick-min part tick-max)
     (if (sfl<= tick-min (part pos) tick-max)
         (plot-axis-sticker-cons* self pos stickers)
         stickers)]
    [(self stickers tick-min part tick-max)
     (let-values ([(src-pos end-pos) (geo-path-endpoints self)])
       (if (sfl<= tick-min (part src-pos) tick-max)
           (cons (geo-path-self-pin-layer self) stickers)
           stickers))]
    [(self anchor pos offset stickers tick-start tick-end maybe-tick)
     (if (geo-dot-on-line? pos tick-start tick-end)
         (plot-axis-sticker-cons* self anchor pos offset stickers maybe-tick)
         stickers)]
    [(self stickers tick-start tick-end)
     (let-values ([(src-pos end-pos) (geo-path-endpoints self)])
       (if (geo-dot-on-line? src-pos tick-start tick-end)
           (cons (geo-path-self-pin-layer self) stickers)
           stickers))]))

(define plot-axis-sticker-cons*
  : (case-> [(U Geo Void False 'minor) Geo-Pin-Anchor Float-Complex Float-Complex (Listof (GLayerof Geo)) (Option Geo-Path) -> (Listof (GLayerof Geo))]
            [(U Geo Void False 'minor) Geo-Pin-Anchor Float-Complex Float-Complex (Listof (GLayerof Geo)) -> (Listof (GLayerof Geo))]
            [(Option (GLayerof Geo)) Float-Complex (Listof (GLayerof Geo)) (Option Geo-Path) -> (Listof (GLayerof Geo))]
            [(Option (GLayerof Geo)) Float-Complex (Listof (GLayerof Geo)) -> (Listof (GLayerof Geo))])
  (case-lambda
    [(self anchor pos offset stickers maybe-tick)
     (cond [(geo? self) (plot-axis-sticker-cons* (geo-own-pin-layer anchor pos self offset) pos stickers maybe-tick)]
           [(eq? self 'minor) (plot-axis-sticker-cons* #false pos stickers maybe-tick)]
           [else stickers])]
    [(self pos stickers maybe-tick)
     (define tick+stickers
       (cond [(not maybe-tick) stickers]
             [else (cons (geo-path-self-pin-layer maybe-tick pos) stickers)]))
     (cond [(not self) tick+stickers]
           [else (cons self tick+stickers)])]
    [(self anchor pos offset stickers) (plot-axis-sticker-cons* self anchor pos offset stickers #false)]
    [(self pos stickers) (plot-axis-sticker-cons* self pos stickers #false)]))
