#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/color)
(require geofun/composite)

(require geofun/digitama/markup)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/arc)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/position)

(require "interface.rkt")
(require "../arithmetics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-plot-axis-tick->sticker : Plot-Axis-Tick->Sticker
  (lambda [id label font color]
    (geo-text label font #:color color)))

(define default-plot-axis-real->sticker : Plot-Axis-Real->Sticker
  (lambda [axis-id real datum flunit font color]
    (geo-text datum font #:color color)))

(define default-plot-axis-real->dot : Plot-Axis-Real->Dot
  (lambda [axis-id real datum flunit color radius]
    (geo-circle radius #:fill color #:stroke #false)))

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
          (geo-hc-append (geo-markup name font #:color color)
                         (geo-text " (" dfont #:color dcolor)
                         (geo-markup desc dfont #:color dcolor)
                         (geo-text ")" dfont #:color dcolor)))
        (geo-markup name font #:color color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-sticker-cons : (-> (U Geo Void False) Geo-Pin-Anchor Float-Complex Float-Complex (Listof (GLayerof Geo))
                                     Flonum (-> Float-Complex Flonum) Flonum
                                     (Option (Pairof Geo Geo-Pin-Anchor))
                                     (Listof (GLayerof Geo)))
  (lambda [sticker anchor pos offset digits tick-min part tick-max maybe-tick]
    (if (and (geo? sticker) (<= (scaled-round tick-min) (scaled-round (part (+ pos offset))) (scaled-round tick-max)))
        (cons (geo-own-pin-layer anchor pos sticker offset)
              (cond [(not maybe-tick) digits]
                    [else (cons (geo-own-pin-layer (cdr maybe-tick) pos
                                                   (car maybe-tick) 0.0+0.0i)
                                digits)]))
        digits)))

(define plot-axis-real-label-values : (-> (Option Symbol) Real Any Font Color Nonnegative-Flonum
                                          Plot-Axis-Real->Sticker Plot-Axis-Real->Dot Nonnegative-Flonum Geo-Pin-Anchor
                                          (Values (Pairof (U Geo Void False) Geo-Pin-Anchor)
                                                  (Option (Pairof Geo Geo-Pin-Anchor))))
  (lambda [id val obj font color radius real->label real->dot flunit anchor]
    (define maybe-label
      (cond [(geo? obj) obj]
            [(and (pair? obj) (geo? (car obj)) (geo-pin-anchor? (cdr obj))) obj]
            [else (real->label id val obj flunit font color)]))
    (define dot (real->dot id val obj flunit color radius))
    
    (values (cond [(pair? maybe-label) maybe-label]
                  [else (cons maybe-label anchor)])
            (cond [(geo? dot) (cons dot 'cc)]
                  [(pair? dot) dot]
                  [else #false]))))
