#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide Plot:Axis plot:axis? plot-axis-real-values)
(provide default-plot-axis-digit->sticker default-plot-axis-real->dot)
(provide default-plot-axis-digit-filter default-plot-axis-real-filter)

(require digimon/metrics)

(require geofun/paint)
(require geofun/font)

(require geofun/constructor)

(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)

(require "digitama/axis/self.rkt")
(require "digitama/axis/interface.rkt")
(require "digitama/axis/dc.rkt")
(require "digitama/utility.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis
  (lambda [#:id [id : (Option Symbol) #false]
           #:axis-thickness [axis-thickness : Real 1.5]
           #:axis-paint [axis-paint : Fill-Paint 'black]
           #:axis-label [axis-label : (Option String) #false]
           #:axis-label-color [label-color : Option-Fill-Paint #false]
           #:tick-thickness [tick-thickness : Real 1.0]
           #:tick-length [tick-length : Real -3.0]
           #:tick-paint [tick-paint : Option-Fill-Paint #false]
           #:tick-range [tick-hint : (U Real (Pairof Real Real) (Listof Real)) null]
           #:tick-anchor [tick-anchor : Geo-Pin-Anchor 'cb]
           #:tick-digit-color [digit-color : Option-Fill-Paint #false]
           #:tick-digit-font [digit-font : Font (default-axis-digit-font)]
           #:tick-digit-position [digit-position : Real 0.618]
           #:tick-digit-anchor [digit-anchor : Geo-Pin-Anchor 'ct]
           #:tick-digit-filter [digit-filter : (Option (-> Integer (Option String))) #false]
           #:tick-digit->sticker [digit->label : Plot-Axis-Digit->Sticker default-plot-axis-digit->sticker]
           #:reals [real-list : (U (Listof Plot-Axis-Real-Datum) (Vectorof Any) (-> Integer Any)) null]
           #:real-color [real-color : Option-Fill-Paint #false]
           #:real-font [real-font : (Option Font) (default-axis-real-font)]
           #:real-position [real-position : Real -1.0]
           #:real-anchor [real-anchor : Geo-Pin-Anchor 'cb]
           #:real-filter [real-filter : (Option Plot-Axis-Real-Filter) #false]
           #:real->sticker [real->label : Plot-Axis-Real->Sticker default-plot-axis-real->sticker]
           #:real->dot [real->dot : Plot-Axis-Real->Dot default-plot-axis-real->dot]
           [length : Real] [origin : Real] [unit-length : Real -0.1]] : Plot:Axis
    (define fllength : Nonnegative-Flonum (~length length))
    (define flunit : Nonnegative-Flonum (~length unit-length fllength))
    (define flthickness : Nonnegative-Flonum (~length axis-thickness))
    (define fltick-thickness : Nonnegative-Flonum (~length tick-thickness flthickness))
    (define fltick-length : Nonnegative-Flonum (~length tick-length flthickness))
    (define fltick-min : Nonnegative-Flonum (* fltick-thickness 0.5))
    (define fltick-max : Nonnegative-Flonum (+ fltick-min fllength))
    (define tick-digits : (Listof Integer) (plot-axis-ticks tick-hint))
    (define flrhead : Nonnegative-Flonum (* flthickness 3.14))

    (define arrow : Geo (geo-arrow flrhead (+ fllength fltick-thickness) #:shaft-thickness flthickness #:stroke #false #:fill axis-paint))
    (define gtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-length #:stroke #false #:fill (or tick-paint axis-paint))
                 tick-anchor)))

    (define arrow-y : Nonnegative-Flonum (* (geo-height arrow) 0.5))
    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))
    (define flc-origin : Float-Complex (make-rectangular (+ (* fllength (real->double-flonum origin)) fltick-min) arrow-y))
    (define tick-offset : Float-Complex (make-rectangular 0.0 (* (real->double-flonum digit-position) em)))
    (define real-offset : Float-Complex (make-rectangular 0.0 (* (real->double-flonum real-position)  em)))

    (define layers : (Listof (GLayerof Geo))
      (parameterize ([default-plot-axis-real-filter (or real-filter (default-plot-axis-real-filter))]
                     [default-plot-axis-digit-filter (or digit-filter (default-plot-axis-digit-filter))])
        (append (cond [(not axis-label) null]
                      [else (let* ([f (desc-font digit-font #:family 'math)]
                                   [g (geo-text axis-label f #:color (or label-color axis-paint))])
                              (plot-axis-sticker-cons g digit-anchor (make-rectangular fltick-max arrow-y)
                                                      tick-offset null fltick-min fltick-max #false))])

                (for/fold ([ticks : (Listof (GLayerof Geo)) null])
                          ([tick (if (pair? tick-digits) (in-list tick-digits) (in-value 0))])
                  (plot-axis-sticker-cons (digit->label id tick digit-font (or digit-color axis-paint)) digit-anchor
                                          (+ flc-origin (* (exact->inexact tick) flunit))
                                          tick-offset ticks fltick-min fltick-max gtick))

                (for/fold ([reals : (Listof (GLayerof Geo)) null])
                          ([real (in-list (cond [(list? real-list) real-list]
                                                [(vector? real-list) (plot-axis-reals-from-vector real-list)]
                                                [else (plot-axis-reals-from-producer real-list tick-digits)]))])
                  (define-values (val obj) ((default-plot-axis-real-filter) real))
                  
                  (if (or val)
                      (let-values ([(label dot) (plot-axis-real-label-values id val obj real-anchor real->label real->dot flunit
                                                                             (or real-font digit-font) real-color axis-paint)])
                        (plot-axis-sticker-cons (car label) (cdr label)
                                                (+ flc-origin (* val flunit))
                                                real-offset reals fltick-min fltick-max dot))
                      reals)))))

    (define translated-layers : (Option (GLayer-Groupof Geo)) (geo-path-try-extend/list (geo-own-layer arrow) layers))

    (if (or translated-layers)
        (let ([delta (geo-layer-position (car (glayer-group-layers translated-layers)))])
          (create-geometry-group plot:axis id #false #false translated-layers (+ flc-origin delta) tick-digits))
        (create-geometry-group plot:axis id #false #false (geo-own-layers arrow) flc-origin tick-digits))))
