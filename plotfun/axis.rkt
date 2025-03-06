#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide Plot:Axis plot:axis? plot-axis-real-values)
(provide default-plot-axis-digit->sticker default-plot-axis-real->dot)
(provide default-plot-axis-digit-filter default-plot-axis-real-filter)
(provide default-plot-axis-datum-dot-radius)

(require digimon/metrics)
(require digimon/constant)

(require geofun/color)
(require geofun/font)

(require geofun/constructor)

(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/position)

(require "digitama/axis/self.rkt")
(require "digitama/axis/interface.rkt")
(require "digitama/axis/dc.rkt")
(require "digitama/utility.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis
  (lambda [#:id [id : (Option Symbol) #false]
           #:axis-thickness [axis-thickness : Real 1.5]
           #:axis-color [axis-color : Color (default-axis-color)]
           #:axis-label [axis-label : (Option String) #false]
           #:axis-label-font [label-font : (Option Font) (default-axis-label-font)]
           #:axis-label-color [label-color : (Option Color) #false]
           #:tick-thickness [tick-thickness : Real 1.0]
           #:tick-length [tick-length : Real -3.0]
           #:tick-color [tick-color : (Option Color) #false]
           #:tick-range [tick-hint : (U Real (Pairof Real Real) (Listof Real)) null]
           #:tick-anchor [tick-anchor : Geo-Pin-Anchor 'cb]
           #:tick-digit-color [digit-color : (Option Color) #false]
           #:tick-digit-font [digit-font : Font (default-axis-digit-font)]
           #:tick-digit-position [digit-position : Real -0.618]
           #:tick-digit-filter [digit-filter : (Option (-> Integer (Option String))) #false]
           #:tick-digit->sticker [digit->label : Plot-Axis-Digit->Sticker default-plot-axis-digit->sticker]
           #:reals [real-list : (U (Listof Plot-Axis-Real-Datum) (Vectorof Any) (-> Integer Any)) null]
           #:real-color [real-color : (Option Color) #false]
           #:real-font [real-font : (Option Font) (default-axis-datum-font)]
           #:real-position [real-position : Real 1.0]
           #:real-anchor [real-anchor : Geo-Pin-Anchor 'cb]
           #:real-filter [real-filter : (Option Plot-Axis-Real-Filter) #false]
           #:real->sticker [real->label : Plot-Axis-Real->Sticker default-plot-axis-real->sticker]
           #:real->dot [real->dot : Plot-Axis-Real->Dot default-plot-axis-real->dot]
           #:real-exclude-zero? [exclude-zero? : Boolean #false]
           #:real-dot-radius [real-dot-radius : (Option Real) #false]
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

    (define arrow : Geo (geo-arrow flrhead (+ fllength fltick-thickness) #:shaft-thickness flthickness #:stroke #false #:fill axis-color))
    (define arrow-half : Nonnegative-Flonum (* (geo-height arrow) 0.5))
    (define gtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-length #:stroke #false #:fill (or tick-color axis-color))
                 tick-anchor)))

    (define -em : Flonum (- (font-metrics-ref digit-font 'em)))
    (define digit-anchor : Geo-Pin-Anchor (if (< digit-position 0.0) 'ct 'cb))
    (define flc-origin : Float-Complex (make-rectangular (+ (* fllength (real->double-flonum origin)) fltick-min) arrow-half))
    (define flc-offset : Float-Complex (make-rectangular 0.0 (* (real->double-flonum digit-position) -em)))
    (define real-offset : Float-Complex (make-rectangular 0.0 (* (real->double-flonum real-position) -em)))

    (define dot->pos : Plot-Axis-Position-Map
      (lambda [x]
        (+ flc-origin (* x flunit))))

    (define layers : (Listof (GLayerof Geo))
      (parameterize ([default-plot-axis-real-filter ((if (not exclude-zero?) values plot-axis-nonzero-values-wrap) (or real-filter (default-plot-axis-real-filter)))]
                     [default-plot-axis-digit-filter (or digit-filter (default-plot-axis-digit-filter))]
                     [default-plot-axis-datum-dot-radius (or real-dot-radius (default-plot-axis-datum-dot-radius))])
        (append (cond [(not axis-label) null]
                      [else (let ([g (geo-text axis-label (or label-font (desc-font digit-font #:family 'math)) #:color (or label-color axis-color))])
                              (plot-axis-sticker-cons g digit-anchor (make-rectangular fltick-max arrow-half)
                                                      flc-offset null fltick-min real-part fltick-max #false))])

                (for/fold ([ticks : (Listof (GLayerof Geo)) null])
                          ([tick (if (pair? tick-digits) (in-list tick-digits) (in-value 0))])
                  (plot-axis-sticker-cons (digit->label id tick digit-font (or digit-color axis-color)) digit-anchor
                                          (dot->pos (exact->inexact tick))
                                          flc-offset ticks fltick-min real-part fltick-max gtick))

                (for/fold ([reals : (Listof (GLayerof Geo)) null])
                          ([real (in-list (cond [(list? real-list) real-list]
                                                [(vector? real-list) (plot-axis-reals-from-vector real-list (if (not exclude-zero?) 0 1))]
                                                [else (plot-axis-reals-from-producer real-list tick-digits)]))])
                  (define-values (val obj) ((default-plot-axis-real-filter) real))
                  
                  (if (or val)
                      (let-values ([(label dot) (plot-axis-real-label-values id val obj real-anchor real->label real->dot flunit
                                                                             (or real-font digit-font) real-color axis-color)])
                        (plot-axis-sticker-cons (car label) (cdr label) (dot->pos val)
                                                real-offset reals fltick-min real-part fltick-max dot))
                      reals)))))

    (define translated-layers : (Option (GLayer-Groupof Geo)) (geo-path-try-extend/list (geo-own-layer arrow) layers))

    (if (or translated-layers)
        (let ([delta-origin (+ flc-origin (geo-layer-position (car (glayer-group-layers translated-layers))))])
          (create-geometry-group plot:axis id #false #false translated-layers delta-origin tick-digits
                                 (λ [[x : Flonum]] (+ delta-origin (* x flunit)))))
        (create-geometry-group plot:axis id #false #false (geo-own-layers arrow) flc-origin tick-digits dot->pos))))

(define plot-cartesian
  (lambda [#:id [id : (Option Symbol) #false]
           #:O [chO : Char #\O]
           #:axis-thickness [axis-thickness : Real 1.5]
           #:axis-color [axis-color : Color (default-axis-color)]
           #:axis-label-font [axis-font : (Option Font) (default-axis-label-font)]
           #:axis-label-color [label-color : (Option Color) #false]
           #:x-axis-label [xlabel : (Option String) "x"]
           #:y-axis-label [ylabel : (Option String) "y"]
           #:xtick-range [xtick-hint : (U Real (Pairof Real Real) (Listof Real)) null]
           #:ytick-range [ytick-hint : (U Real (Pairof Real Real) (Listof Real)) null]
           #:tick-color [tick-color : (Option Color) #false]
           #:tick-thickness [tick-thickness : Real 1.0]
           #:tick-length [tick-length : Complex -4.0]
           #:tick-digit-color [digit-color : (Option Color) #false]
           #:tick-digit-font [digit-font : Font (default-axis-digit-font)]
           #:tick-digit-position [digit-position : Complex -0.618]
           #:xtick-anchor [xtick-anchor : Geo-Pin-Anchor 'cc]
           #:ytick-anchor [ytick-anchor : Geo-Pin-Anchor 'cc]
           #:xtick-digit-filter [xdigit-filter : (Option (-> Integer (Option String))) #false]
           #:ytick-digit-filter [ydigit-filter : (Option (-> Integer (Option String))) #false]
           #:xtick-digit->sticker [xdigit->label : Plot-Axis-Digit->Sticker default-plot-axis-digit->sticker]
           #:ytick-digit->sticker [ydigit->label : Plot-Axis-Digit->Sticker default-plot-axis-digit->sticker]
           #:dot-radius [dot-radius : (Option Real) #false]
           [size : Complex] [origin : Complex] [unit-length : Complex -0.1]] : Plot:Cartesian
    (define-values (flwidth flheight) (plot-cartesian-settings* size ~length))
    (define-values (xO yO) (plot-cartesian-settings origin))
    (define-values (xU yU) (plot-cartesian-settings unit-length))
    (define-values (xunit yunit) (values (~length xU flwidth) (make-rectangular 0.0 (~length yU flheight))))
    (define-values (xdigit-position ydigit-position) (plot-cartesian-settings digit-position))
    (define flthickness : Nonnegative-Flonum (~length axis-thickness))
    (define fltick-thickness : Nonnegative-Flonum (~length tick-thickness flthickness))
    (define fltick-min : Nonnegative-Flonum (* fltick-thickness 0.5))
    (define xtick-max : Nonnegative-Flonum (+ fltick-min flwidth))
    (define ytick-max : Nonnegative-Flonum (+ fltick-min flheight))
    (define xtick-digits : (Listof Integer) (plot-axis-ticks xtick-hint))
    (define ytick-digits : (Listof Integer) (plot-axis-ticks ytick-hint))
    (define flrhead : Nonnegative-Flonum (* flthickness 3.14))

    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))
    (define label-font : Font (or axis-font (desc-font digit-font #:family 'math)))
    (define zero : Geo (geo-text (string chO) digit-font #:color (or digit-color axis-color)))
    (define-values (xtick-length ytick-length) (plot-cartesian-settings* tick-length ~length flthickness))
    (define Oshadow : Float-Complex (make-rectangular (+ (* flwidth xO) fltick-min) (+ (* flheight yO) fltick-min)))
    (define Origin : Float-Complex (make-rectangular (real-part Oshadow) 0.0))

    (define xaxis : Geo (geo-arrow flrhead (+ flwidth fltick-thickness) #:shaft-thickness flthickness #:stroke #false #:fill axis-color))
    (define xdigit-anchor : Geo-Pin-Anchor (if (< xdigit-position 0.0) 'ct 'cb))
    (define xoffset : Float-Complex (make-rectangular 0.0 (* xdigit-position em -1.0)))
    (define xtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> xtick-length 0.0)
           (cons (geo-rectangle fltick-thickness xtick-length #:stroke #false #:fill (or tick-color axis-color))
                 xtick-anchor)))

    (define yaxis : Geo (geo-arrow flrhead (+ flheight fltick-thickness) -pi/2 #:shaft-thickness flthickness #:stroke #false #:fill axis-color))
    (define ydigit-anchor : Geo-Pin-Anchor (if (< ydigit-position 0.0) 'rc 'lc))
    (define yoffset : Float-Complex (make-rectangular (* ydigit-position em) 0.0))
    (define ytick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> ytick-length 0.0)
           (cons (geo-rectangle ytick-length fltick-thickness #:stroke #false #:fill (or tick-color axis-color))
                 ytick-anchor)))

    (define layers : (Listof (GLayerof Geo))
      (parameterize ([default-plot-axis-digit-filter (or xdigit-filter (default-plot-axis-digit-filter))]
                     [default-plot-axis-datum-dot-radius (or dot-radius (default-plot-axis-datum-dot-radius))])
        (append (list (geo-own-pin-layer 'lc 0.0+0.0i xaxis 0.0+0.0i)
                      (geo-own-pin-layer 'cb 0.0+0.0i yaxis Oshadow))

                (cond [(not xlabel) null]
                      [else (let ([xgeo (geo-text xlabel label-font #:color (or label-color axis-color))])
                              (plot-axis-sticker-cons xgeo xdigit-anchor (make-rectangular xtick-max 0.0)
                                                      xoffset null -inf.0 real-part +inf.0 #false))])

                (for/fold ([xticks : (Listof (GLayerof Geo)) null])
                          ([xdigit (in-list xtick-digits)] #:unless (= xdigit 0))
                  (plot-axis-sticker-cons (xdigit->label id xdigit digit-font (or digit-color axis-color)) xdigit-anchor
                                          (+ Origin (* (exact->inexact xdigit) xunit))
                                          xoffset xticks fltick-min real-part xtick-max xtick))

                (cond [(not ylabel) null]
                      [else (let ([ygeo (geo-text ylabel label-font #:color (or label-color axis-color))])
                              (plot-axis-sticker-cons ygeo ydigit-anchor (make-rectangular 0.0 (- ytick-max))
                                                      (+ Oshadow yoffset) null -inf.0 imag-part +inf.0 #false))])

                (for/fold ([yticks : (Listof (GLayerof Geo)) null])
                          ([ydigit (in-list ytick-digits)] #:unless (= ydigit 0))
                  (plot-axis-sticker-cons (ydigit->label id ydigit digit-font (or digit-color axis-color)) ydigit-anchor
                                          (+ Origin (* (- (exact->inexact ydigit)) yunit))
                                          yoffset yticks -inf.0 imag-part +inf.0 ytick)))))
    
    (define translated-layers : (Option (GLayer-Groupof Geo))
      (geo-path-try-extend/list (geo-own-pin-layer (geo-anchor-merge xdigit-anchor ydigit-anchor)
                                                   Origin zero (+ yoffset xoffset))
                                layers))
    
    (define delta-origin : Float-Complex
      (if (or translated-layers)
          (+ Origin (geo-layer-position (car (glayer-group-layers translated-layers))))
          Origin))

    (define dot->pos : Plot-Cartesian-Position-Map
      (case-lambda
        [(x y) (+ delta-origin (* x xunit) (* (- y) yunit))]
        [(dot) (dot->pos (real-part dot) (imag-part dot))]))

    (create-geometry-group plot:cartesian id #false #false
                           (or translated-layers #;#:deadcode (geo-own-layers zero))
                           delta-origin xtick-digits ytick-digits
                           dot->pos)))
  