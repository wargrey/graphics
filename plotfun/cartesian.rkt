#lang typed/racket/base

(provide (all-defined-out))
(provide Plot:Cartesian plot:cartesian?)
(provide Plot:Function plot:function? function)
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide (all-from-out "digitama/axis/tick/self.rkt"))
(provide (all-from-out "digitama/axis/tick/real.rkt"))
(provide (all-from-out "digitama/axis/style.rkt"))

(require digimon/metrics)
(require digimon/complex)
(require digimon/constant)

(require geofun/font)
(require geofun/color)
(require geofun/constructor)

(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/position)

(require "digitama/arithmetics.rkt")
(require "digitama/axis/self.rkt")
(require "digitama/axis/style.rkt")
(require "digitama/axis/config.rkt")
(require "digitama/axis/interface.rkt")
(require "digitama/axis/sticker.rkt")
(require "digitama/axis/tick.rkt")
(require "digitama/axis/tick/self.rkt")
(require "digitama/axis/tick/real.rkt")

(require "digitama/axis/visualizer/self.rkt")
(require "digitama/axis/visualizer/function.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-cartesian
  (lambda [#:id [id : (Option Symbol) #false]
           #:O [chO : Char #\O]
           #:origin [maybe-origin : (Option Complex) #false]
           #:unit-length [maybe-unit : (Option Complex) #false]
           #:width [width : Real (default-plot-cartesian-width)]
           #:height [height : Real (default-plot-cartesian-height)]
           #:style [as : Plot-Axis-Style (make-plot-axis-style)]
           #:x-label [xlabel : (Option String) "x"]
           #:y-label [ylabel : (Option String) "y"]
           #:x-range [xtick-hint : (U Real (Pairof Real Real) False) #false]
           #:y-range [ytick-hint : (U Real (Pairof Real Real) False) #false]
           #:x-ticks [xticks-engine : Plot-Tick-Engine (plot-real-ticks*)]
           #:y-ticks [yticks-engine : Plot-Tick-Engine (plot-real-ticks*)]
           #:x-tick-format [xtick-format : (Option Plot-Tick-Format) #false]
           #:y-tick-format [ytick-format : (Option Plot-Tick-Format) #false]
           #:x-tick->sticker [xdigit->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:y-tick->sticker [ydigit->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:fallback-range [fallback-dom : (Pairof Real Real) (default-plot-visualizer-domain-range)]
           . [tree : (U Plot:Visualizer Plot-Visualizer-Tree) *]] : Plot:Cartesian
    (define-values (visualizers maybe-xivl maybe-yivl) (plot-visualizer-tree-flatten tree))
    (define-values (xview yview)
      (plot-visualizer-ranges visualizers
                              (if (not xtick-hint) (plot-tick-engine-range xticks-engine) (plot-tick-range xtick-hint))
                              (if (not ytick-hint) (plot-tick-engine-range yticks-engine) (plot-tick-range ytick-hint))
                              maybe-xivl maybe-yivl fallback-dom))

    (define-values (maybe-xorig maybe-yorig) (plot-cartesian-maybe-settings maybe-origin))
    (define-values (maybe-xunit maybe-yunit) (plot-cartesian-maybe-settings maybe-unit))
    (define-values (flwidth   view-width x-neg-margin x-pos-margin) (plot-axis-length-values as width))
    (define-values (flheight view-height y-neg-margin y-pos-margin)
      (cond [(rational? height) (plot-axis-length-values as height flwidth)]
            [else (plot-axis-height-values as view-width (/ (- (cdr yview) (car yview))
                                                            (- (cdr xview) (car xview))))]))
    
    (define-values (xtick-range xO xunit) (plot-axis-metrics  xview maybe-xorig  view-width maybe-xunit))
    (define-values (ytick-range yO yunit) (plot-axis-metrics* yview maybe-yorig view-height (if (rational? height) maybe-yunit xunit) flc-ri))

    (define-values (actual-xticks maybe-step) (plot-ticks-generate xticks-engine xtick-range xtick-format))
    (define-values (actual-yticks _)
      (cond [(rational? height) (plot-ticks-generate yticks-engine ytick-range ytick-format)]
            [(not (and (rational? maybe-step) (positive? maybe-step))) (plot-ticks-generate yticks-engine ytick-range ytick-format)]
            [else (plot-ticks-generate (plot-interval-ticks maybe-step (plot-tick-engine-format yticks-engine)) ytick-range ytick-format)]))
    
    (define-values (digit-font label-font axis-color digit-color tick-color label-color) (plot-axis-visual-values as))
    (define-values (xdigit-position xdigit-anchor) (plot-axis-digit-position-values as 'x))
    (define-values (ydigit-position ydigit-anchor) (plot-axis-digit-position-values as 'y))
    (define flthickness : Nonnegative-Flonum (plot-axis-style-thickness as))
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness as) flthickness))
    (define fltick-min : Nonnegative-Flonum (* fltick-thickness 0.5))
    (define xtick-min : Nonnegative-Flonum (+ fltick-min x-neg-margin))
    (define ytick-min : Nonnegative-Flonum (+ fltick-min y-neg-margin))
    (define xtick-max : Nonnegative-Flonum (+ xtick-min view-width))
    (define ytick-max : Nonnegative-Flonum (+ ytick-min view-height))
    (define xaxis-max : Nonnegative-Flonum (+ xtick-max x-pos-margin))
    (define yaxis-max : Nonnegative-Flonum (+ ytick-max y-pos-margin))
    (define flrhead : Nonnegative-Flonum (* flthickness pi))

    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))
    (define zero : Geo (geo-text (string chO) digit-font #:color digit-color))
    (define xtick-length : Nonnegative-Flonum (~length (plot-axis-tick-length as 'x) flthickness))
    (define ytick-length : Nonnegative-Flonum (~length (plot-axis-tick-length as 'y) flthickness))
    (define Oshadow : Float-Complex (make-rectangular (+ (* view-width xO) xtick-min) (+ (* view-height yO) ytick-min)))
    (define Origin : Float-Complex (make-rectangular (real-part Oshadow) 0.0))

    (define xaxis : Geo (geo-arrow flrhead (+ flwidth fltick-thickness) #:shaft-thickness flthickness #:stroke #false #:fill axis-color))
    (define xoffset : Float-Complex (make-rectangular 0.0 (* xdigit-position em -1.0)))
    (define gxtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> xtick-length 0.0)
           (cons (geo-rectangle fltick-thickness xtick-length #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor as 'x))))

    (define yaxis : Geo (geo-arrow flrhead (+ flheight fltick-thickness) -pi/2 #:shaft-thickness flthickness #:stroke #false #:fill axis-color))
    (define yoffset : Float-Complex (make-rectangular (* ydigit-position em) 0.0))
    (define gytick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> ytick-length 0.0)
           (cons (geo-rectangle ytick-length fltick-thickness #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor as 'y))))

    (define origin-dot->pos : Plot-Cartesian-Position-Map
      (case-lambda
        [(x y) (+ Origin (* x xunit) (* (- y) yunit))]
        [(dot) (origin-dot->pos (real-part dot) (imag-part dot))]))

    (define 0-as-xdigit? : Boolean
      (and (< (imag-part Oshadow) (imag-part xoffset))
           (null? actual-yticks)))

    (define vlayers : (Listof (Pairof (GLayerof Geo) (Option FlRGBA)))
      (for/list ([r (in-list visualizers)]
                 [idx (in-naturals 1)])
        (define this-dom (plot-range-select (plot:visualizer-xrng r) xview))
        (define this-ran (plot-range-select (plot:visualizer-yrng r) yview))
        (define-values (graph pos legend-color) ((plot:visualizer-realize r) idx this-dom this-ran origin-dot->pos))
        
        (cons (geo-own-pin-layer 'lt pos graph 0.0+0.0i) legend-color)))

    (define layers : (Listof (GLayerof Geo))
      (parameterize ()
        (append (list (geo-own-pin-layer 'lc 0.0+0.0i xaxis 0.0+0.0i)
                      (geo-own-pin-layer 'cb 0.0+0.0i yaxis Oshadow))

                (cond [(not xlabel) null]
                      [else (let ([xgeo (geo-text xlabel label-font #:color label-color)])
                              (plot-axis-sticker-cons xgeo xdigit-anchor (make-rectangular xaxis-max 0.0)
                                                      xoffset null -inf.0 real-part +inf.0 #false))])

                (for/fold ([xticks : (Listof (GLayerof Geo)) null])
                          ([xtick (in-list actual-xticks)])
                  (define xval : Flonum (real->double-flonum (plot-tick-value (car xtick))))
                  (cond [(not (zero? (scaled-round xval)))
                         (plot-axis-sticker-cons (xdigit->sticker id (cdr xtick) digit-font (or digit-color axis-color)) xdigit-anchor
                                                 (origin-dot->pos xval 0.0) xoffset xticks xtick-min real-part xtick-max gxtick)]
                        [(or 0-as-xdigit?)
                         (plot-axis-sticker-cons (xdigit->sticker id (cdr xtick) digit-font (or digit-color axis-color)) xdigit-anchor
                                                 (origin-dot->pos xval 0.0) xoffset xticks xtick-min real-part xtick-max #false)]
                        [else xticks]))

                (cond [(not ylabel) null]
                      [else (let ([ygeo (geo-text ylabel label-font #:color label-color)])
                              (plot-axis-sticker-cons ygeo ydigit-anchor (make-rectangular 0.0 (- yaxis-max))
                                                      (+ Oshadow yoffset) null -inf.0 imag-part +inf.0 #false))])

                (for/fold ([yticks : (Listof (GLayerof Geo)) null])
                          ([ytick (in-list actual-yticks)])
                  (define yval : Flonum (real->double-flonum (plot-tick-value (car ytick))))
                  (if (not (zero? (scaled-round yval)))
                      (plot-axis-sticker-cons (ydigit->sticker id (cdr ytick) digit-font (or digit-color axis-color)) ydigit-anchor
                                              (origin-dot->pos 0.0 yval) yoffset yticks -inf.0 imag-part +inf.0 gytick)
                      yticks))

                (map (inst car (GLayerof Geo) (Option FlRGBA)) vlayers))))
    
    (define translated-layers : (Option (GLayer-Groupof Geo))
      (if (not 0-as-xdigit?)
          (geo-path-try-extend/list (geo-own-pin-layer (geo-anchor-merge xdigit-anchor ydigit-anchor)
                                                       Origin zero (+ yoffset xoffset))
                                    layers)
          (and (pair? layers)
               (geo-path-try-extend/list layers 0.0 0.0))))
    
    (define delta-origin : Float-Complex
      (if (or translated-layers)
          (+ Origin (geo-layer-position (car (glayer-group-layers translated-layers))))
          Origin))

    (if (not translated-layers) ; just in case. say, infinite width
        (create-geometry-group plot:cartesian id #false #false (geo-own-layers zero) delta-origin null null
                               (case-lambda
                                 [(x y) (make-rectangular x y)]
                                 [(dot) dot]))
        (create-geometry-group plot:cartesian id #false #false translated-layers delta-origin
                               (map plot-tick-value* actual-xticks)
                               (map plot-tick-value* actual-yticks)
                               (let ([dot->pos (λ [[x : Flonum] [y : Flonum]] : Float-Complex (+ delta-origin (* x xunit) (* (- y) yunit)))])
                                 (case-lambda
                                   [(x y) (dot->pos x y)]
                                   [(dot) (dot->pos (real-part dot) (imag-part dot))]))))))
  