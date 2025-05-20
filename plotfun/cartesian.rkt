#lang typed/racket/base

(provide (all-defined-out))
(provide Plot:Cartesian plot:cartesian?)
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide (all-from-out "digitama/axis/tick/self.rkt"))
(provide (all-from-out "digitama/axis/style.rkt"))

(require digimon/metrics)
(require digimon/complex)
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
(require "digitama/axis/style.rkt")
(require "digitama/axis/config.rkt")
(require "digitama/axis/interface.rkt")
(require "digitama/axis/sticker.rkt")

(require "digitama/axis/tick.rkt")
(require "digitama/axis/tick/self.rkt")
(require "digitama/axis/tick/real.rkt")

(require "digitama/axis/renderer/self.rkt")

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
           #:x-range [xtick-hint : (U Real (Pairof Real Real) (Listof Real) False) #false]
           #:y-range [ytick-hint : (U Real (Pairof Real Real) (Listof Real) False) #false]
           #:x-ticks [xticks-generate : (Plot-Ticks-Generate Real) (plot-real-ticks)]
           #:y-ticks [yticks-generate : (Plot-Ticks-Generate Real) (plot-real-ticks)]
           #:x-digit->sticker [xdigit->label : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:y-digit->sticker [ydigit->label : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           . [tree : (U Plot:Renderer Plot-Renderer-Tree) *]] : Plot:Cartesian
    (define-values (renderers xrng) (plot-renderer-tree-flatten tree))
    (define-values (flwidth  used-width  x-neg-margin x-pos-margin) (plot-axis-length-values as width))
    (define-values (flheight used-height y-neg-margin y-pos-margin) (plot-axis-length-values as height flwidth))
    (define-values (maybe-xorig maybe-yorig) (plot-cartesian-maybe-settings maybe-origin))
    (define-values (maybe-xunit maybe-yunit) (plot-cartesian-maybe-settings maybe-unit))

    (define-values (xticks xO xunit) (plot-axis-metrics xtick-hint (cons -4 -4) maybe-xorig used-width maybe-xunit))
    (define-values (yticks yO yunit) (plot-axis-metrics ytick-hint (cons -4 -4) maybe-yorig used-height maybe-yunit flc-ri))

    (define actual-xticks : (Plot-Ticks Real) (ticks-select xtick-hint xticks xticks-generate))
    (define actual-xtick-values (map (inst plot-tick-value* Real) actual-xticks))
    (define actual-yticks : (Plot-Ticks Real) (ticks-select ytick-hint yticks yticks-generate))
    (define actual-ytick-values (map (inst plot-tick-value* Real) actual-yticks))
    
    (define-values (digit-font label-font axis-color digit-color tick-color label-color) (plot-axis-visual-values as))
    (define-values (xdigit-position xdigit-anchor) (plot-axis-digit-position-values as 'x))
    (define-values (ydigit-position ydigit-anchor) (plot-axis-digit-position-values as 'y))
    (define flthickness : Nonnegative-Flonum (plot-axis-style-thickness as))
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness as) flthickness))
    (define fltick-min : Nonnegative-Flonum (* fltick-thickness 0.5))
    (define xtick-min : Nonnegative-Flonum (+ fltick-min x-neg-margin))
    (define ytick-min : Nonnegative-Flonum (+ fltick-min y-neg-margin))
    (define xtick-max : Nonnegative-Flonum (+ xtick-min used-width))
    (define ytick-max : Nonnegative-Flonum (+ ytick-min used-height))
    (define xaxis-max : Nonnegative-Flonum (+ xtick-max x-pos-margin))
    (define yaxis-max : Nonnegative-Flonum (+ ytick-max y-pos-margin))
    (define flrhead : Nonnegative-Flonum (* flthickness 3.14))

    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))
    (define zero : Geo (geo-text (string chO) digit-font #:color digit-color))
    (define xtick-length : Nonnegative-Flonum (~length (plot-axis-tick-length as 'x) flthickness))
    (define ytick-length : Nonnegative-Flonum (~length (plot-axis-tick-length as 'y) flthickness))
    (define Oshadow : Float-Complex (make-rectangular (+ (* used-width xO) xtick-min) (+ (* used-height yO) ytick-min)))
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
                  (if (not (zero? xval))
                      (plot-axis-sticker-cons (xdigit->label id (cdr xtick) digit-font (or digit-color axis-color)) xdigit-anchor
                                              (+ Origin (* xval xunit))
                                              xoffset xticks xtick-min real-part xtick-max gxtick)
                      xticks))

                (cond [(not ylabel) null]
                      [else (let ([ygeo (geo-text ylabel label-font #:color label-color)])
                              (plot-axis-sticker-cons ygeo ydigit-anchor (make-rectangular 0.0 (- yaxis-max))
                                                      (+ Oshadow yoffset) null -inf.0 imag-part +inf.0 #false))])

                (for/fold ([yticks : (Listof (GLayerof Geo)) null])
                          ([ytick (in-list actual-yticks)])
                  (define yval : Flonum (real->double-flonum (plot-tick-value (car ytick))))
                  (if (not (zero? yval))
                      (plot-axis-sticker-cons (ydigit->label id (cdr ytick) digit-font (or digit-color axis-color)) ydigit-anchor
                                              (+ Origin (* (- yval) yunit))
                                              yoffset yticks -inf.0 imag-part +inf.0 gytick)
                      yticks)))))
    
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
                           delta-origin actual-xtick-values actual-ytick-values
                           dot->pos)))
  