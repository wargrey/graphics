#lang typed/racket/base

(provide (all-defined-out))
(provide Plot:Cartesian plot:cartesian?)
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide (all-from-out "digitama/axis/tick/self.rkt"))

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
(require "digitama/axis/style.rkt")
(require "digitama/axis/config.rkt")
(require "digitama/axis/interface.rkt")
(require "digitama/axis/sticker.rkt")

(require "digitama/axis/tick.rkt")
(require "digitama/axis/tick/self.rkt")
(require "digitama/axis/tick/real.rkt")

(require "digitama/axis/renderer/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(define plot-cartesian
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
           #:x-digit-filter [xdigit-filter : (Option (-> Integer (Option String))) #false]
           #:y-digit-filter [ydigit-filter : (Option (-> Integer (Option String))) #false]
           #:x-digit->sticker [xdigit->label : Plot-Axis-Digit->Sticker default-plot-axis-tick->sticker]
           #:y-digit->sticker [ydigit->label : Plot-Axis-Digit->Sticker default-plot-axis-tick->sticker]
           . [tree : (U Plot:Renderer Plot-Renderer-Tree) *]] : Plot:Cartesian
    (define-values (renderers xrng) (plot-renderer-tree-flatten tree))
    
    (define-values (xtick-digits xO flwidth xU)
      (plot-axis-metrics xtick-hint (plot-axis-real-range real-list exclude-zero?)
                         (plot-cartesian-value maybe-origin 'x) width (plot-cartesian-value maybe-unit 'x)
                         (default-plot-axis-unit-maxlength)))
    
    (define-values (ytick-digits yO fllength yU)
      (plot-axis-metrics ytick-hint (plot-axis-real-range real-list exclude-zero?)
                         (plot-cartesian-value maybe-origin 'y) (~length height flwidth) (plot-cartesian-value maybe-unit 'y)
                         (default-plot-axis-unit-maxlength)))
    
    (define-values (xunit yunit) (values (~length xU flwidth) (make-rectangular 0.0 (~length yU flheight))))
    (define-values (digit-font label-font axis-color digit-color tick-color label-color) (plot-axis-visual-values as))
    (define-values (xdigit-position xdigit-anchor) (plot-axis-digit-position-values as 'x))
    (define-values (ydigit-position ydigit-anchor) (plot-axis-digit-position-values as 'y))
    (define flthickness : Nonnegative-Flonum (plot-axis-style-thickness as))
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness as) flthickness))
    (define fltick-min : Nonnegative-Flonum (* fltick-thickness 0.5))
    (define xtick-max : Nonnegative-Flonum (+ fltick-min flwidth))
    (define ytick-max : Nonnegative-Flonum (+ fltick-min flheight))
    (define flrhead : Nonnegative-Flonum (* flthickness 3.14))

    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))
    (define zero : Geo (geo-text (string chO) digit-font #:color digit-color))
    (define xtick-length : Nonnegative-Flonum (~length (plot-axis-tick-length as 'x) flthickness))
    (define ytick-length : Nonnegative-Flonum (~length (plot-axis-tick-length as 'y) flthickness))
    (define Oshadow : Float-Complex (make-rectangular (+ (* flwidth xO) fltick-min) (+ (* flheight yO) fltick-min)))
    (define Origin : Float-Complex (make-rectangular (real-part Oshadow) 0.0))

    (define xaxis : Geo (geo-arrow flrhead (+ flwidth fltick-thickness) #:shaft-thickness flthickness #:stroke #false #:fill axis-color))
    (define xoffset : Float-Complex (make-rectangular 0.0 (* xdigit-position em -1.0)))
    (define xtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> xtick-length 0.0)
           (cons (geo-rectangle fltick-thickness xtick-length #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor as 'x))))

    (define yaxis : Geo (geo-arrow flrhead (+ flheight fltick-thickness) -pi/2 #:shaft-thickness flthickness #:stroke #false #:fill axis-color))
    (define yoffset : Float-Complex (make-rectangular (* ydigit-position em) 0.0))
    (define ytick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> ytick-length 0.0)
           (cons (geo-rectangle ytick-length fltick-thickness #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor as 'y))))

    (define layers : (Listof (GLayerof Geo))
      (parameterize ([default-plot-axis-digit-filter (or xdigit-filter (default-plot-axis-digit-filter))])
        (append (list (geo-own-pin-layer 'lc 0.0+0.0i xaxis 0.0+0.0i)
                      (geo-own-pin-layer 'cb 0.0+0.0i yaxis Oshadow))

                (cond [(not xlabel) null]
                      [else (let ([xgeo (geo-text xlabel label-font #:color label-color)])
                              (plot-axis-sticker-cons xgeo xdigit-anchor (make-rectangular xtick-max 0.0)
                                                      xoffset null -inf.0 real-part +inf.0 #false))])

                (for/fold ([xticks : (Listof (GLayerof Geo)) null])
                          ([xdigit (in-list xtick-digits)] #:unless (= xdigit 0))
                  (plot-axis-sticker-cons (xdigit->label id xdigit digit-font (or digit-color axis-color)) xdigit-anchor
                                          (+ Origin (* (exact->inexact xdigit) xunit))
                                          xoffset xticks fltick-min real-part xtick-max xtick))

                (cond [(not ylabel) null]
                      [else (let ([ygeo (geo-text ylabel label-font #:color label-color)])
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
  