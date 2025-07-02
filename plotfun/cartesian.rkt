#lang typed/racket/base

(provide (all-defined-out))
(provide Plot:Cartesian plot:cartesian?)
(provide Plot:Function plot:function? function)
(provide Plot-Visualizer-Tree Plot-Visualizer plot-visualizer?)
(provide (all-from-out "digitama/axis/style.rkt"))
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide (all-from-out "digitama/axis/singleton.rkt"))
(provide (all-from-out "digitama/axis/tick/self.rkt"))
(provide (all-from-out "digitama/axis/tick/real.rkt"))
(provide (all-from-out "digitama/marker/self.rkt"))
(provide (all-from-out "digitama/marker/style.rkt"))
(provide (all-from-out "digitama/visualizer/interface.rkt"))

(require digimon/metrics)
(require digimon/complex)

(require geofun/font)
(require geofun/color)
(require geofun/paint)
(require geofun/stroke)
(require geofun/constructor)

(require geofun/digitama/markup)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/edge)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/position)
(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/footprint)

(require "digitama/arithmetics.rkt")
(require "digitama/axis/self.rkt")
(require "digitama/axis/style.rkt")
(require "digitama/axis/config.rkt")
(require "digitama/axis/interface.rkt")
(require "digitama/axis/singleton.rkt")
(require "digitama/axis/sticker.rkt")
(require "digitama/axis/tick.rkt")
(require "digitama/axis/tick/self.rkt")
(require "digitama/axis/tick/real.rkt")

(require "digitama/marker/dc.rkt")
(require "digitama/marker/self.rkt")
(require "digitama/marker/style.rkt")
(require "digitama/marker/config.rkt")

(require "digitama/visualizer/self.rkt")
(require "digitama/visualizer/interface.rkt")
(require "digitama/visualizer/function.rkt")
(require "digitama/visualizer/reference.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-cartesian
  (lambda [#:id [id : (Option Symbol) #false]
           #:O [chO : Char #\O]
           #:origin [maybe-origin : (Option Complex) #false]
           #:unit-length [maybe-unit : (Option Complex+%) #false]
           #:width [width : Real (default-plot-cartesian-view-width)]
           #:height [height : Real (default-plot-cartesian-view-height)]
           #:style [axis-style : Plot-Axis-Style (default-plot-axis-style)]
           #:mark-style [mark-style : (Option Plot-Mark-Style) #false]
           #:x-label [x-label : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) "x"]
           #:y-label [y-label : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) "y"]
           #:x-desc [x-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:y-desc [y-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:x-range [xtick-hint : (U Real (Pairof Real Real) False) #false]
           #:y-range [ytick-hint : (U Real (Pairof Real Real) False) #false]
           #:x-ticks [xticks-engine : Plot-Tick-Engine (plot-real-ticks*)]
           #:y-ticks [yticks-engine : Plot-Tick-Engine (plot-real-ticks*)]
           #:x-tick-format [xtick-format : (Option Plot-Tick-Format) #false]
           #:y-tick-format [ytick-format : (Option Plot-Tick-Format) #false]
           #:x-tick->sticker [xdigit->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:y-tick->sticker [ydigit->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:palette [palette : Plot-Palette (default-plot-palette)]
           #:fallback-range [fallback-dom : (Pairof Real Real) (default-plot-visualizer-domain-range)]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false]
           #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           . [tree : (U Plot-Visualizer Plot-Visualizer-Tree) *]] : Plot:Cartesian
    (define-values (visualizers maybe-xivl maybe-yivl) (plot-visualizer-tree-flatten tree))
    (define-values (xview yview)
      (plot-visualizer-ranges visualizers
                              (if (not xtick-hint) (plot-tick-engine-range xticks-engine) (plot-tick-range xtick-hint))
                              (if (not ytick-hint) (plot-tick-engine-range yticks-engine) (plot-tick-range ytick-hint))
                              maybe-xivl maybe-yivl fallback-dom))

    (define-values (maybe-xorig maybe-yorig) (plot-cartesian-maybe-settings maybe-origin))
    (define-values (maybe-xunit maybe-yunit) (plot-cartesian-maybe-settings maybe-unit))
    (define-values (x-tip y-tip) (values (plot-axis-tip axis-style 'x) (plot-axis-tip axis-style 'y)))
    (define-values ( flwidth  view-width x-neg-margin x-pos-margin) (plot-axis-length-values axis-style x-tip width))
    (define-values (flheight view-height y-neg-margin y-pos-margin)
      (cond [(rational? height) (plot-axis-length-values axis-style y-tip height flwidth)]
            [else (plot-axis-height-values axis-style y-tip view-width
                                           (/ (- (cdr yview) (car yview))
                                              (- (cdr xview) (car xview))))]))
    
    (define-values (xtick-range xO xunit) (plot-axis-metrics  xview maybe-xorig  view-width maybe-xunit))
    (define-values (ytick-range yO yunit) (plot-axis-metrics* yview maybe-yorig view-height (if (rational? height) maybe-yunit xunit) flc-ri))

    (define-values (actual-xticks maybe-step) (plot-ticks-generate xticks-engine xtick-range xtick-format))
    (define-values (actual-yticks _)
      (cond [(rational? height) (plot-ticks-generate yticks-engine ytick-range ytick-format)]
            [(not (and (rational? maybe-step) (positive? maybe-step))) (plot-ticks-generate yticks-engine ytick-range ytick-format)]
            [else (plot-ticks-generate (plot-interval-ticks maybe-step (plot-tick-engine-format yticks-engine)) ytick-range ytick-format)]))

    (define bg-color : (Option FlRGBA) (brush-maybe-rgba bg))
    (define adjust-color (λ [[c : Color]] (plot-adjust-pen-color palette (rgb* c) bg-color)))
    (define-values (axis-font digit-font label-font desc-font axis-pen flthickness digit-color tick-color label-color desc-color)
      (plot-axis-visual-values axis-style adjust-color))
    (define-values (pin-pen mark-font mark-color mark-anchor) (plot-mark-visual-values mark-style axis-font axis-pen adjust-color))
    
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness axis-style) flthickness))
    (define fltick-length : Nonnegative-Flonum (~length (plot-axis-style-tick-length axis-style) flthickness))
    (define fltick-min : Nonnegative-Flonum (* fltick-thickness 0.5))
    (define xtick-min : Nonnegative-Flonum (+ fltick-min x-neg-margin))
    (define ytick-min : Nonnegative-Flonum (+ fltick-min y-neg-margin))
    (define xtick-max : Nonnegative-Flonum (+ xtick-min view-width))
    (define ytick-max : Nonnegative-Flonum (+ ytick-min view-height))

    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))
    (define zero : Geo (geo-text (string chO) digit-font #:color digit-color))
    (define Oshadow : Float-Complex (make-rectangular (+ (* view-width xO) xtick-min) (+ (* view-height yO) ytick-min)))
    (define Origin : Float-Complex (make-rectangular (real-part Oshadow) 0.0))

    (define-values (xdigit-position xdigit-anchor) (plot-axis-digit-position-values axis-style 'x))
    (define-values (ydigit-position ydigit-anchor) (plot-axis-digit-position-values axis-style 'y))
    
    (define xaxis : Geo:Edge
      (geo-edge* #:stroke axis-pen #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape x-tip)
                 #:target-tip (plot-axis-tip-style-positive-shape x-tip)
                 (list the-M0 (gpp:point #\L (make-rectangular flwidth 0.0)))))
    
    (define xoffset : Float-Complex (flc-ri (* xdigit-position em -1.0)))
    (define gxtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-length #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor axis-style 'x))))

    (define yaxis : Geo:Edge
      (geo-edge* #:stroke axis-pen #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape (or y-tip x-tip))
                 #:target-tip (plot-axis-tip-style-positive-shape (or y-tip x-tip))
                 (list the-M0 (gpp:point #\L (flc-ri (- flheight))))))
    
    (define yoffset : Float-Complex (make-rectangular (* ydigit-position em) 0.0))
    (define gytick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-length fltick-thickness #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor axis-style 'y))))

    (define-values (xsoff xeoff) (geo-edge-endpoint-offsets xaxis))
    (define-values (ysoff yeoff) (geo-edge-endpoint-offsets yaxis))
    (define label-as-digit? : Boolean (eq? (plot-axis-style-label-placement axis-style) 'digit))
    (define xaxis-min : Flonum (- xtick-min x-neg-margin (- (real-part xsoff))))
    (define xaxis-max : Flonum (+ xtick-max x-pos-margin (real-part xeoff)))
    (define yaxis-min : Flonum (- ytick-min y-neg-margin (imag-part ysoff)))
    (define yaxis-max : Flonum (+ ytick-max y-pos-margin (- (imag-part yeoff))))

    (define origin-dot->pos : Plot-Position-Transform
      (case-lambda
        [(x y) (+ Origin (* x xunit) (* (- y) yunit))]
        [(dot) (origin-dot->pos (real-part dot) (imag-part dot))]))

    (define 0-as-xdigit? : Boolean
      (and (< (imag-part Oshadow) (imag-part xoffset))
           (negative? (car xview))
           #;(null? actual-yticks)))

    (define color-pool : (HashTable Natural FlRGBA) (make-hasheq))
    (define plots : (Listof Geo:Visualizer)
      (parameterize ([default-plot-palette palette]
                     [current-visualizer-color-pool color-pool])
        (let ([N (length visualizers)])
          (if (> N 0)
              (let realize ([visualizers : (Listof Plot-Visualizer) visualizers]
                            [idx : Nonnegative-Fixnum 0]
                            [sreyalv : (Listof Geo:Visualizer) null])
                (if (and (pair? visualizers) (<= idx N))
                    (let ([self (car visualizers)])
                      (define visualizer (plot-realize self idx N xview yview origin-dot->pos bg-color))
                      (unless (plot-visualizer-skip-palette? self)
                        (hash-set! color-pool idx (geo:visualizer-color visualizer)))
                      (realize (cdr visualizers)
                               (if (plot-visualizer-skip-palette? self) idx (+ idx 1))
                               (cons visualizer sreyalv)))
                    (reverse sreyalv)))
              null))))
      
    (define layers : (Listof (GLayerof Geo))
      (append

       ; arrows
       (list (geo-own-pin-layer 'lc 0.0+0.0i xaxis 0.0+0.0i)
             (geo-own-pin-layer 'cb 0.0+0.0i yaxis Oshadow))

       ; visualizers' labels
       (for/list : (Listof (GLayerof Geo)) ([self (in-list plots)]
                                            #:when (geo:visualizer-label self))
         (define-values (real-pin real-gap)
           (plot-mark-vector-values mark-style
                                    (or (geo:visualizer-pin-angle self) 0.0)
                                    (or (geo:visualizer-gap-angle self) 0.0)))
         
         ;; visualizer should ensure its label being pinned at visible point 
         (geo-edge-self-pin-layer
          (plot-marker #:color (geo:visualizer-color self) #:font mark-font #:pin-stroke pin-pen
                       #:fallback-pin real-pin #:fallback-gap real-gap
                       #:fallback-anchor mark-anchor #:length-base em
                       (geo:visualizer-label self) origin-dot->pos)))
       
       ; visualizers
       (for/list : (Listof (GLayerof Geo)) ([self (in-list plots)])
         (geo-own-pin-layer 'lt (geo:visualizer-position self) self 0.0+0.0i))

       ; x-axis's labels
       (for/fold ([labels : (Listof (GLayerof Geo)) null])
                 ([lbl (in-list (plot-axis-label-settings x-label x-desc xaxis-min xaxis-max (if (or label-as-digit?) 0.0 (* em 0.6))))])
         (if (car lbl)
             (let ([lbl.geo (plot-x-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color (* em 0.4))])
               (if (or label-as-digit?)
                   (plot-axis-sticker-cons lbl.geo 'cc (+ (cadr lbl) xoffset) 0.0+0.0i labels -inf.0 real-part +inf.0 #false)
                   (plot-axis-sticker-cons lbl.geo (cadddr lbl) (make-rectangular (cadr lbl) +0.0) 0.0+0.0i labels -inf.0 real-part +inf.0 #false)))
             labels))

       ; x-axis's ticks
       (for/fold ([xticks : (Listof (GLayerof Geo)) null])
                 ([xtick (in-list actual-xticks)])
         (define xval : Flonum (real->double-flonum (plot-tick-value (car xtick))))
         (cond [(not (zero? (scaled-round xval)))
                (plot-axis-sticker-cons (xdigit->sticker id (cdr xtick) digit-font digit-color) xdigit-anchor
                                        (origin-dot->pos xval 0.0) xoffset xticks xtick-min real-part xtick-max gxtick)]
               [(or 0-as-xdigit?)
                (plot-axis-sticker-cons (xdigit->sticker id (cdr xtick) digit-font digit-color) xdigit-anchor
                                        (origin-dot->pos xval 0.0) xoffset xticks xtick-min real-part xtick-max #false)]
               [else xticks]))
       
       ; y-axis's labels
       (for/fold ([labels : (Listof (GLayerof Geo)) null])
                 ([lbl (in-list (plot-axis-label-settings y-label y-desc yaxis-min yaxis-max (if (or label-as-digit?) 0.0 (* em 1.5))))])
         (if (car lbl)
             (if (or label-as-digit?)
                 (plot-axis-sticker-cons (plot-y-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color)
                                         ydigit-anchor (flc-ri (- (cadr lbl))) (+ Oshadow yoffset) labels -inf.0 imag-part +inf.0 #false)
                 (plot-axis-sticker-cons (plot-y-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color)
                                         'cc (flc-ri (- (cadr lbl))) Oshadow labels -inf.0 imag-part +inf.0 #false))
             labels))

       ; y-axis's ticks
       (for/fold ([yticks : (Listof (GLayerof Geo)) null])
                 ([ytick (in-list actual-yticks)])
         (define yval : Flonum (real->double-flonum (plot-tick-value (car ytick))))
         (if (not (zero? (scaled-round yval)))
             (plot-axis-sticker-cons (ydigit->sticker id (cdr ytick) digit-font digit-color) ydigit-anchor
                                     (origin-dot->pos 0.0 yval) yoffset yticks -inf.0 imag-part +inf.0 gytick)
             yticks))))
    
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
        (create-geometry-group plot:cartesian id #false #false
                               #:border bdr #:background bg
                               #:margin margin #:padding padding
                               (geo-own-layers zero) delta-origin null null
                               origin-dot->pos)
        (create-geometry-group plot:cartesian id #false #false
                               #:border bdr #:background bg
                               #:margin margin #:padding padding
                               translated-layers delta-origin
                               (map plot-tick-value* actual-xticks)
                               (map plot-tick-value* actual-yticks)
                               (let ([dot->pos (λ [[x : Flonum] [y : Flonum]] : Float-Complex
                                                 (+ delta-origin (* x xunit) (* (- y) yunit)))])
                                 (case-lambda
                                   [(x y) (dot->pos x y)]
                                   [(dot) (dot->pos (real-part dot) (imag-part dot))]))))))
  