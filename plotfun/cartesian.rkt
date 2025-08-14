#lang typed/racket/base

(provide (all-defined-out) Plot-Mark-Auto-Anchor)
(provide Plot:Cartesian plot:cartesian?)
(provide (all-from-out geofun/digitama/path/tips))
(provide (all-from-out "digitama/axis/style.rkt"))
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide (all-from-out "digitama/axis/singleton.rkt"))
(provide (all-from-out "digitama/marker/self.rkt"))
(provide (all-from-out "digitama/marker/style.rkt"))
(provide (all-from-out "digitama/axis/tick/self.rkt"))
(provide (all-from-out "digitama/axis/tick/real.rkt"))
(provide (all-from-out "digitama/axis/grid/self.rkt"))
(provide (all-from-out "digitama/visualizer.rkt"))

(require racket/case)
(require racket/math)

(require digimon/metrics)
(require digimon/complex)
(require digimon/flonum)

(require geofun/font)
(require geofun/color)
(require geofun/paint)
(require geofun/stroke)
(require geofun/constructor)

(require geofun/digitama/markup)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/merge)
(require geofun/digitama/layer/position)
(require geofun/digitama/paint/self)
(require geofun/digitama/path/tips)
(require geofun/digitama/path/tick)
(require geofun/digitama/geometry/footprint)

(require "digitama/visualizer.rkt")
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
(require "digitama/marker/anchor.rkt")

(require "digitama/axis/grid/self.rkt")
(require "digitama/axis/grid/config.rkt")

(require "digitama/visualizer/self.rkt")
(require "digitama/visualizer/interface.rkt")
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
           #:y-ticks [yticks-engine : Plot-Tick-Engine xticks-engine]
           #:x-tick-format [xtick-format : (Option Plot-Tick-Format) #false]
           #:y-tick-format [ytick-format : (Option Plot-Tick-Format) xtick-format]
           #:x-tick->sticker [xdigit->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:y-tick->sticker [ydigit->sticker : Plot-Axis-Tick->Sticker xdigit->sticker]
           #:x-grid-style [xgrid-style : (Option Plot-Grid-Style) (default-plot-grid-style)]
           #:y-grid-style [ygrid-style : (Option Plot-Grid-Style) xgrid-style]
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

    (define-values (actual-xticks maybe-xstep xminor-count)
      (plot-ticks-generate xticks-engine xtick-range xtick-format
                           (and xgrid-style (plot-grid-style-minor-count xgrid-style))))
    
    (define-values (actual-yticks maybe-ystep yminor-count)
      (let ([y-minor-count (and ygrid-style (plot-grid-style-minor-count ygrid-style))])
        (cond [(rational? height)
               (plot-ticks-generate yticks-engine ytick-range ytick-format y-minor-count)]
              [(not (and (rational? maybe-xstep) (positive? maybe-xstep)))
               (plot-ticks-generate yticks-engine ytick-range ytick-format y-minor-count)]
              [else ; copied from the xtick engine
               (plot-ticks-generate (plot-interval-ticks #:minor-count (or y-minor-count xminor-count)
                                                         maybe-xstep (plot-tick-engine-format yticks-engine))
                                    ytick-range ytick-format)])))
      
    (define bg-color : (Option FlRGBA) (brush-maybe-rgba bg))
    (define adjust-color (λ [[c : FlRGBA]] (plot-adjust-pen-color palette c bg-color)))
    (define-values (axis-font digit-font label-font desc-font axis-pen flthickness digit-color tick-color label-color desc-color)
      (plot-axis-visual-values axis-style adjust-color))
    (define-values (pin-pen mark-font mark-color mark-anchor) (plot-mark-visual-values mark-style axis-font axis-pen adjust-color))
    
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness axis-style) flthickness))
    (define fltick-length : Nonnegative-Flonum (~length (plot-axis-style-tick-length axis-style) flthickness))
    (define fltick-sublen : Nonnegative-Flonum (~length (plot-axis-style-minor-tick-length axis-style) fltick-length))
    (define fltick-min : Nonnegative-Flonum (* fltick-thickness 0.5))
    (define xtick-min : Nonnegative-Flonum (+ fltick-min x-neg-margin))
    (define ytick-min : Nonnegative-Flonum (+ fltick-min y-neg-margin))
    (define xtick-max : Nonnegative-Flonum (+ xtick-min view-width))
    (define ytick-max : Nonnegative-Flonum (+ ytick-min view-height))

    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))
    (define zero : Geo (geo-text (string chO) digit-font #:color digit-color))
    (define Oshadow : Float-Complex (make-rectangular (+ (* view-width xO) xtick-min) (+ (* view-height yO) ytick-min)))
    (define Origin : Float-Complex (make-rectangular (real-part Oshadow) 0.0))

    (define-values (xdigit-position xdigit-anchor xmiror-anchor) (plot-axis-digit-position-values axis-style 'x))
    (define-values (ydigit-position ydigit-anchor ymiror-anchor) (plot-axis-digit-position-values axis-style 'y))
    (define tick-pen : Stroke (desc-stroke axis-pen #:width fltick-thickness #:color tick-color))
    (define-values (xmajor-pen xminor-pen) (plot-grid-visual-values xgrid-style adjust-color))
    (define-values (ymajor-pen yminor-pen) (plot-grid-visual-values ygrid-style adjust-color))
    
    (define xaxis : Geo:Path:Self
      (geo-path* #:stroke axis-pen #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape x-tip)
                 #:target-tip (plot-axis-tip-style-positive-shape x-tip)
                 (list the-M0 (gpp:point #\L (make-rectangular flwidth 0.0)))))
    
    (define xdigit-offset : Float-Complex (flc-ri (* xdigit-position em -1.0)))
    (define xmiror-offset :  Float-Complex (flc-ri (* xdigit-position em +1.0)))
    (define xmajor-tick : (Option Geo-Path)
      (and (> fltick-length 0.0)
           (geo-path* #:stroke tick-pen
                      (geo-xtick-footprints fltick-length 0.0
                                            (plot-axis-style-tick-placement axis-style)))))
    (define xminor-tick : (Option Geo-Path)
      (and (> xminor-count 0)
           (> fltick-length 0.0)
           (geo-path* #:stroke tick-pen
                      (geo-xtick-footprints fltick-sublen 0.0
                                            (plot-axis-style-tick-placement axis-style)))))

    (define yaxis : Geo:Path:Self
      (geo-path* #:stroke axis-pen #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape (or y-tip x-tip))
                 #:target-tip (plot-axis-tip-style-positive-shape (or y-tip x-tip))
                 (list the-M0 (gpp:point #\L (flc-ri (- flheight))))))
    
    (define ydigit-offset : Float-Complex (make-rectangular (* ydigit-position em) 0.0))
    (define ymiror-offset : Float-Complex (make-rectangular (* ydigit-position (- em)) 0.0))
    (define ymajor-tick : (Option Geo-Path)
      (and (> fltick-length 0.0)
           (geo-path* #:stroke tick-pen
                      (geo-ytick-footprints fltick-length 0.0
                                            (plot-axis-style-tick-placement axis-style)))))
    (define yminor-tick : (Option Geo-Path)
      (and (> yminor-count 0)
           (> fltick-length 0.0)
           (geo-path* #:stroke tick-pen
                      (geo-ytick-footprints fltick-sublen 0.0
                                            (plot-axis-style-tick-placement axis-style)))))

    (define-values (xsoff xeoff) (geo-path-endpoint-offsets xaxis))
    (define-values (ysoff yeoff) (geo-path-endpoint-offsets yaxis))
    (define label-at-axis? : Boolean (eq? (plot-axis-style-label-placement axis-style) 'axis))
    (define xaxis-min : Flonum (- xtick-min x-neg-margin (real-part xsoff)))
    (define xaxis-max : Flonum (+ xtick-max x-pos-margin (real-part xeoff)))
    (define yaxis-min : Flonum (- ytick-min y-neg-margin (imag-part ysoff)))
    (define yaxis-max : Flonum (+ ytick-max y-pos-margin (imag-part yeoff)))

    (define origin-dot->pos : Plot-Position-Transform
      (case-lambda
        [(x y) (+ Origin (* x xunit) (* (- y) yunit))]
        [(dot) (origin-dot->pos (real-part dot) (imag-part dot))]))

    (define 0-as-xdigit? : Boolean
      (and (< (imag-part Oshadow) (imag-part xdigit-offset))
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

    (define grid-pos : Float-Complex
      (origin-dot->pos (real->double-flonum (car xview))
                       (real->double-flonum (cdr yview))))
    
    (define xgrid : (Option Geo:Grid)
      (and (or xmajor-pen xminor-pen)
           (> view-width 0.0) (> view-height 0.0)
           (let ([x-pos (λ [[T : Plot-Tick]] (real-part (- (origin-dot->pos (real->double-flonum (plot-tick-value T)) 0.0) grid-pos)))])
             (geo-grid #:major-stroke xmajor-pen #:minor-stroke xminor-pen
                       #:extra-major-xs (for/list : (Listof Flonum) ([xtick (in-list actual-xticks)] #:when (plot-tick-major? xtick))
                                          (x-pos xtick))
                       #:extra-minor-xs (for/list : (Listof Flonum) ([xtick (in-list actual-xticks)] #:unless (plot-tick-major? xtick))
                                          (x-pos xtick))
                       view-width view-height))))
    
    (define ygrid : (Option Geo:Grid)
      (and (or ymajor-pen yminor-pen)
           (> view-width 0.0) (> view-height 0.0)
           (let ([y-pos (λ [[T : Plot-Tick]] (imag-part (- (origin-dot->pos 0.0 (real->double-flonum (plot-tick-value T))) grid-pos)))])
             (geo-grid #:major-stroke ymajor-pen #:minor-stroke yminor-pen
                       #:extra-major-ys (for/list : (Listof Flonum) ([ytick (in-list actual-yticks)] #:when (plot-tick-major? ytick))
                                          (y-pos ytick))
                       #:extra-minor-ys (for/list : (Listof Flonum) ([ytick (in-list actual-yticks)] #:unless (plot-tick-major? ytick))
                                          (y-pos ytick))
                       view-width view-height))))
    
    (define layers : (Listof (GLayerof Geo))
      (append
       ; grid
       (cond [(and xgrid ygrid) (list (geo-own-pin-layer 'lt grid-pos xgrid 0.0+0.0i)
                                      (geo-own-pin-layer 'lt grid-pos ygrid 0.0+0.0i))]
             [(or xgrid) (list (geo-own-pin-layer 'lt grid-pos xgrid 0.0+0.0i))]
             [(or ygrid) (list (geo-own-pin-layer 'lt grid-pos ygrid 0.0+0.0i))]
             [else null])

       ; arrows
       (list (geo-own-pin-layer 'lc 0.0+0.0i xaxis 0.0+0.0i)
             (geo-own-pin-layer 'cb 0.0+0.0i yaxis Oshadow))
       
       ; visualizers
       (for/list : (Listof (GLayerof Geo)) ([self (in-list plots)])
         (geo-own-pin-layer 'lt (geo:visualizer-position self) self 0.0+0.0i))

       ; x-axis's labels
       (for/fold ([labels : (Listof (GLayerof Geo)) null])
                 ([lbl (in-list (plot-axis-label-settings x-label x-desc xaxis-min xaxis-max (if (not label-at-axis?) 0.0 (* em 0.6))))])
         (if (car lbl)
             (let ([lbl.geo (plot-x-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color (* em 0.4))])
               (case/eq (plot-axis-style-label-placement axis-style)
                 [(digit)  (plot-axis-sticker-cons* lbl.geo xdigit-anchor (+ (cadr lbl) xdigit-offset) 0.0+0.0i labels)]
                 [(mirror) (plot-axis-sticker-cons* lbl.geo xmiror-anchor (+ (cadr lbl) xmiror-offset) 0.0+0.0i labels)]
                 [else (plot-axis-sticker-cons* lbl.geo (cadddr lbl) (make-rectangular (cadr lbl) +0.0) 0.0+0.0i labels)]))
             labels))

       ; x-axis's ticks
       (for/fold ([xticks : (Listof (GLayerof Geo)) null])
                 ([xtick (in-list actual-xticks)])
         (define xval : Flonum (real->double-flonum (plot-tick-value xtick)))
         (define-values (maybe-sticker gtick)
           (if (plot-tick-major? xtick)
               (values (xdigit->sticker id (plot-tick-desc xtick) digit-font digit-color) xmajor-tick)
               (values 'minor xminor-tick)))

         (cond [(not (flnear? xval 0.0))
                (plot-axis-sticker-cons maybe-sticker xdigit-anchor (origin-dot->pos xval 0.0)
                                        xdigit-offset xticks xtick-min real-part xtick-max gtick)]
               [(or 0-as-xdigit?)
                (plot-axis-sticker-cons maybe-sticker xdigit-anchor (origin-dot->pos xval 0.0)
                                        xdigit-offset xticks xtick-min real-part xtick-max #false)]
               [else xticks]))
       
       ; y-axis's labels
       (for/fold ([labels : (Listof (GLayerof Geo)) null])
                 ([lbl (in-list (plot-axis-label-settings y-label y-desc yaxis-min yaxis-max (if (not label-at-axis?) 0.0 (* em 1.5))))])
         (if (car lbl)
             (let ([lbl.geo (plot-y-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color)])
               (case/eq (plot-axis-style-label-placement axis-style)
                 [(digit)  (plot-axis-sticker-cons* lbl.geo ydigit-anchor (flc-ri (- (cadr lbl))) (+ Oshadow ydigit-offset) labels)]
                 [(mirror) (plot-axis-sticker-cons* lbl.geo ymiror-anchor (flc-ri (- (cadr lbl))) (+ Oshadow ymiror-offset) labels)]
                 [else (plot-axis-sticker-cons* lbl.geo 'cc (flc-ri (- (cadr lbl))) Oshadow labels)]))
             labels))

       ; y-axis's ticks
       (for/fold ([yticks : (Listof (GLayerof Geo)) null])
                 ([ytick (in-list actual-yticks)])
         (define yval : Flonum (real->double-flonum (plot-tick-value ytick)))
         (define-values (maybe-sticker gtick)
           (if (plot-tick-major? ytick)
               (values (ydigit->sticker id (plot-tick-desc ytick) digit-font digit-color) ymajor-tick)
               (values 'minor yminor-tick)))

         (if (not (flnear? yval 0.0))
             (plot-axis-sticker-cons* maybe-sticker ydigit-anchor (origin-dot->pos 0.0 yval) ydigit-offset yticks gtick)
             yticks))

       ; visualizers' labels
       (for/list : (Listof (GLayerof Geo)) ([self (in-list plots)]
                                            #:when (geo:visualizer-label self))
         (define-values (real-pin real-gap)
           (plot-mark-vector-values mark-style
                                    (or (geo:visualizer-pin-angle self) 0.0)
                                    (or (geo:visualizer-gap-angle self) 0.0)))
         
         ;; visualizer should ensure its label being pinned at visible point 
         (geo-path-self-pin-layer
          (plot-marker #:color (geo:visualizer-color self) #:font mark-font #:pin-stroke pin-pen
                       #:fallback-pin real-pin #:fallback-gap real-gap
                       #:fallback-anchor mark-anchor #:length-base em
                       (geo:visualizer-label self) origin-dot->pos)))))
    
    (define translated-layers : (Option (GLayer-Groupof Geo))
      (if (not 0-as-xdigit?)
          (geo-layers-try-extend (geo-own-pin-layer (geo-anchor-merge xdigit-anchor ydigit-anchor)
                                                    Origin zero (+ ydigit-offset xdigit-offset))
                                 layers)
          (and (pair? layers)
               (geo-layers-try-extend layers 0.0 0.0))))
    
    (define delta-origin : Float-Complex
      (if (or translated-layers)
          (+ Origin (geo-layer-position (car (glayer-group-layers translated-layers))))
          Origin))

    (if (or translated-layers)
        (create-geometry-group plot:cartesian id #false #false
                               #:border bdr #:background bg
                               #:margin margin #:padding padding
                               translated-layers delta-origin
                               (map plot-tick-value actual-xticks)
                               (map plot-tick-value actual-yticks)
                               (let ([dot->pos (λ [[x : Flonum] [y : Flonum]] : Float-Complex
                                                 (+ delta-origin (* x xunit) (* (- y) yunit)))])
                                 (case-lambda
                                   [(x y) (dot->pos x y)]
                                   [(dot) (dot->pos (real-part dot) (imag-part dot))])))
        
        ; just in case. say, infinite width
        ; WARNING: this would be also true, if rotation is supported.
        (create-geometry-group plot:cartesian id #false #false
                               #:border bdr #:background bg
                               #:margin margin #:padding padding
                               (geo-own-layers zero) delta-origin null null
                               origin-dot->pos))))
  