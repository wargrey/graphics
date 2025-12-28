#lang typed/racket/base

(provide (all-defined-out) Plot-Mark-Auto-Anchor Plot-Cartesian-Layer)
(provide Plot:Cartesian plot:cartesian?)
(provide default-plot-cartesian-layer-order)
(provide (all-from-out "digitama/base.rkt"))
(provide (all-from-out "digitama/visualizer.rkt"))
(provide (all-from-out "digitama/visualizer/grid/self.rkt"))
(provide (all-from-out "digitama/visualizer/vaid/self.rkt"))

(require racket/case)
(require digimon/metrics)

(require geofun/digitama/dc/grid)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/merge)
(require geofun/digitama/layer/position)
(require geofun/digitama/geometry/footprint)

(require "digitama/visualizer.rkt")

(require "digitama/axis/self.rkt")
(require "digitama/axis/config.rkt")
(require "digitama/axis/sticker.rkt")
(require "digitama/axis/tick.rkt")

(require "digitama/marker/dc.rkt")
(require "digitama/marker/config.rkt")
(require "digitama/marker/anchor.rkt")

(require "digitama/visualizer/self.rkt")
(require "digitama/visualizer/layer.rkt")
(require "digitama/visualizer/realize.rkt")
(require "digitama/visualizer/interface.rkt")
(require "digitama/visualizer/grid/self.rkt")
(require "digitama/visualizer/grid/tick.rkt")
(require "digitama/visualizer/grid/config.rkt")
(require "digitama/visualizer/vaid/self.rkt")

(require "digitama/base.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-cartesian
  (lambda [#:id [id : (Option Symbol) #false]
           #:screen? [screen? : Boolean #false]
           #:O [chO : Char #\O]
           #:origin [maybe-origin : (Option Complex) #false]
           #:unit-length [maybe-unit : (Option Complex+%) #false]
           #:width [width : Real (default-plot-cartesian-view-width)]
           #:height [height : Real (default-plot-cartesian-view-height)]
           #:style [axis-style : Plot-Axis-Style (default-plot-axis-style)]
           #:mark-style [mark-style : (Option Plot-Mark-Style) #false]
           #:x-label [x-label : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) "x"]
           #:y-label [y-label : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) "y"]
           #:x-unit-desc [x-unit-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:y-unit-desc [y-unit-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:x-desc [x-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:y-desc [y-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:x-range [xtick-hint : (U Real Plot-Visualizer-View-Range False) #false]
           #:y-range [ytick-hint : (U Real Plot-Visualizer-View-Range False) #false]
           #:x-ticks [xticks-engine : Plot-Tick-Engine (plot-real-ticks*)]
           #:y-ticks [yticks-engine : Plot-Tick-Engine xticks-engine]
           #:x-tick-format [xtick-format : (Option Plot-Tick-Format) #false]
           #:y-tick-format [ytick-format : (Option Plot-Tick-Format) xtick-format]
           #:x-tick->sticker [xdigit->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:y-tick->sticker [ydigit->sticker : Plot-Axis-Tick->Sticker xdigit->sticker]
           #:x-grid-style [xgrid-style : (Option Plot-Grid-Style) (default-plot-grid-style)]
           #:y-grid-style [ygrid-style : (Option Plot-Grid-Style) xgrid-style]
           #:palette [palette : Plot-Palette (default-plot-palette)]
           #:layer-order [layer-order : (Listof Plot-Cartesian-Layer) (default-plot-cartesian-layer-order)]
           #:fallback-range [fallback-dom : (Pairof Real Real) (default-plot-visualizer-domain-range)]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Spacing) #false]
           #:padding [padding : (Option Geo-Spacing) #false]
           . [tree : (U Plot-Visualizer Plot-Visualizer-Tree) *]] : Plot:Cartesian
    (define-values (visualizers maybe-xivl maybe-yivl) (plot-visualizer-tree-flatten tree))
    (define-values (xview0 yview0)
      (plot-visualizer-ranges visualizers
                              (cond [(procedure? xtick-hint) xtick-hint]
                                    [(or xtick-hint) (plot-tick-range xtick-hint)]
                                    [else (plot-tick-engine-range xticks-engine)])
                              (cond [(procedure? ytick-hint) ytick-hint]
                                    [(or ytick-hint) (plot-tick-range ytick-hint)]
                                    [else (plot-tick-engine-range yticks-engine)])
                              maybe-xivl maybe-yivl fallback-dom))
    (define-values (xview yview)
      (values (or (plot-tick-range xview0) xview0)
              (or (plot-tick-range yview0) yview0)))

    (define yrel-scale (real->double-flonum (/ (plot-tick-engine-unit-scale xticks-engine) (plot-tick-engine-unit-scale yticks-engine))))
    (define-values (maybe-xorig maybe-yorig) (plot-cartesian-maybe-settings maybe-origin))
    (define-values (maybe-xunit maybe-yunit) (plot-cartesian-maybe-settings maybe-unit))
    (define-values (x-tip y-tip) (values (plot-axis-tip axis-style 'x) (plot-axis-tip axis-style 'y)))
    (define-values ( flwidth  view-width x-neg-margin x-pos-margin) (plot-axis-length-values axis-style x-tip width))
    (define-values (flheight view-height y-neg-margin0 y-pos-margin0)
      (cond [(rational? height) (plot-axis-length-values axis-style y-tip height flwidth)]
            [else (plot-axis-height-values axis-style y-tip view-width
                                           (* (/ (- (cdr yview) (car yview))
                                                 (- (cdr xview) (car xview)))
                                              yrel-scale))]))

    (define-values (y-neg-margin y-pos-margin) (if (not screen?) (values y-neg-margin0 y-pos-margin0) (values y-pos-margin0 y-neg-margin0)))
    (define-values (xtick-range xO xunit) (plot-axis-metrics  xview maybe-xorig  view-width maybe-xunit))
    (define-values (ytick-range yO yU yunit) (plot-axis-metrics* yview maybe-yorig view-height (if (rational? height) maybe-yunit (* xunit yrel-scale)) flc-ri))

    (define-values (actual-xticks maybe-xstep xminor-count) (plot-ticks-generate xticks-engine xtick-range xtick-format))
    (define-values (actual-yticks maybe-ystep yminor-count)
      (cond [(rational? height)
             (plot-ticks-generate yticks-engine ytick-range ytick-format)]
            [(not (and (rational? maybe-xstep) (positive? maybe-xstep)))
             (plot-ticks-generate yticks-engine ytick-range ytick-format)]
            [else ; copied from the xtick engine
             (plot-ticks-generate (plot-interval-ticks #:minor-count xminor-count
                                                       maybe-xstep (plot-tick-engine-format yticks-engine))
                                  ytick-range ytick-format)]))
    
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
    (define zero : Geo (geo-text (string chO) label-font #:color label-color))

    ; The x-axis is the major axis, which is placed at screen (0, 0)
    ; So, `Oview` is used to adjust the place of the y-axis,
    ;   instead of calculating vertical positions of real points on screen.
    ; Whereas the `Origin` is the math concept, along with `xunit` and `yunit`,
    ;   make the actual transformation be simplified and elegant.
    (define y-cart-transform : (-> Flonum Flonum) (if (not screen?) - values))
    (define Origin : Float-Complex (make-rectangular (+ (* view-width xO) xtick-min) 0.0))
    (define Oview : Float-Complex (+ Origin (flc-ri (+ (* view-height (if (not screen?) yO (- 1.0 yO))) ytick-min))))

    (define tick-pen : Pen (desc-stroke axis-pen #:width fltick-thickness #:color tick-color))
    (define-values (xdigit-position xdigit-anchor xmiror-anchor) (plot-axis-digit-position-values axis-style 'x))
    (define-values (ydigit-position ydigit-anchor ymiror-anchor) (plot-axis-digit-position-values axis-style 'y))
    (define-values (xmajor-pen xminor-pen xg-minor-count) (plot-grid-visual-values xgrid-style xminor-count adjust-color))
    (define-values (ymajor-pen yminor-pen yg-minor-count) (plot-grid-visual-values ygrid-style yminor-count adjust-color))
    
    (define xaxis : Geo:Path:Self
      (geo-path* #:stroke axis-pen #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape x-tip)
                 #:target-tip (plot-axis-tip-style-positive-shape x-tip)
                 (list the-M0 (gpp:point #\L (make-rectangular flwidth 0.0)))))
   
    (define yaxis : Geo:Path:Self
      (geo-path* #:stroke axis-pen #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape (or y-tip x-tip))
                 #:target-tip (plot-axis-tip-style-positive-shape (or y-tip x-tip))
                 (list the-M0 (gpp:point #\L (flc-ri (y-cart-transform flheight))))))

    (define xdigit-offset : Float-Complex (flc-ri (* xdigit-position em (if (not screen?) -1.0 +1.0))))
    (define xmiror-offset : Float-Complex (flc-ri (* xdigit-position em (if (not screen?) +1.0 -1.0))))
    (define ydigit-offset : Float-Complex (make-rectangular (* ydigit-position em) 0.0))
    (define ymiror-offset : Float-Complex (make-rectangular (* ydigit-position (- em)) 0.0))
    
    (define-values (xsoff xeoff) (geo-path-endpoint-offsets xaxis))
    (define-values (ysoff yeoff) (geo-path-endpoint-offsets yaxis))
    (define label-at-axis? : Boolean (eq? (plot-axis-style-label-placement axis-style) 'axis))
    (define xaxis-min : Flonum (+ (- xtick-min x-neg-margin) (real-part xsoff)))
    (define xaxis-max : Flonum (+ xtick-max x-pos-margin (real-part xeoff)))
    (define yaxis-min : Flonum (+ (- ytick-min y-neg-margin) (imag-part ysoff)))
    (define yaxis-max : Flonum (+ ytick-max y-pos-margin (imag-part yeoff)))

    (define origin-dot->pos : Plot-Position-Transform
      (case-lambda
        [(x y) (+ Origin (* x xunit) (* (y-cart-transform y) yunit))]
        [(dot) (origin-dot->pos (real-part dot) (imag-part dot))]))

    (define-values (visible-xticks visible-xview) (plot-ticks-trim actual-xticks xO xunit  view-width xview))
    (define-values (visible-yticks visible-yview) (plot-ticks-trim actual-yticks yO yU    view-height yview))
    
    (define 0-as-xdigit? : Boolean
      (and (negative? (car visible-xview))
           (< (+ (* view-height yO) fltick-min y-neg-margin0)
              (* xdigit-position em -1.0))
           #;(null? actual-yticks)))

    (define plots : (Listof Geo-Visualizer) (plot-realize-all visualizers visible-xview visible-yview origin-dot->pos palette bg-color))
    (define-values (visible-pos visible-diag) (plot-diagonal* origin-dot->pos visible-xview visible-yview screen?))
    (define-values (visible-width visible-height) (values (abs (real-part visible-diag)) (abs (imag-part visible-diag))))
    
    (define xgrid : (Option Geo:Grid)
      (and (or xmajor-pen xminor-pen)
           (> visible-width 0.0) (> visible-height 0.0)
           (let-values ([(majors minors) (plot-grid-values visible-xticks xminor-count xg-minor-count (plot-x-transform origin-dot->pos visible-pos))])
             (geo-grid #:major-stroke xmajor-pen #:minor-stroke xminor-pen
                       #:extra-major-xs majors #:extra-minor-xs minors
                       visible-width visible-height))))
    
    (define ygrid : (Option Geo:Grid)
      (and (or ymajor-pen yminor-pen)
           (> visible-width 0.0) (> visible-height 0.0)
           (let-values ([(majors minors) (plot-grid-values visible-yticks yminor-count yg-minor-count (plot-y-transform origin-dot->pos visible-pos))])
             (geo-grid #:major-stroke ymajor-pen #:minor-stroke yminor-pen
                       #:extra-major-ys majors #:extra-minor-ys minors
                       visible-width visible-height))))

    (define x-tick-layers : (Listof (GLayerof Geo))
      (append
       ; x-axis's labels
       (for/fold ([labels : (Listof (GLayerof Geo)) null])
                 ([lbl (in-list (plot-axis-label-settings x-label x-desc xaxis-min xaxis-max (if (or label-at-axis?) (* em 0.618) 0.0) 'x))])
         (if (car lbl)
             (let ([lbl.geo (plot-x-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color (* em 0.382))])
               (case/eq (plot-axis-style-label-placement axis-style)
                 [(digit  digit-mirror) (plot-axis-sticker-cons* lbl.geo xdigit-anchor (+ (cadr lbl) xdigit-offset) 0.0+0.0i labels)]
                 [(mirror mirror-digit) (plot-axis-sticker-cons* lbl.geo xmiror-anchor (+ (cadr lbl) xmiror-offset) 0.0+0.0i labels)]
                 [else (plot-axis-sticker-cons* lbl.geo (cadddr lbl) (make-rectangular (cadr lbl) +0.0) 0.0+0.0i labels)]))
             labels))

       ; x-axis's ticks
       (for/fold ([xticks : (Listof (GLayerof Geo)) null])
                 ([xtick (in-list visible-xticks)])
         (define xval : Flonum (real->double-flonum (plot-tick-value xtick)))
         (define-values (maybe-sticker gtick)
           (if (plot-tick-major? xtick)
               (values (xdigit->sticker id (plot-tick-desc xtick) digit-font digit-color)
                       (plot-axis-xtick-sticker fltick-length (plot-axis-style-xtick-placement axis-style screen?) tick-pen))
               (values 'minor
                       (plot-axis-xtick-sticker fltick-sublen (plot-axis-style-xtick-placement axis-style screen?) tick-pen))))

         (cond [(not (flnear? xval 0.0))
                (plot-axis-sticker-cons maybe-sticker xdigit-anchor (origin-dot->pos xval 0.0)
                                        xdigit-offset xticks xtick-min real-part xtick-max gtick)]
               [(or 0-as-xdigit?)
                (plot-axis-sticker-cons maybe-sticker xdigit-anchor (origin-dot->pos xval 0.0)
                                        xdigit-offset xticks xtick-min real-part xtick-max #false)]
               [else xticks]))))

    (define y-tick-layers : (Listof (GLayerof Geo))
      ;;; WARNING
      ; bacause of the complicated translation on the y-axis,
      ; the `ytick-min` and `ytick-max` are not reliable for filtering out visualizers that are out of the view.
      ;
      ; Besides, visualizer implementors should ensure their plots actually be located in visible region.
      (append
       ; y-axis's labels.
       (for/fold ([labels : (Listof (GLayerof Geo)) null])
                 ([lbl (in-list (plot-axis-label-settings (plot-screen-axis-label-adjust y-label screen?)
                                                          (plot-screen-axis-label-adjust y-desc screen?)
                                                          yaxis-min yaxis-max (if (or label-at-axis?) (* em 0.618) (* em -0.5)) 'y))])
         (if (car lbl)
             (let ([lbl.geo (plot-y-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color)])
               (case/eq (plot-axis-style-label-placement axis-style)
                 [(digit  mirror-digit) (plot-axis-sticker-cons* lbl.geo ydigit-anchor (flc-ri (- (cadr lbl))) (+ Oview ydigit-offset) labels)]
                 [(mirror digit-mirror) (plot-axis-sticker-cons* lbl.geo ymiror-anchor (flc-ri (- (cadr lbl))) (+ Oview ymiror-offset) labels)]
                 [else (plot-axis-sticker-cons* lbl.geo (cadddr lbl) (flc-ri (- (cadr lbl))) Oview labels)]))
             labels))

       ; y-axis's ticks
       (for/fold ([yticks : (Listof (GLayerof Geo)) null])
                 ([ytick (in-list visible-yticks)])
         (define yval : Flonum (real->double-flonum (plot-tick-value ytick)))
         (define-values (maybe-sticker gtick)
           (if (plot-tick-major? ytick)
               (values (ydigit->sticker id (plot-tick-desc ytick) digit-font digit-color)
                       (plot-axis-ytick-sticker fltick-length (plot-axis-style-tick-placement axis-style) tick-pen))
               (values 'minor
                       (plot-axis-ytick-sticker fltick-sublen (plot-axis-style-tick-placement axis-style) tick-pen))))

         (if (not (flnear? yval 0.0))
             (plot-axis-sticker-cons* maybe-sticker ydigit-anchor (origin-dot->pos 0.0 yval) ydigit-offset yticks gtick)
             yticks))))

    (define annotation-layers
      ; visualizers' data labels
      (for/list : (Listof (GLayerof Geo)) ([self (in-list plots)] #:when (geo:visualizer-label self))
        (define-values (real-pin real-gap)
          (plot-mark-vector-values mark-style
                                   (or (geo:visualizer-pin-angle self) 0.0)
                                   (or (geo:visualizer-gap-angle self) 0.0)))
        
        ;; visualizer should ensure its label being pinned at visible point
        (geo-path-self-pin-layer
         (plot-marker #:color (geo:visualizer-color self) #:font mark-font #:pin-stroke pin-pen
                      #:fallback-pin real-pin #:fallback-gap real-gap
                      #:fallback-anchor mark-anchor #:length-base em
                      (assert (geo:visualizer-label self)) origin-dot->pos))))

    (define layer-groups : (Immutable-HashTable Plot-Cartesian-Layer (Listof (GLayerof Geo)))
      ((inst make-immutable-hasheq Plot-Cartesian-Layer (Listof (GLayerof Geo)))
       (list (cons 'grid (cond [(and xgrid ygrid) (list (geo-own-pin-layer 'lt visible-pos xgrid 0.0+0.0i)
                                                        (geo-own-pin-layer 'lt visible-pos ygrid 0.0+0.0i))]
                               [(or xgrid) (list (geo-own-pin-layer 'lt visible-pos xgrid 0.0+0.0i))]
                               [(or ygrid) (list (geo-own-pin-layer 'lt visible-pos ygrid 0.0+0.0i))]
                               [else null]))
             (cons 'axes (list (geo-own-pin-layer 'lc 0.0+0.0i xaxis 0.0+0.0i)
                               (geo-own-pin-layer 'cb 0.0+0.0i yaxis Oview)))
             
             (cons 'tick (append x-tick-layers y-tick-layers))

             (cons 'aid null)

             (cons 'annotation annotation-layers))))
    
    (define layers : (Listof (GLayerof Geo)) (plot-cartesian-layers layer-groups plots layer-order))
    (define translated-layers : (Option (GLayer-Groupof Geo))
      (if (not 0-as-xdigit?)
          (geo-layers-try-push-back (geo-own-pin-layer (geo-anchor-merge xdigit-anchor ydigit-anchor)
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
                               (map plot-tick-value visible-xticks)
                               (map plot-tick-value visible-yticks)
                               (let ([dot->pos (λ [[x : Flonum] [y : Flonum]] : Float-Complex
                                                 (+ delta-origin (* x xunit) (* (y-cart-transform y) yunit)))])
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
  