#lang typed/racket/base

(provide (all-defined-out))
(provide Plot:Axis plot:axis?)
(provide plot-axis-real-values plot-axis-integer-values)
(provide default-plot-axis-integer-filter default-plot-axis-real-filter)
(provide (all-from-out "digitama/axis/style.rkt"))
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide (all-from-out "digitama/axis/singleton.rkt"))
(provide (all-from-out "digitama/axis/tick/self.rkt"))
(provide (all-from-out "digitama/axis/tick/real.rkt"))

(require digimon/metrics)

(require geofun/font)
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
(require geofun/digitama/geometry/footprint)

(require "digitama/axis/self.rkt")
(require "digitama/axis/real.rkt")
(require "digitama/axis/sticker.rkt")
(require "digitama/axis/interface.rkt")
(require "digitama/axis/singleton.rkt")
(require "digitama/axis/style.rkt")
(require "digitama/axis/config.rkt")
(require "digitama/axis/tick.rkt")
(require "digitama/axis/tick/self.rkt")
(require "digitama/axis/tick/real.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis
  (lambda [#:id [id : (Option Symbol) #false]
           #:length [length : Real (default-plot-axis-length)]
           #:unit-length [maybe-unit : (Option Real) (default-plot-axis-unit-length)]
           #:origin [maybe-origin : (Option Real) #false]
           #:style [axis-style : Plot-Axis-Style (default-plot-axis-style)]
           #:tip [tip : Plot-Axis-Tip-Style (default-plot-axis-tip-style)]
           #:label [axis-label : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:desc [axis-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:range [tick-hint : (U Real (Pairof Real Real) False) #false]
           #:ticks [ticks-engine : Plot-Tick-Engine (plot-real-ticks)]
           #:tick-format [alt-format : (Option Plot-Tick-Format) #false]
           #:tick->sticker [tick->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:real-style [real-style : (Option Plot-Mark-Style) #false]
           #:real-filter [real-filter : (Option Plot-Axis-Real-Filter) #false]
           #:real->sticker [real->label : Plot-Axis-Real->Sticker default-plot-axis-real->sticker]
           #:real->dot [real->dot : Plot-Axis-Real->Dot default-plot-axis-real->dot]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false]
           #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           [real-list : (U (Listof Plot-Axis-Real-Datum) (-> Real Any)) null]] : Plot:Axis
    (define-values (fllength used-length neg-margin pos-margin) (plot-axis-length-values axis-style tip length))
    (define-values (tick-range origin flunit)
      (plot-axis-metrics (or tick-hint (plot-tick-engine-range ticks-engine))
                         (plot-axis-real-range real-list)
                         maybe-origin used-length maybe-unit))

    (define-values (actual-ticks _) (plot-ticks-generate ticks-engine tick-range alt-format))
    (define actual-tick-values (map plot-tick-value* actual-ticks))

    (define-values (digit-font label-font desc-font axis-color digit-color tick-color label-color desc-color) (plot-axis-visual-values axis-style))
    (define-values (digit-position digit-anchor) (plot-axis-digit-position-values axis-style 'x))
    (define em : Flonum (font-metrics-ref digit-font 'em))

    (define flthickness : Nonnegative-Flonum (plot-axis-style-thickness axis-style))
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness axis-style) flthickness))
    (define fltick-length : Nonnegative-Flonum (~length (plot-axis-tick-length axis-style 'x) flthickness))
    (define fltick-min : Nonnegative-Flonum (+ neg-margin (* fltick-thickness 0.5)))
    (define fltick-max : Nonnegative-Flonum (+ fltick-min used-length))
    
    (define main-axis : Geo:Edge
      (geo-edge* #:stroke (desc-stroke #:color axis-color #:width flthickness) #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape tip)
                 #:target-tip (plot-axis-tip-style-positive-shape tip)
                 (list the-M0 (gpp:point #\L (make-rectangular fllength 0.0)))))

    (define arrow-half : Nonnegative-Flonum (* (geo-height main-axis) 0.5))
    (define flc-origin : Float-Complex (make-rectangular (+ (* used-length origin) fltick-min) arrow-half))
    (define flc-offset : Float-Complex (make-rectangular 0.0 (* digit-position (- em))))
    
    (define-values (soff eoff) (geo-edge-endpoint-offsets main-axis))
    (define label-as-digit? : Boolean (eq? (plot-axis-style-label-position axis-style) 'digit))
    (define flaxis-min : Flonum (- fltick-min neg-margin (- (real-part soff))))
    (define flaxis-max : Flonum (+ fltick-max pos-margin (real-part eoff)))

    (define gtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-length #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor axis-style 'x))))

    (define dot->pos : Plot-Axis-Position-Map
      (lambda [x]
        (+ flc-origin (* x flunit))))

    (define layers : (Listof (GLayerof Geo))
      (append (for/fold ([labels : (Listof (GLayerof Geo)) null])
                        ([lbl (in-list (plot-axis-label-settings axis-label axis-desc flaxis-min flaxis-max (if (or label-as-digit?) 0.0 (* em 0.6))))])
                (if (car lbl)
                    (let ([lbl.geo (plot-x-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color (* em 0.4))])
                      (if (or label-as-digit?)
                          (plot-axis-sticker-cons lbl.geo 'cc (+ (cadr lbl) flc-offset) 0.0+0.0i labels -inf.0 real-part +inf.0 #false)
                          (plot-axis-sticker-cons lbl.geo (cadddr lbl) (make-rectangular (cadr lbl) arrow-half) 0.0+0.0i labels -inf.0 real-part +inf.0 #false)))
                    labels))
              
              (for/fold ([ticks : (Listof (GLayerof Geo)) null])
                        ([tick (in-list actual-ticks)])
                (plot-axis-sticker-cons (tick->sticker id (cdr tick) digit-font digit-color) digit-anchor
                                        (dot->pos (real->double-flonum (plot-tick-value (car tick))))
                                        flc-offset ticks fltick-min real-part fltick-max gtick))
              
              (let-values ([(real-font real-color real-position real-anchor dot-radius) (plot-mark-style-values real-style axis-style)])
                (for/fold ([reals : (Listof (GLayerof Geo)) null])
                          ([real (in-list (cond [(list? real-list) real-list]
                                                [else (plot-axis-reals-from-producer real-list actual-tick-values)]))])
                  (define-values (val obj) ((or real-filter (default-plot-axis-real-filter)) real))
                  
                  (if (or val)
                      (let*-values ([(real-offset) (make-rectangular 0.0 (* real-position (- em)))]
                                    [(label dot) (plot-axis-real-label-values id val obj real-font real-color dot-radius
                                                                              real->label real->dot flunit real-anchor)])
                        (plot-axis-sticker-cons (car label) (cdr label) (dot->pos val)
                                                real-offset reals fltick-min real-part fltick-max dot))
                      reals)))))
  
    (define translated-layers : (Option (GLayer-Groupof Geo)) (geo-path-try-extend/list (geo-own-layer main-axis) layers))
    
    (if (or translated-layers)
        (let ([delta-origin (+ flc-origin (geo-layer-position (car (glayer-group-layers translated-layers))))])
          (create-geometry-group plot:axis id #false #false
                                 #:border bdr #:background bg
                                 #:margin margin #:padding padding
                                 translated-layers delta-origin actual-tick-values
                                 (λ [[x : Flonum]] (+ delta-origin (* x flunit)))))
        (create-geometry-group plot:axis id #false #false
                               #:border bdr #:background bg
                               #:margin margin #:padding padding
                               (geo-own-layers main-axis) flc-origin actual-tick-values
                               dot->pos))))

(define plot-integer-axis
  (lambda [#:id [id : (Option Symbol) #false]
           #:length [length : Real (default-plot-axis-length)]
           #:unit-length [maybe-unit : (Option Real) (default-plot-axis-unit-length)]
           #:origin [maybe-origin : (Option Real) #false]
           #:style [axis-style : Plot-Axis-Style (default-plot-axis-style)]
           #:tip [tip : Plot-Axis-Tip-Style (default-plot-axis-tip-style)]
           #:label [axis-label : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:desc [axis-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:range [tick-hint : (U Integer (Pairof Integer Integer) False) #false]
           #:ticks [ticks-engine : Plot-Tick-Engine (plot-integer-ticks)]
           #:tick-format [alt-format : (Option Plot-Tick-Format) #false]
           #:tick->sticker [tick->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:integer-style [int-style : (Option Plot-Mark-Style) #false]
           #:integer-filter [int-filter : (Option Plot-Axis-Integer-Filter) #false]
           #:integer->sticker [int->label : Plot-Axis-Real->Sticker default-plot-axis-real->sticker]
           #:integer->dot [int->dot : Plot-Axis-Real->Dot default-plot-axis-real->dot]
           #:exclude-zero? [exclude-zero? : Boolean #true]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false]
           #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           [number-sequence : (U (Listof Plot-Axis-Integer-Datum) (Vectorof Any) (-> Integer Any)) null]] : Plot:Axis
    (define-values (fllength used-length neg-margin pos-margin) (plot-axis-length-values axis-style tip length))
    (define-values (tick-range origin flunit)
      (plot-axis-metrics (or (plot-tick-engine-range ticks-engine) tick-hint)
                         (plot-axis-integer-range number-sequence exclude-zero?)
                         maybe-origin used-length maybe-unit))
    
    (define-values (actual-ticks _) (plot-ticks-generate ticks-engine tick-range alt-format))
    (define actual-tick-values (filter exact-integer? (map plot-tick-value* actual-ticks)))

    (define-values (digit-font label-font desc-font axis-color digit-color tick-color label-color desc-color) (plot-axis-visual-values axis-style))
    (define-values (digit-position digit-anchor) (plot-axis-digit-position-values axis-style 'x))
    (define em : Flonum (font-metrics-ref digit-font 'em))
    
    (define flthickness : Nonnegative-Flonum (plot-axis-style-thickness axis-style))
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness axis-style) flthickness))
    (define fltick-length : Nonnegative-Flonum (~length (plot-axis-tick-length axis-style 'x) flthickness))
    (define fltick-min : Nonnegative-Flonum (+ neg-margin (* fltick-thickness 0.5)))
    (define fltick-max : Nonnegative-Flonum (+ fltick-min used-length))
    
    (define main-axis : Geo:Edge
      (geo-edge* #:stroke (desc-stroke #:color axis-color #:width flthickness) #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape tip)
                 #:target-tip (plot-axis-tip-style-positive-shape tip)
                 (list the-M0 (gpp:point #\L (make-rectangular fllength 0.0)))))

    (define arrow-half : Nonnegative-Flonum (* (geo-height main-axis) 0.5))
    (define flc-origin : Float-Complex (make-rectangular (+ (* used-length origin) fltick-min) arrow-half))
    (define flc-offset : Float-Complex (make-rectangular 0.0 (* digit-position (- em))))

    (define-values (soff eoff) (geo-edge-endpoint-offsets main-axis))
    (define label-as-digit? : Boolean (eq? (plot-axis-style-label-position axis-style) 'digit))
    (define flaxis-min : Flonum (- fltick-min neg-margin (- (real-part soff))))
    (define flaxis-max : Flonum (+ fltick-max pos-margin (real-part eoff)))

    (define gtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-length #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor axis-style 'x))))

    (define dot->pos : Plot-Axis-Position-Map
      (lambda [x]
        (+ flc-origin (* x flunit))))

    (define layers : (Listof (GLayerof Geo))
      (parameterize ([default-plot-axis-integer-filter ((if (not exclude-zero?) values plot-axis-nonzero-values-wrap)
                                                        (or int-filter (default-plot-axis-integer-filter)))])
        (append (for/fold ([labels : (Listof (GLayerof Geo)) null])
                          ([lbl (in-list (plot-axis-label-settings axis-label axis-desc flaxis-min flaxis-max (if (or label-as-digit?) 0.0 (* em 0.6))))])
                  (if (car lbl)
                      (let ([lbl.geo (plot-x-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color (* em 0.4))])
                        (if (or label-as-digit?)
                            (plot-axis-sticker-cons lbl.geo 'cc (+ (cadr lbl) flc-offset) 0.0+0.0i labels -inf.0 real-part +inf.0 #false)
                            (plot-axis-sticker-cons lbl.geo (cadddr lbl) (make-rectangular (cadr lbl) arrow-half) 0.0+0.0i labels -inf.0 real-part +inf.0 #false)))
                      labels))

                (for/fold ([ticks : (Listof (GLayerof Geo)) null])
                          ([tick actual-ticks])
                  (plot-axis-sticker-cons (tick->sticker id (cdr tick) digit-font digit-color) digit-anchor
                                          (dot->pos (real->double-flonum (plot-tick-value* tick)))
                                          flc-offset ticks fltick-min real-part fltick-max gtick))

                (let-values ([(int-font int-color int-position int-anchor dot-radius) (plot-mark-style-values int-style axis-style)])
                  (for/fold ([reals : (Listof (GLayerof Geo)) null])
                            ([real (in-list (cond [(list? number-sequence) number-sequence]
                                                  [(vector? number-sequence) (plot-axis-reals-from-vector number-sequence (if (not exclude-zero?) 0 1))]
                                                  [else (plot-axis-reals-from-producer number-sequence actual-tick-values)]))])
                    (define-values (val obj) ((default-plot-axis-integer-filter) real))
                    
                    (if (or val)
                        (let*-values ([(ival) (exact->inexact val)]
                                      [(real-offset) (make-rectangular 0.0 (* int-position (- em)))]
                                      [(label dot) (plot-axis-real-label-values id ival obj int-font int-color dot-radius
                                                                                int->label int->dot flunit int-anchor)])
                          (plot-axis-sticker-cons (car label) (cdr label) (dot->pos ival)
                                                  real-offset reals fltick-min real-part fltick-max dot))
                        reals))))))

    (define translated-layers : (Option (GLayer-Groupof Geo)) (geo-path-try-extend/list (geo-own-layer main-axis) layers))

    (if (or translated-layers)
        (let ([delta-origin (+ flc-origin (geo-layer-position (car (glayer-group-layers translated-layers))))])
          (create-geometry-group plot:axis id #false #false
                                 #:border bdr #:background bg
                                 #:margin margin #:padding padding
                                 translated-layers delta-origin actual-tick-values
                                 (λ [[x : Flonum]] (+ delta-origin (* x flunit)))))
        (create-geometry-group plot:axis id #false #false
                               #:border bdr #:background bg
                               #:margin margin #:padding padding
                               (geo-own-layers main-axis) flc-origin actual-tick-values
                               dot->pos))))
  