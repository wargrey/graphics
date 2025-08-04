#lang typed/racket/base

(provide (all-defined-out))
(provide Plot:Line plot:line?)
(provide Plot-Mark-Auto-Anchor)
(provide (rename-out [plot-real-line plot-line]))
(provide (all-from-out "digitama/axis/style.rkt"))
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide (all-from-out "digitama/axis/singleton.rkt"))
(provide (all-from-out "digitama/axis/tick/self.rkt"))
(provide (all-from-out "digitama/axis/tick/real.rkt"))
(provide (all-from-out "digitama/marker/self.rkt"))
(provide (all-from-out "digitama/marker/style.rkt"))

(require racket/case)
(require racket/math)

(require digimon/metrics)
(require digimon/constant)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)
(require geofun/constructor)

(require geofun/digitama/markup)
(require geofun/digitama/convert)
(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/merge)
(require geofun/digitama/paint/self)
(require geofun/digitama/path/tip)
(require geofun/digitama/path/tips)

(require geofun/digitama/geometry/footprint)
(require geofun/digitama/geometry/computation/line)

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

(require "digitama/marker/dc.rkt")
(require "digitama/marker/self.rkt")
(require "digitama/marker/style.rkt")
(require "digitama/marker/config.rkt")
(require "digitama/marker/anchor.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-real-line
  (lambda [#:id [id : (Option Symbol) #false]
           #:length [length : Real (default-plot-axis-length)]
           #:unit-length [maybe-unit : (Option Real+%) (default-plot-axis-unit-length)]
           #:origin [maybe-origin : (Option Real) #false]
           #:style [axis-style : Plot-Axis-Style (default-plot-axis-style)]
           #:label [axis-label : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:desc [axis-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:range [tick-hint : (U Real (Pairof Real Real) False) #false]
           #:ticks [ticks-engine : Plot-Tick-Engine (plot-real-ticks*)]
           #:tick-format [alt-format : (Option Plot-Tick-Format) #false]
           #:tick->sticker [tick->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:mark-style [real-style : (Option Plot-Mark-Style) #false]
           #:mark-template [desc-real : (U Plot-Mark->Description Plot:Mark) plot-desc-real]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false]
           #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:rotate [rotate : Real 0.0]
           [real-list : (U (Listof Plot-Axis-Real-Datum) (-> Real Any)) null]] : Plot:Line
    (define tip : Plot-Axis-Tip-Style (plot-axis-tip axis-style 'x))
    (define-values (fllength used-length neg-margin pos-margin) (plot-axis-length-values axis-style tip length))
    (define-values (tick-range origin flunit)
      (plot-axis-metrics (or tick-hint (plot-tick-engine-range ticks-engine))
                         (plot-axis-real-range real-list)
                         maybe-origin used-length maybe-unit))

    (define-values (actual-ticks maybe-stable-step minor-count) (plot-ticks-generate ticks-engine tick-range alt-format))
    (define actual-tick-values (map plot-tick-value* actual-ticks))

    (define α : Flonum (real->double-flonum rotate))
    (define e^αi : Float-Complex (make-polar 1.0 α))
    (define cosα : Flonum (cos α))
    (define vertical? : Boolean (zero? cosα))

    (define-values (axis-font digit-font label-font desc-font axis-pen flthickness digit-color tick-color label-color desc-color) (plot-axis-visual-values axis-style))
    (define-values (digit-position digit-anchor miror-anchor) (plot-axis-digit-position-values axis-style (if (not vertical?) 'x 'y)))
    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))

    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness axis-style) flthickness))
    (define fltick-length : Nonnegative-Flonum (~length (plot-axis-style-tick-length axis-style) flthickness))
    (define fltick-sublen : Nonnegative-Flonum (~length (plot-axis-style-minor-tick-length axis-style) fltick-length))
    (define fltick-min : Nonnegative-Flonum (+ neg-margin (* fltick-thickness 0.5)))
    (define fltick-max : Nonnegative-Flonum (+ fltick-min used-length))
    
    (define Vself : Float-Complex (make-polar fllength α))
    (define src-tip (geo-tip-filter (plot-axis-tip-style-negative-shape tip)))
    (define tgt-tip (geo-tip-filter (plot-axis-tip-style-positive-shape tip)))
    (define main-axis : Geo:Path:Self
      (geo-path* #:stroke axis-pen #:tip-placement 'inside
                 #:source-tip src-tip #:target-tip tgt-tip
                 (list the-M0 (gpp:point #\L Vself))))

    (define digit-offset : Float-Complex (* (make-rectangular 0.0 (* digit-position (- em))) e^αi))
    (define view-offset : Float-Complex (- (geo:path:self-origin main-axis)))
    (define flc-origin : Float-Complex (+ (make-polar (+ (* used-length origin) fltick-min) α) view-offset))
    
    (define tick-pen : Stroke (desc-stroke axis-pen #:width fltick-thickness #:color tick-color))
    (define label-at-axis? : Boolean (eq? (plot-axis-style-label-placement axis-style) 'axis))

    ;;; NOTE
    ; we don't need to care about the tip sizes here,
    ; since they only contribute to compute the position of label in other coordinate system,
    ; and for this rotated number line, we've worked in a completely different way for that purpose.
    ; Besides, taking care about the sizes might cause the last tick out of the axis.
    (define tick-bgn : Float-Complex (+ (make-polar (floor fltick-min) α)   view-offset))
    (define tick-end : Float-Complex (+ (make-polar (ceiling fltick-max) α) view-offset))
    
    (define major-tick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-path* (list the-M0 (gpp:point #\L (* (make-polar fltick-length α) 0.0+1.0i))) #:stroke tick-pen)
                 (plot-axis-tick-anchor axis-style (if (not vertical?) 'x 'y)))))

    (define minor-tick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> minor-count 0)
           (> fltick-sublen 0.0)
           (cons (geo-path* (list the-M0 (gpp:point #\L (* (make-polar fltick-sublen α) 0.0+1.0i))) #:stroke tick-pen)
                 (plot-axis-tick-anchor axis-style (if (not vertical?) 'x 'y)))))

    (define dot->pos : Plot-Position-Transform
      (case-lambda
        [(x y) (+ flc-origin (make-polar (* x flunit) α))]
        [(pt) (dot->pos (real-part pt) 0.0)]))
    
    (define layers : (Listof (GLayerof Geo))
      (append (for/fold ([labels : (Listof (GLayerof Geo)) null])
                        ([lbl (in-list (plot-axis-label-settings axis-label axis-desc 0.0+0.0i Vself))])
                (if (car lbl)
                    (let ([lbl.geo (plot-y-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color)]
                          [angle.rad (case/eq (plot-axis-style-label-placement axis-style)
                                       [(digit) (if (positive? (geo-dot-line-orientation-test digit-offset Vself))  pi/2 -pi/2)]
                                       [(mirror) (if (positive? (geo-dot-line-orientation-test digit-offset Vself)) -pi/2  pi/2)]
                                       [else (if (cadddr lbl) 0.0 (- pi))])]
                          [length (cond [(not label-at-axis?) em]
                                        [(zero? cosα) (* em 1.5)]
                                        [else (* em 0.618)])])
                      (cons (geo-path-self-pin-layer
                             (plot-marker #:rotate α
                                          (plot-label #:at (+ (cadr lbl) view-offset) #:anchor #false
                                                      #:gap-angle angle.rad #:gap-length length
                                                      lbl.geo)))
                            labels))
                    labels))
              
              (for/fold ([ticks : (Listof (GLayerof Geo)) null])
                        ([tick (in-list actual-ticks)])
                (define-values (maybe-sticker gtick)
                  (if (plot-tick-major? tick)
                      (values (tick->sticker id (plot-tick-desc tick) digit-font digit-color) major-tick)
                      (values 'minor minor-tick)))
                
                (plot-axis-sticker-cons maybe-sticker digit-anchor
                                        (dot->pos (real->double-flonum (plot-tick-value tick)) 0.0)
                                        digit-offset ticks tick-bgn tick-end gtick))
              
              (let-values ([(pin-pen real-font real-color real-anchor) (plot-mark-visual-values real-style axis-font axis-pen)]
                           [(real-pin real-gap) (plot-mark-vector-values real-style -pi/2 -pi/2)])
                (for/fold ([reals : (Listof (GLayerof Geo)) null])
                          ([mark (in-list (cond [(list? real-list) (plot-axis-list->marks real-list desc-real)]
                                                [else (plot-axis-produce-marks real-list actual-tick-values desc-real)]))])
                  (plot-axis-sticker-cons (plot-marker #:color real-color #:font real-font #:pin-stroke pin-pen
                                                       #:fallback-pin real-pin #:fallback-gap real-gap
                                                       #:fallback-anchor real-anchor #:length-base em
                                                       #:rotate rotate
                                                       mark dot->pos)
                                          reals tick-bgn tick-end)))))
  
    (define translated-layers : (Option (GLayer-Groupof Geo)) (geo-layers-try-extend (geo-own-layer main-axis) layers))
    
    (if (or translated-layers)
        (let* ([delta-origin (+ flc-origin (geo-layer-position (car (glayer-group-layers translated-layers))))]
               [dot->pos (λ [[x : Flonum]] : Float-Complex (+ delta-origin (make-polar (* x flunit) α)))])
          (create-geometry-group plot:line id #false #false
                                 #:border bdr #:background bg
                                 #:margin margin #:padding padding
                                 translated-layers delta-origin actual-tick-values
                                 (case-lambda
                                   [(x y) (dot->pos x)]
                                   [(pt) (dot->pos (real-part pt))])))
        (create-geometry-group plot:line id #false #false
                               #:border bdr #:background bg
                               #:margin margin #:padding padding
                               (geo-own-layers main-axis) flc-origin actual-tick-values
                               dot->pos))))

(define plot-integer-line
  (lambda [#:id [id : (Option Symbol) #false]
           #:length [length : Real (default-plot-axis-length)]
           #:unit-length [maybe-unit : (Option Real+%) (default-plot-axis-unit-length)]
           #:origin [maybe-origin : (Option Real) #false]
           #:style [axis-style : Plot-Axis-Style (default-plot-axis-style)]
           #:label [axis-label : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:desc [axis-desc : (U DC-Markup-Text False (Pairof (Option DC-Markup-Text) (Option DC-Markup-Text))) #false]
           #:range [tick-hint : (U Integer (Pairof Integer Integer) False) #false]
           #:ticks [ticks-engine : Plot-Tick-Engine (plot-integer-ticks)]
           #:tick-format [alt-format : (Option Plot-Tick-Format) #false]
           #:tick->sticker [tick->sticker : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:mark-style [int-style : (Option Plot-Mark-Style) #false]
           #:mark-template [desc-int : (U Plot-Mark->Description Plot:Mark) plot-desc-real]
           #:exclude-zero? [exclude-zero? : Boolean #true]
           #:border [bdr : Maybe-Stroke-Paint #false]
           #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false]
           #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           [number-sequence : (U (Listof Plot-Axis-Integer-Datum) (Vectorof Any) (-> Integer Any)) null]] : Plot:Line
    (define tip : Plot-Axis-Tip-Style (plot-axis-tip axis-style 'x))
    (define-values (fllength used-length neg-margin pos-margin) (plot-axis-length-values axis-style tip length))
    (define-values (tick-range origin flunit)
      (plot-axis-metrics (or (plot-tick-engine-range ticks-engine) tick-hint)
                         (plot-axis-integer-range number-sequence exclude-zero?)
                         maybe-origin used-length maybe-unit))
    
    (define-values (actual-ticks maybe-stable-step minor-count) (plot-ticks-generate ticks-engine tick-range alt-format))
    (define actual-tick-values (filter exact-integer? (map plot-tick-value* actual-ticks)))

    (define-values (axis-font digit-font label-font desc-font axis-pen flthickness digit-color tick-color label-color desc-color) (plot-axis-visual-values axis-style))
    (define-values (digit-position digit-anchor miror-anchor) (plot-axis-digit-position-values axis-style 'x))
    (define em : Nonnegative-Flonum (font-metrics-ref digit-font 'em))
    
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness axis-style) flthickness))
    (define fltick-length : Nonnegative-Flonum (~length (plot-axis-style-tick-length axis-style) flthickness))
    (define fltick-sublen : Nonnegative-Flonum (~length (plot-axis-style-minor-tick-length axis-style) fltick-length))
    (define fltick-min : Nonnegative-Flonum (+ neg-margin (* fltick-thickness 0.5)))
    (define fltick-max : Nonnegative-Flonum (+ fltick-min used-length))
    
    (define main-axis : Geo:Path:Self
      (geo-path* #:stroke axis-pen #:tip-placement 'inside
                 #:source-tip (plot-axis-tip-style-negative-shape tip)
                 #:target-tip (plot-axis-tip-style-positive-shape tip)
                 (list the-M0 (gpp:point #\L (make-rectangular fllength 0.0)))))

    (define view-offset : Float-Complex (- (geo:path:self-origin main-axis)))
    (define digit-offset : Float-Complex (make-rectangular 0.0 (* digit-position (- em))))
    (define miror-offset :  Float-Complex (make-rectangular 0.0 (* digit-position (+ em))))
    (define flc-origin : Float-Complex (+ (make-rectangular (+ (* used-length origin) fltick-min) 0.0) view-offset))

    (define-values (soff eoff) (geo-path-endpoint-offsets main-axis))
    (define label-at-axis? : Boolean (eq? (plot-axis-style-label-placement axis-style) 'axis))
    (define flaxis-min : Flonum (- fltick-min neg-margin (real-part soff)))
    (define flaxis-max : Flonum (+ fltick-max pos-margin (real-part eoff)))

    (define major-tick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-length #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor axis-style 'x))))
    (define minor-tick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> minor-count 0)
           (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-sublen #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor axis-style 'x))))

    (define dot->pos : Plot-Position-Transform
      (case-lambda
        [(x y) (+ flc-origin (* x flunit))]
        [(pt) (dot->pos (real-part pt) 0.0)]))

    (define layers : (Listof (GLayerof Geo))
      (append (for/fold ([labels : (Listof (GLayerof Geo)) null])
                        ([lbl (in-list (plot-axis-label-settings axis-label axis-desc flaxis-min flaxis-max (if (not label-at-axis?) 0.0 (* em 0.6))))])
                (if (car lbl)
                    (let ([lbl.geo (plot-x-axis-label (car lbl) label-font label-color (caddr lbl) desc-font desc-color (* em 0.4))])
                      (case/eq (plot-axis-style-label-placement axis-style)
                        [(digit)  (plot-axis-sticker-cons* lbl.geo digit-anchor (+ (cadr lbl) digit-offset) view-offset labels)]
                        [(mirror) (plot-axis-sticker-cons* lbl.geo miror-anchor (+ (cadr lbl) miror-offset) view-offset labels)]
                        [else (plot-axis-sticker-cons* lbl.geo (cadddr lbl) (make-rectangular (cadr lbl) 0.0) view-offset labels)]))
                    labels))
              
              (for/fold ([ticks : (Listof (GLayerof Geo)) null])
                        ([tick actual-ticks])
                (define-values (maybe-sticker gtick)
                  (if (plot-tick-major? tick)
                      (values (tick->sticker id (plot-tick-desc tick) digit-font digit-color) major-tick)
                      (values 'minor minor-tick)))
                
                (plot-axis-sticker-cons maybe-sticker digit-anchor (dot->pos (real->double-flonum (plot-tick-value tick)) 0.0)
                                        digit-offset ticks fltick-min real-part fltick-max gtick))
              
              (let-values ([(pin-pen int-font int-color int-anchor) (plot-mark-visual-values int-style axis-font axis-pen)]
                           [(int-pin int-gap) (plot-mark-vector-values int-style -pi/2 -pi/2)])
                (for/fold ([integers : (Listof (GLayerof Geo)) null])
                          ([mark (in-list (cond [(list? number-sequence) (plot-axis-list->marks number-sequence desc-int)]
                                                [(vector? number-sequence) (plot-axis-vector->marks number-sequence desc-int (if (not exclude-zero?) 0 1))]
                                                [else (plot-axis-produce-marks number-sequence actual-tick-values desc-int)]))]
                           #:when (not (and exclude-zero? (zero? (plot:mark-point mark)))))
                  (plot-axis-sticker-cons (plot-marker #:pin-stroke pin-pen #:color int-color #:font int-font
                                                       #:fallback-pin int-pin #:fallback-gap int-gap
                                                       #:fallback-anchor int-anchor #:length-base em
                                                       mark dot->pos)
                                          integers fltick-min real-part fltick-max)))))
  
    (define translated-layers : (Option (GLayer-Groupof Geo)) (geo-layers-try-extend (geo-own-layer main-axis) layers))
    
    (if (or translated-layers)
        (let* ([delta-origin (+ flc-origin (geo-layer-position (car (glayer-group-layers translated-layers))))]
               [dot->pos (λ [[x : Flonum]] : Float-Complex (+ delta-origin (* x flunit)))])
          (create-geometry-group plot:line id #false #false
                                 #:border bdr #:background bg
                                 #:margin margin #:padding padding
                                 translated-layers delta-origin actual-tick-values
                                 (case-lambda
                                   [(x y) (dot->pos x)]
                                   [(pt) (dot->pos (real-part pt))])))
        (create-geometry-group plot:line id #false #false
                               #:border bdr #:background bg
                               #:margin margin #:padding padding
                               (geo-own-layers main-axis) flc-origin actual-tick-values
                               dot->pos))))
