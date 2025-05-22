#lang typed/racket/base

(provide (all-defined-out))
(provide Plot:Axis plot:axis?)
(provide plot-axis-real-values plot-axis-integer-values)
(provide default-plot-axis-integer-filter default-plot-axis-real-filter)
(provide (all-from-out "digitama/axis/style.rkt"))
(provide (all-from-out "digitama/axis/interface.rkt"))
(provide (all-from-out "digitama/axis/tick/self.rkt"))
(provide (all-from-out "digitama/axis/tick/real.rkt"))

(require digimon/metrics)

(require geofun/font)
(require geofun/constructor)

(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)
(require geofun/digitama/layer/sticker)

(require "digitama/axis/self.rkt")
(require "digitama/axis/real.rkt")
(require "digitama/axis/sticker.rkt")
(require "digitama/axis/interface.rkt")
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
           #:style [axis-style : Plot-Axis-Style (make-plot-axis-style #:tick-anchor 'cb #:tick-length -3.0)]
           #:label [axis-label : (Option String) #false]
           #:range [tick-hint : (U Real (Pairof Real Real) (Listof Real) False) #false]
           #:ticks [ticks-generate : (Plot-Ticks-Generate Real) (plot-real-ticks)]
           #:tick->sticker [digit->label : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:real-style [real-style : (Option Plot-Axis-Real-Style) #false]
           #:real-filter [real-filter : (Option Plot-Axis-Real-Filter) #false]
           #:real->sticker [real->label : Plot-Axis-Real->Sticker default-plot-axis-real->sticker]
           #:real->dot [real->dot : Plot-Axis-Real->Dot default-plot-axis-real->dot]
           [real-list : (U (Listof Plot-Axis-Real-Datum) (-> Real Any)) null]] : Plot:Axis
    (define-values (fllength used-length neg-margin pos-margin) (plot-axis-length-values axis-style length))
    (define-values (tick-range origin flunit)
      (plot-axis-metrics tick-hint (plot-axis-real-range real-list)
                         maybe-origin used-length maybe-unit))

    (define actual-ticks : (Plot-Ticks Real) (ticks-select tick-hint tick-range ticks-generate))
    (define actual-tick-values (map (inst plot-tick-value* Real) actual-ticks))
    
    (define flthickness : Nonnegative-Flonum (plot-axis-style-thickness axis-style))
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness axis-style) flthickness))
    (define fltick-length : Nonnegative-Flonum (~length (plot-axis-tick-length axis-style 'x) flthickness))
    (define fltick-min : Nonnegative-Flonum (+ neg-margin (* fltick-thickness 0.5)))
    (define fltick-max : Nonnegative-Flonum (+ fltick-min used-length))
    (define flaxis-max : Nonnegative-Flonum (+ fltick-max pos-margin))

    (define-values (digit-font label-font axis-color digit-color tick-color label-color) (plot-axis-visual-values axis-style))
    (define-values (digit-position digit-anchor) (plot-axis-digit-position-values axis-style 'x))
    
    (define arrow : Geo (geo-arrow #:shaft-thickness flthickness #:stroke #false #:fill axis-color
                                   (~length (plot-axis-style-arrow-radius axis-style) flthickness)
                                   (+ fllength fltick-thickness)))
    
    (define gtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-length #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor axis-style 'x))))

    (define -em : Flonum (- (font-metrics-ref digit-font 'em)))
    (define arrow-half : Nonnegative-Flonum (* (geo-height arrow) 0.5))
    (define flc-origin : Float-Complex (make-rectangular (+ (* used-length origin) fltick-min) arrow-half))
    (define flc-offset : Float-Complex (make-rectangular 0.0 (* digit-position -em)))
    
    (define dot->pos : Plot-Axis-Position-Map
      (lambda [x]
        (+ flc-origin (* x flunit))))

    (define layers : (Listof (GLayerof Geo))
      (append (cond [(not axis-label) null]
                    [else (let ([g (geo-text axis-label label-font #:color label-color)])
                            (plot-axis-sticker-cons g digit-anchor (make-rectangular flaxis-max arrow-half)
                                                    flc-offset null fltick-min real-part flaxis-max #false))])
              
              (for/fold ([ticks : (Listof (GLayerof Geo)) null])
                        ([tick (in-list actual-ticks)])
                (plot-axis-sticker-cons (digit->label id (cdr tick) digit-font digit-color) digit-anchor
                                        (dot->pos (real->double-flonum (plot-tick-value (car tick))))
                                        flc-offset ticks fltick-min real-part fltick-max gtick))
              
              (let-values ([(real-font real-color real-position real-anchor dot-radius) (plot-axis-real-style-values real-style axis-style)])
                (for/fold ([reals : (Listof (GLayerof Geo)) null])
                          ([real (in-list (cond [(list? real-list) real-list]
                                                [(list? tick-hint) (plot-axis-reals-from-producer real-list tick-hint)]
                                                [(or tick-range) (plot-axis-reals-from-producer real-list actual-tick-values)]
                                                [else null]))])
                  (define-values (val obj) ((or real-filter (default-plot-axis-real-filter)) real))
                  
                  (if (or val)
                      (let*-values ([(real-offset) (make-rectangular 0.0 (* real-position -em))]
                                    [(label dot) (plot-axis-real-label-values id val obj real-font real-color dot-radius
                                                                              real->label real->dot flunit real-anchor)])
                        (plot-axis-sticker-cons (car label) (cdr label) (dot->pos val)
                                                real-offset reals fltick-min real-part fltick-max dot))
                      reals)))))
  
    (define translated-layers : (Option (GLayer-Groupof Geo)) (geo-path-try-extend/list (geo-own-layer arrow) layers))
    
    (if (or translated-layers)
        (let ([delta-origin (+ flc-origin (geo-layer-position (car (glayer-group-layers translated-layers))))])
          (create-geometry-group plot:axis id #false #false translated-layers delta-origin actual-tick-values
                                 (λ [[x : Flonum]] (+ delta-origin (* x flunit)))))
        (create-geometry-group plot:axis id #false #false (geo-own-layers arrow) flc-origin actual-tick-values dot->pos))))

(define plot-integer-axis
  (lambda [#:id [id : (Option Symbol) #false]
           #:length [length : Real (default-plot-axis-length)]
           #:unit-length [maybe-unit : (Option Real) (default-plot-axis-unit-length)]
           #:origin [maybe-origin : (Option Real) #false]
           #:style [axis-style : Plot-Axis-Style (make-plot-axis-style #:tick-anchor 'cb #:tick-length -3.0)]
           #:label [axis-label : (Option String) #false]
           #:range [tick-hint : (U Integer (Pairof Integer Integer) (Listof Integer) False) #false]
           #:ticks [ticks-generate : (Plot-Ticks-Generate Integer) (plot-integer-ticks)]
           #:tick->sticker [digit->label : Plot-Axis-Tick->Sticker default-plot-axis-tick->sticker]
           #:integer-style [int-style : (Option Plot-Axis-Real-Style) #false]
           #:integer-filter [int-filter : (Option Plot-Axis-Integer-Filter) #false]
           #:integer->sticker [int->label : Plot-Axis-Real->Sticker default-plot-axis-real->sticker]
           #:integer->dot [int->dot : Plot-Axis-Real->Dot default-plot-axis-real->dot]
           #:exclude-zero? [exclude-zero? : Boolean #true]
           [number-sequence : (U (Listof Plot-Axis-Integer-Datum) (Vectorof Any) (-> Integer Any)) null]] : Plot:Axis
    (define-values (fllength used-length neg-margin pos-margin) (plot-axis-length-values axis-style length))
    (define-values (ticks origin flunit)
      (plot-axis-metrics tick-hint (plot-axis-integer-range number-sequence exclude-zero?)
                         maybe-origin used-length maybe-unit))
    
    (define actual-ticks : (Plot-Ticks Integer) (ticks-select tick-hint ticks ticks-generate))
    (define actual-tick-values (map (inst plot-tick-value* Integer) actual-ticks))
    
    (define flthickness : Nonnegative-Flonum (plot-axis-style-thickness axis-style))
    (define fltick-thickness : Nonnegative-Flonum (~length (plot-axis-style-tick-thickness axis-style) flthickness))
    (define fltick-length : Nonnegative-Flonum (~length (plot-axis-tick-length axis-style 'x) flthickness))
    (define fltick-min : Nonnegative-Flonum (+ neg-margin (* fltick-thickness 0.5)))
    (define fltick-max : Nonnegative-Flonum (+ fltick-min used-length))
    (define flaxis-max : Nonnegative-Flonum (+ fltick-max pos-margin))

    (define-values (digit-font label-font axis-color digit-color tick-color label-color) (plot-axis-visual-values axis-style))
    (define-values (digit-position digit-anchor) (plot-axis-digit-position-values axis-style 'x))
    
    (define arrow : Geo (geo-arrow #:shaft-thickness flthickness #:stroke #false #:fill axis-color
                                   (~length (plot-axis-style-arrow-radius axis-style) flthickness)
                                   (+ fllength fltick-thickness)))
    
    (define gtick : (Option (Pairof Geo Geo-Pin-Anchor))
      (and (> fltick-length 0.0)
           (cons (geo-rectangle fltick-thickness fltick-length #:stroke #false #:fill tick-color)
                 (plot-axis-tick-anchor axis-style 'x))))

    (define -em : Flonum (- (font-metrics-ref digit-font 'em)))
    (define arrow-half : Nonnegative-Flonum (* (geo-height arrow) 0.5))
    (define flc-origin : Float-Complex (make-rectangular (+ (* used-length origin) fltick-min) arrow-half))
    (define flc-offset : Float-Complex (make-rectangular 0.0 (* digit-position -em)))
    
    (define dot->pos : Plot-Axis-Position-Map
      (lambda [x]
        (+ flc-origin (* x flunit))))

    (define layers : (Listof (GLayerof Geo))
      (parameterize ([default-plot-axis-integer-filter ((if (not exclude-zero?) values plot-axis-nonzero-values-wrap) (or int-filter (default-plot-axis-integer-filter)))])
        (append (cond [(not axis-label) null]
                      [else (let ([g (geo-text axis-label label-font #:color label-color)])
                              (plot-axis-sticker-cons g digit-anchor (make-rectangular flaxis-max arrow-half)
                                                      flc-offset null fltick-min real-part flaxis-max #false))])

                (for/fold ([ticks : (Listof (GLayerof Geo)) null])
                          ([tick actual-ticks])
                  (plot-axis-sticker-cons (digit->label id (cdr tick) digit-font digit-color) digit-anchor
                                          (dot->pos (real->double-flonum (plot-tick-value* tick)))
                                          flc-offset ticks fltick-min real-part fltick-max gtick))

                (let-values ([(int-font int-color int-position int-anchor dot-radius) (plot-axis-real-style-values int-style axis-style)])
                  (for/fold ([reals : (Listof (GLayerof Geo)) null])
                            ([real (in-list (cond [(list? number-sequence) number-sequence]
                                                  [(vector? number-sequence) (plot-axis-reals-from-vector number-sequence (if (not exclude-zero?) 0 1))]
                                                  [(list? tick-hint) (plot-axis-reals-from-producer number-sequence tick-hint)]
                                                  [(or ticks) (plot-axis-reals-from-producer number-sequence actual-tick-values)]
                                                  [else null]))])
                    (define-values (val obj) ((default-plot-axis-integer-filter) real))
                    
                    (if (or val)
                        (let*-values ([(ival) (exact->inexact val)]
                                      [(real-offset) (make-rectangular 0.0 (* int-position -em))]
                                      [(label dot) (plot-axis-real-label-values id ival obj int-font int-color dot-radius
                                                                                int->label int->dot flunit int-anchor)])
                          (plot-axis-sticker-cons (car label) (cdr label) (dot->pos ival)
                                                  real-offset reals fltick-min real-part fltick-max dot))
                        reals))))))

    (define translated-layers : (Option (GLayer-Groupof Geo)) (geo-path-try-extend/list (geo-own-layer arrow) layers))

    (if (or translated-layers)
        (let ([delta-origin (+ flc-origin (geo-layer-position (car (glayer-group-layers translated-layers))))])
          (create-geometry-group plot:axis id #false #false translated-layers delta-origin actual-tick-values
                                 (λ [[x : Flonum]] (+ delta-origin (* x flunit)))))
        (create-geometry-group plot:axis id #false #false (geo-own-layers arrow) flc-origin actual-tick-values dot->pos))))
  