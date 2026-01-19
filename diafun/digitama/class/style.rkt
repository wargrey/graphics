#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../block/style.rkt"))
(provide (all-from-out "../track/style.rkt"))

(require digimon/struct)
(require digimon/measure)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/richtext/self)
(require geofun/digitama/path/tip/self)
(require geofun/digitama/path/tip/arrow)
(require geofun/digitama/path/tip/diamond)

(require "../block/style.rkt")
(require "../track/style.rkt")
(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct cls-block-style () #:type-name Cls-Block-Style)
(struct cls-track-style () #:type-name Cls-Track-Style)

(define-type Cls-Block-Metadata (Option Symbol))
(define-type Cls-Block-Theme-Adjuster (Dia-Block-Theme-Adjuster Cls-Block-Style Cls-Block-Metadata))
(define-type Cls-Track-Theme-Adjuster (Dia-Track-Theme-Adjuster Cls-Track-Style))

(define default-cls-block-theme-adjuster : (Parameterof (Option Cls-Block-Theme-Adjuster)) (make-parameter #false))
(define default-cls-track-theme-adjuster : (Parameterof (Option Cls-Track-Theme-Adjuster)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration cls-block-backstop-style : Cls-Block-Backstop-Style #:as dia-block-backstop-style
  #:format "default-cls-block-~a"
  ([width : Nonnegative-Flonum 150.0]
   [height : Nonnegative-Flonum 45.0]
   [padding : Dia-Block-Padding (&L 0.333 'em)]
   [font : Font dia-preset-header-font]
   [font-paint : Fill-Paint 'White]
   [stroke-paint : Option-Stroke-Paint #false]
   [fill-paint : Option-Fill-Paint 'CadetBlue]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTICE
; the class diagram only has one block type, and it is the class;
; the detialed type of a class is marked with its property of the stereotype.
(define-phantom-struct cls-class-style : Cls-Class-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct cls-interface-style : Cls-Interface-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Goldenrod]))

(define-phantom-struct cls-abstract-style : Cls-Abstract-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) (make-font:tweak #:style 'italic)]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DarkGoldenrod]))

(define-phantom-struct cls-enumeration-style : Cls-Enumeration-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DarkSalmon]))

(define-phantom-struct cls-type-style : Cls-Type-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DodgerBlue]))

(define-phantom-struct cls-lambda-style : Cls-Lambda-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'RoyalBlue]))

(define-phantom-struct cls-datum-style : Cls-Datum-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Aquamarine]))

(define-phantom-struct cls-structure-style : Cls-Structure-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DarkGoldenrod]))

(define-phantom-struct cls-behavior-style : Cls-Behavior-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DarkGoldenrod]))

(define-phantom-struct cls-unrecognized-style : Cls-Unrecognized-Style #:-> cls-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration cls-track-backstop-style : Cls-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-cls-track-~a"
  ([font : Font dia-preset-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint dia-preset-track-stroke]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip default-arrow-tip]
   [label-rotate? : Boolean #false]
   [label-inline? : Boolean #false]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct cls~generalization~style : Cls~Generalization~Style #:-> cls-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'MediumPurple]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip default-generalization-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct cls~realization~style : Cls~Realization~Style #:-> cls-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'MediumPurple]
   [dash : (Option Stroke-Dash+Offset) 'short-dash]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip default-generalization-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct cls~composition~style : Cls~Composition~Style #:-> cls-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'MediumAquamarine]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip default-composition-tip]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct cls~aggregation~style : Cls~Aggregation~Style #:-> cls-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'Turquoise]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip default-aggregation-tip]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct cls~association~style : Cls~Association~Style #:-> cls-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DimGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct cls~bidirection~style : Cls~Bidirection~Style #:-> cls-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DimGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip #false]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct cls~dependency~style : Cls~Dependency~Style #:-> cls-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DimGray]
   [dash : (Option Stroke-Dash+Offset) 'short-dash]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))
