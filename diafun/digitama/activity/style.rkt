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

(require "../block/style.rkt")
(require "../track/style.rkt")
(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct act-track-style () #:type-name Act-Track-Style)
(struct act-block-style () #:type-name Act-Block-Style)

(struct act-action-step-style act-block-style () #:type-name Act-Action-Step-Style)
(struct act-control-node-style act-block-style () #:type-name Act-Control-Node-Style)

(define-type Act-Block-Metadata (Option Symbol))
(define-type Act-Track-Theme-Adjuster (Dia-Track-Theme-Adjuster Act-Track-Style))
(define-type Act-Block-Theme-Adjuster (Dia-Block-Theme-Adjuster Act-Block-Style Act-Block-Metadata))

(define default-act-track-theme-adjuster : (Parameterof (Option Act-Track-Theme-Adjuster)) (make-parameter #false))
(define default-act-block-theme-adjuster : (Parameterof (Option Act-Block-Theme-Adjuster)) (make-parameter #false))

(define default-act-success-decision-regexp : (Parameterof (U Byte-Regexp Regexp)) (make-parameter #px"^(?i:(t(rue)?)|(?i:y(es)?)|(?i:do))$"))
(define default-act-failure-decision-regexp : (Parameterof (U Byte-Regexp Regexp)) (make-parameter #px"^(?i:(f(alse)?)|(?i:n(o)?))$"))
(define default-act-loop-label-regexp : (Parameterof (U Byte-Regexp Regexp)) (make-parameter #px"(^(?i:(for|each|while|next)))|((?i:loop)$)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration act-block-backstop-style : Act-Block-Backstop-Style #:as dia-block-backstop-style
  #:format "default-act-block-~a"
  ([width : Nonnegative-Flonum 200.0]
   [height : Nonnegative-Flonum 50.0]
   [padding : Dia-Block-Padding (&L 0.333 'em)]
   [font : Font dia-preset-block-caption-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint dia-preset-block-stroke]
   [fill-paint : Option-Fill-Paint 'GhostWhite]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.uml-diagrams.org/activity-diagrams-controls.html
(define-phantom-struct act-initial-style : Act-Initial-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false] ; useless, fixed by its shape
   [height : Dia-Block-Option-Size (&% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'ForestGreen]))

(define-phantom-struct act-final-style : Act-Final-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size (&% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkSlateGray]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DimGray]))

(define-phantom-struct act-flow-final-style : Act-Flow-Final-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size (&% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkSlateGray]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DarkGray]))

;;; the decision node works much more like a merge node, rather than the decision node in flowchart
(define-phantom-struct act-decision-style : Act-Decision-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  +nan.0]
   [height : Dia-Block-Option-Size (&% 85)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Crimson]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct act-merge-style : Act-Merge-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  +nan.0]
   [height : Dia-Block-Option-Size (&% 85)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Maroon]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct act-fork-style : Act-Fork-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size (&% 20)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkOrange]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'MistyRose]))

(define-phantom-struct act-join-style : Act-Join-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size (&% 20)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Orange]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LemonChiffon]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.uml-diagrams.org/activity-diagrams-actions.html
(define-phantom-struct act-action-style : Act-Action-Style #:-> act-action-step-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'RoyalBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

; one time event, two typical usages
(define-phantom-struct act-time-event-style : Act-Delay-Style #:-> act-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  +nan.0]
   [height : Dia-Block-Option-Size (&% 120)]
   [padding : Dia-Block-Option-Padding (&% 2.5)]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Peru]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.uml-diagrams.org/activity-diagrams-objects.html
#;(define-phantom-struct act-reference-style : Act-Reference-Style #:-> act-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  +nan.0]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding (&% 2)]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DeepSkyBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]))

#;(define-phantom-struct act-input-style : Act-Input-Style #:-> act-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'LightSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'MintCream]))

#;(define-phantom-struct act-output-style : Act-Output-Style #:-> act-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkOrchid]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

; Manual-Operation -> Operation
#;(define-phantom-struct act-operation-style : Act-Operation-Style #:-> act-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Teal]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

; Initialization -> Preparation
#;(define-phantom-struct act-preparation-style : Act-Preparation-Style #:-> act-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Maroon]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

#;(define-phantom-struct act-storage-style : Act-Storage-Style #:-> act-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  (&% 61.8)]
   [height : Dia-Block-Option-Size (&% 161.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'MediumSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Honeydew]))

#;(define-phantom-struct act-database-style : Act-Database-Style #:-> act-storage-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false] ; useless, fixed by its shape
   [height : Dia-Block-Option-Size (&% 161.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'SeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'MintCream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Others
(define-phantom-struct act-connector-style : Act-Connector-Style #:-> act-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size (&% 61.8)]
   [padding : Dia-Block-Option-Padding (&% 4)]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DeepSkyBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration act-track-backstop-style : Act-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-act-track-~a"
  ([font : Font dia-preset-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint dia-preset-track-stroke]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip default-arrow-tip]
   [label-rotate? : Boolean #false]
   [label-inline? : Boolean #true]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct act~line~style : Act~Line~Style #:-> act-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color (void)]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~decision~style : Act~Decision~Style #:-> act-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DarkSlateGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~success~style : Act~Success~Style #:-> act~decision~style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'MediumSeaGreen]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~failure~style : Act~Failure~Style #:-> act~decision~style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DarkGoldenrod]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~loop~style : Act~Loop~Style #:-> act-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'SteelBlue]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~storage~style : Act~Storage~Style #:-> act-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint 'DodgerBlue]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DodgerBlue]
   [dash : (Option Stroke-Dash+Offset) 'dot]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))
