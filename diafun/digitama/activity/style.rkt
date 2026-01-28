#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../block/style.rkt"))
(provide (all-from-out "../track/style.rkt"))

(require digimon/struct)
(require digimon/measure)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/path/tip/self)
(require geofun/digitama/path/tip/arrow)

(require "../block/style.rkt")
(require "../track/style.rkt")
(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct act-track-style () #:type-name Act-Track-Style)
(struct act-block-style () #:type-name Act-Block-Style)

(struct act-executable-node-style act-block-style () #:type-name Act-Executable-Node-Style)
(struct act-control-node-style act-block-style () #:type-name Act-Control-Node-Style)
(struct act-object-node-style act-block-style () #:type-name Act-Object-Node-Style)

(define-type Act-Block-Metadata (Option Symbol))
(define-type Act-Track-Theme-Adjuster (Dia-Track-Theme-Adjuster Act-Track-Style))
(define-type Act-Block-Theme-Adjuster (Dia-Block-Theme-Adjuster Act-Block-Style Act-Block-Metadata))

(define default-act-track-theme-adjuster : (Parameterof (Option Act-Track-Theme-Adjuster)) (make-parameter #false))
(define default-act-block-theme-adjuster : (Parameterof (Option Act-Block-Theme-Adjuster)) (make-parameter #false))

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
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'ForestGreen]))

(define-phantom-struct act-final-style : Act-Final-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size (&% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkSlateGray]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DimGray]))

(define-phantom-struct act-flow-final-style : Act-Flow-Final-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size (&% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkSlateGray]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DarkGray]))

;;; the decision node works much more like a merge node, rather than the decision node in flowchart
(define-phantom-struct act-decision-style : Act-Decision-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  +nan.0]
   [height : Dia-Block-Option-Size (&% 75)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Crimson]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct act-merge-style : Act-Merge-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  +nan.0]
   [height : Dia-Block-Option-Size (&% 75)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Maroon]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct act-fork-style : Act-Fork-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  (&% 200)]
   [height : Dia-Block-Option-Size (&% 20)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Sienna]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Sienna]))

(define-phantom-struct act-join-style : Act-Join-Style #:-> act-control-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  (&% 200)]
   [height : Dia-Block-Option-Size (&% 20)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Chocolate]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Chocolate]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.uml-diagrams.org/activity-diagrams-actions.html
(define-phantom-struct act-action-style : Act-Action-Style #:-> act-executable-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'RoyalBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct act-signal-style : Act-Reference-Style #:-> act-executable-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  +nan.0]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding (&% 2)]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DeepSkyBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]))

; one time event, two typical usages
(define-phantom-struct act-time-event-style : Act-Delay-Style #:-> act-executable-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  +nan.0]
   [height : Dia-Block-Option-Size (&% 120)]
   [padding : Dia-Block-Option-Padding (&% 2.5)]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Peru]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.uml-diagrams.org/activity-diagrams-objects.html
(define-phantom-struct act-object-style : Act-Object-Style #:-> act-object-node-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  (&% 80)]
   [height : Dia-Block-Option-Size (&% 80)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'MediumSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Honeydew]))

(define-phantom-struct act-central-buffer-style : Act-Central-Buffer-Style #:-> act-object-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  (&% 61.8)]
   [height : Dia-Block-Option-Size (&% 200)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
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
   [font : (Option Font+Tweak) #false]
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
   [label-rotate? : Boolean #true]
   [label-inline? : Boolean #false]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct act~control~flow~style : Act~Control~Flow~Style #:-> act-track-style #:for dia-track-style
  ([font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color (void)]
   [dash : (Option Stroke-Dash+Offset) 'long-dash]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~object~flow~style : Act~Object~Flow~Style #:-> act-track-style #:for dia-track-style
  ([font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color (void)]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~decision~style : Act~Decision~Style #:-> act~control~flow~style #:for dia-track-style
  ([font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DarkSlateGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~parallel~style : Act~Parallel~Style #:-> act~control~flow~style #:for dia-track-style
  ([font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DarkSlateBlue]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct act~storage~style : Act~Storage~Style #:-> act~object~flow~style #:for dia-track-style
  ([font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint 'DodgerBlue]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DodgerBlue]
   [dash : (Option Stroke-Dash+Offset) 'dot]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))
