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
(struct flow-track-style () #:type-name Flow-Track-Style)
(struct flow-block-style () #:type-name Flow-Block-Style)

(define-type Flow-Block-Metadata (Option Symbol))
(define-type Flow-Track-Theme-Adjuster (Dia-Track-Theme-Adjuster Flow-Track-Style))
(define-type Flow-Block-Theme-Adjuster (Dia-Block-Theme-Adjuster Flow-Block-Style Flow-Block-Metadata))

(define default-flow-track-theme-adjuster : (Parameterof (Option Flow-Track-Theme-Adjuster)) (make-parameter #false))
(define default-flow-block-theme-adjuster : (Parameterof (Option Flow-Block-Theme-Adjuster)) (make-parameter #false))

(define default-flow-success-decision-regexp : (Parameterof (U Byte-Regexp Regexp)) (make-parameter #px"^(?i:(t(rue)?)|(?i:y(es)?)|(?i:do))$"))
(define default-flow-failure-decision-regexp : (Parameterof (U Byte-Regexp Regexp)) (make-parameter #px"^(?i:(f(alse)?)|(?i:n(o)?))$"))
(define default-flow-loop-label-regexp : (Parameterof (U Byte-Regexp Regexp)) (make-parameter #px"(^(?i:(for|each|while|next)))|((?i:loop)$)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration flow-block-backstop-style : Flow-Block-Backstop-Style #:as dia-block-backstop-style
  #:format "default-flow-block-~a"
  ([width : Nonnegative-Flonum 200.0]
   [height : Nonnegative-Flonum 50.0]
   [padding : Dia-Block-Padding (~L 0.333 'em)]
   [font : Font dia-preset-block-caption-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint dia-preset-block-stroke]
   [fill-paint : Option-Fill-Paint 'GhostWhite]
   [text-alignment : Geo-Text-Alignment 'center]
   [text-trim? : Boolean #true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://creately.com/guides/flowchart-symbols/
(define-phantom-struct flow-start-style : Flow-Start-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'ForestGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-stop-style : Flow-Stop-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkSlateGray]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Gainsboro]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

; On-Page-Connector -> Inspection
(define-phantom-struct flow-inspection-style : Flow-Inspection-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size (~% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DeepSkyBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

; Off-Page-Connector -> Reference
(define-phantom-struct flow-reference-style : Flow-Reference-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false] ; useless, fixed by its shape
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DeepSkyBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-input-style : Flow-Input-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'LightSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'MintCream]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-output-style : Flow-Output-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkOrchid]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-decision-style : Flow-Decision-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  (~% 85)]
   [height : Dia-Block-Option-Size (~% 85)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Crimson]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

; Predefined-Process -> Subroutine -> Prefab
(define-phantom-struct flow-process-style : Flow-Process-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'RoyalBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

; Manual-Operation -> Operation
(define-phantom-struct flow-operation-style : Flow-Operation-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Teal]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

; Initialization -> Preparation
(define-phantom-struct flow-preparation-style : Flow-Preparation-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Maroon]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-delay-style : Flow-Delay-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Peru]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

; Or -> Selection, when a decision produces exclusive multiple outcomes
(define-phantom-struct flow-selection-style : Flow-Selection-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size (~% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'IndianRed]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'SeaShell]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-junction-style : Flow-Junction-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size (~% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Orange]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LemonChiffon]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

; Normal dividing of a flow into multiple parallel ones
(define-phantom-struct flow-extract-style : Flow-Extract-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false] ; useless, since the shape should be an equilateral triangle
   [height : Dia-Block-Option-Size (~% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'DarkOrange]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'MistyRose]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-merge-style : Flow-Merge-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false] ; useless, since the shape should be an equilateral triangle
   [height : Dia-Block-Option-Size (~% 61.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'Orange]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LemonChiffon]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-collation-style : Flow-Collation-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false] ; useless, fixed by its shape
   [height : Dia-Block-Option-Size (~% 16.18)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'MediumTurquoise]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-sort-style : Flow-Sort-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false] ; useless, fixed by its shape
   [height : Dia-Block-Option-Size (~% 16.18)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'MediumTurquoise]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-storage-style : Flow-Storage-Style #:-> flow-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  (~% 61.8)]
   [height : Dia-Block-Option-Size (~% 161.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'MediumSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Honeydew]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

(define-phantom-struct flow-file-style : Flow-File-Style #:-> flow-storage-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  (~% 61.8)]
   [height : Dia-Block-Option-Size (~% 161.8)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) dia-preset-file-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'MediumSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Azure]
   [text-alignment : (Option Geo-Text-Alignment) 'left]
   [text-trim? : (U Void Boolean) #false]))

(define-phantom-struct flow-database-style : Flow-Database-Style #:-> flow-storage-style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false] ; useless, fixed by its shape
   [height : Dia-Block-Option-Size (~% 16.18)]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color 'SeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'MintCream]
   [text-alignment : (Option Geo-Text-Alignment) #false]
   [text-trim? : (U Void Boolean) (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration flow-track-backstop-style : Flow-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-flow-track-~a"
  ([font : Font dia-preset-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint dia-preset-track-stroke]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip default-arrow-tip]
   [label-rotate? : Boolean #false]
   [label-inline? : Boolean #true]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct flow~line~style : Flow~Line~Style #:-> flow-track-style #:for dia-track-style
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

(define-phantom-struct flow~decision~style : Flow~Decision~Style #:-> flow-track-style #:for dia-track-style
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

(define-phantom-struct flow~success~style : Flow~Success~Style #:-> flow~decision~style #:for dia-track-style
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

(define-phantom-struct flow~failure~style : Flow~Failure~Style #:-> flow~decision~style #:for dia-track-style
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

(define-phantom-struct flow~loop~style : Flow~Loop~Style #:-> flow-track-style #:for dia-track-style
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

(define-phantom-struct flow~storage~style : Flow~Storage~Style #:-> flow-track-style #:for dia-track-style
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
