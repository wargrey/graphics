#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../block/style.rkt"))
(provide (all-from-out "../track/style.rkt"))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/path/tip/self)
(require geofun/digitama/path/tip/arrow)

(require "../block/style.rkt")
(require "../track/style.rkt")
(require "../shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://creately.com/guides/flowchart-symbols/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaFlow-Arrow-Style))) (make-parameter #false))
(define default-diaflow-decision-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaFlow-Decision-Arrow-Style))) (make-parameter #false))
(define default-diaflow-success-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaFlow-Success-Arrow-Style))) (make-parameter #false))
(define default-diaflow-failure-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaFlow-Failure-Arrow-Style))) (make-parameter #false))
(define default-diaflow-loop-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaFlow-Loop-Arrow-Style))) (make-parameter #false))
(define default-diaflow-storage-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaFlow-Storage-Arrow-Style))) (make-parameter #false))
(define default-diaflow-free-track-style-make : (Parameterof (Option (Dia-Free-Track-Style-Make DiaFlow-Free-Track-Style))) (make-parameter #false))

(define default-diaflow-success-decision-regexp : (Parameterof (U Byte-Regexp Regexp)) (make-parameter #px"^([Tt]([Rr][Uu][Ee])?|[Yy]([Ee][Ss])?|[Dd][Oo])$"))
(define default-diaflow-failure-decision-regexp : (Parameterof (U Byte-Regexp Regexp)) (make-parameter #px"^([Ff]([Aa][Ll][Ss][Ee])?|[Nn]([Oo])?)$"))
(define default-diaflow-loop-label-regexp : (Parameterof (U Byte-Regexp Regexp))
  (make-parameter #px"(^[Ff][Oo][Rr])|(^[Ee][Aa][Cc][Hh])|(^[Ww][Hh][Ii][Ll][Ee])|(^[Nn][Ee][Xx][Tt])|([Ll][Oo][Oo][Pp]$)"))

(define-configuration diaflow-track-backstop-style : DiaFlow-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-diaflow-track-~a"
  ([font : Font default-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint default-track-stroke]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip default-arrow-tip]
   [label-rotate? : Boolean #false]
   [label-inline? : Boolean #true]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diaflow-arrow-style : DiaFlow-Arrow-Style #:as dia-track-style
  #:format "default-diaflow-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) (void)]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Flonum) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diaflow-decision-arrow-style : DiaFlow-Decision-Arrow-Style #:as dia-track-style
  #:format "default-diaflow-decision-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DarkSlateGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Flonum) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diaflow-success-arrow-style : DiaFlow-Success-Arrow-Style #:as dia-track-style
  #:format "default-diaflow-success-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumSeaGreen]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Flonum) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diaflow-failure-arrow-style : DiaFlow-Failure-Arrow-Style #:as dia-track-style
  #:format "default-diaflow-failure-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DarkGoldenrod]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Flonum) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diaflow-loop-arrow-style : DiaFlow-Loop-Arrow-Style #:as dia-track-style
  #:format "default-diaflow-loop-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'SteelBlue]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Flonum) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diaflow-storage-arrow-style : DiaFlow-Storage-Arrow-Style #:as dia-track-style
  #:format "default-diaflow-storage-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DodgerBlue]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DodgerBlue]
   [dash : (Option Stroke-Dash+Offset) 'dot]
   [opacity : (Option Flonum) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diaflow-free-track-style : DiaFlow-Free-Track-Style #:as dia-track-style
  #:format "default-diaflow-free-track-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash+Offset) 'dot-dash]
   [opacity : (Option Flonum) #false]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip #false]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #true]
   [label-distance : (U Void Flonum) (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-canonical-start-name : (Parameterof String) (make-parameter ""))

(define default-diaflow-start-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Start-Style))) (make-parameter #false))
(define default-diaflow-stop-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Stop-Style))) (make-parameter #false))
(define default-diaflow-decision-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Decision-Style))) (make-parameter #false))
(define default-diaflow-collation-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Collation-Style))) (make-parameter #false))
(define default-diaflow-sort-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Sort-Style))) (make-parameter #false))
(define default-diaflow-operation-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Operation-Style))) (make-parameter #false))
(define default-diaflow-preparation-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Preparation-Style))) (make-parameter #false))
(define default-diaflow-delay-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Delay-Style))) (make-parameter #false))

(define default-diaflow-process-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Process-Style))) (make-parameter #false))
(define default-diaflow-prefab-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Process-Style))) (make-parameter #false))
(define default-diaflow-alternate-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Process-Style))) (make-parameter #false))

(define default-diaflow-inspection-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Inspection-Style))) (make-parameter #false))
(define default-diaflow-reference-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Reference-Style))) (make-parameter #false))
(define default-diaflow-selection-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Selection-Style))) (make-parameter #false))
(define default-diaflow-junction-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Junction-Style))) (make-parameter #false))
(define default-diaflow-extract-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Extract-Style))) (make-parameter #false))
(define default-diaflow-merge-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Merge-Style))) (make-parameter #false))

(define default-diaflow-keyboard-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Input-Style))) (make-parameter #false))
(define default-diaflow-display-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Output-Style))) (make-parameter #false))
(define default-diaflow-input-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Input-Style))) (make-parameter #false))
(define default-diaflow-output-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Output-Style))) (make-parameter #false))

(define default-diaflow-storage-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Storage-Style))) (make-parameter #false))
(define default-diaflow-memory-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Storage-Style))) (make-parameter #false))
(define default-diaflow-document-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Storage-Style))) (make-parameter #false))
(define default-diaflow-database-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaFlow-Database-Style))) (make-parameter #false))

(define-configuration diaflow-block-backstop-style : DiaFlow-Block-Backstop-Style #:as dia-block-backstop-style
  #:format "default-diaflow-~a"
  ([block-width : Nonnegative-Flonum 200.0]
   [block-height : Nonnegative-Flonum 50.0]
   [font : Font default-block-brief-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint default-block-stroke]
   [fill-paint : Option-Fill-Paint 'GhostWhite]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diaflow-start-style : DiaFlow-Start-Style #:as dia-block-style
  #:format "default-diaflow-start-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'ForestGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-stop-style : DiaFlow-Stop-Style #:as dia-block-style
  #:format "default-diaflow-stop-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DarkSlateGray]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Gainsboro]
   [opacity : (Option Flonum) #false]))

; On-Page-Connector -> Inspection
(define-configuration diaflow-inspection-style : DiaFlow-Inspection-Style #:as dia-block-style
  #:format "default-diaflow-inspection-~a"
  ([block-width : (Option Nonnegative-Flonum)  #false]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height))  0.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DeepSkyBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]
   [opacity : (Option Flonum) #false]))

; Off-Page-Connector -> Reference
(define-configuration diaflow-reference-style : DiaFlow-Reference-Style #:as dia-block-style
  #:format "default-diaflow-reference-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* ((default-diaflow-block-height))  0.618 1.618)]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DeepSkyBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-input-style : DiaFlow-Input-Style #:as dia-block-style
  #:format "default-diaflow-input-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'LightSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'MintCream]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-output-style : DiaFlow-Output-Style #:as dia-block-style
  #:format "default-diaflow-output-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DarkOrchid]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-decision-style : DiaFlow-Decision-Style #:as dia-block-style
  #:format "default-diaflow-decision-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* ((default-diaflow-block-width))  0.85)]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 0.85)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Crimson]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Flonum) #false]))

; Predefined-Process -> Subroutine -> Prefab
(define-configuration diaflow-process-style : DiaFlow-Process-Style #:as dia-block-style
  #:format "default-diaflow-process-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'RoyalBlue]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Flonum) #false]))

; Manual-Operation -> Operation
(define-configuration diaflow-operation-style : DiaFlow-Operation-Style #:as dia-block-style
  #:format "default-diaflow-operation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Teal]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Flonum) #false]))

; Initialization -> Preparation
(define-configuration diaflow-preparation-style : DiaFlow-Preparation-Style #:as dia-block-style
  #:format "default-diaflow-preparation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Maroon]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-delay-style : DiaFlow-Delay-Style #:as dia-block-style
  #:format "default-diaflow-delay-~a"
  ([block-width : (Option Nonnegative-Flonum)  #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Peru]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Flonum) #false]))

; Or -> Selection, when a decision produces exclusive multiple outcomes
(define-configuration diaflow-selection-style : DiaFlow-Selection-Style #:as dia-block-style
  #:format "default-diaflow-selection-~a"
  ([block-width : (Option Nonnegative-Flonum)  #false]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 0.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'IndianRed]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'SeaShell]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-junction-style : DiaFlow-Junction-Style #:as dia-block-style
  #:format "default-diaflow-junction-~a"
  ([block-width : (Option Nonnegative-Flonum)  #false]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 0.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Orange]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LemonChiffon]
   [opacity : (Option Flonum) #false]))

; Normal dividing of a flow into multiple parallel ones
(define-configuration diaflow-extract-style : DiaFlow-Extract-Style #:as dia-block-style
  #:format "default-diaflow-extract-~a"
  ([block-width : (Option Nonnegative-Flonum)  (max (/ ((default-diaflow-block-height)) (sqrt 3.0) 0.5 1.618) 0.0)]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 0.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DarkOrange]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'MistyRose]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-merge-style : DiaFlow-Merge-Style #:as dia-block-style
  #:format "default-diaflow-merge-~a"
  ([block-width : (Option Nonnegative-Flonum)  (max (/ ((default-diaflow-block-height)) (sqrt 3.0) 0.5 1.618) 0.0)]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 0.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Orange]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LemonChiffon]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-collation-style : DiaFlow-Collation-Style #:as dia-block-style
  #:format "default-diaflow-collation-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* ((default-diaflow-block-height)) 1.618 1.618)]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 1.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'MediumTurquoise]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-sort-style : DiaFlow-Sort-Style #:as dia-block-style
  #:format "default-diaflow-sort-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* ((default-diaflow-block-height)) 1.618 1.618)]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 1.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'MediumTurquoise]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-storage-style : DiaFlow-Storage-Style #:as dia-block-style
  #:format "default-diaflow-storage-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* ((default-diaflow-block-width)) 0.618)]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 1.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'MediumSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Honeydew]
   [opacity : (Option Flonum) #false]))

(define-configuration diaflow-database-style : DiaFlow-Database-Style #:as dia-block-style
  #:format "default-diaflow-database-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* ((default-diaflow-block-height)) 1.618 1.618)]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diaflow-block-height)) 1.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'MediumSeaGreen]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Honeydew]
   [opacity : (Option Flonum) #false]))
