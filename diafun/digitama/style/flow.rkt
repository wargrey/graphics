#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../node/style.rkt"))
(provide (all-from-out "../edge/style.rkt" "../edge/arrow.rkt"))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)
(require geofun/digitama/base)

(require "../node/style.rkt")
(require "../edge/style.rkt")
(require "../edge/tip.rkt")
(require "../edge/arrow.rkt")

(require "shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some standarded aliases
; Annotation -> Comment
; Predefined-Process -> Subroutine -> Prefab
; On-Page-Connector -> Inspection
; Off-Page-Connector -> Reference
; DataFile -> Database
; Manual-Operation -> Operation
; Manual-Input -> Keyboard
; Initialization -> Preparation
(define-type DiaFlow-Node-Type
  (U 'Start 'Stop 'Inspection 'Reference
     'Input 'Output 'Operation 'Keyboard
     'Preparation 'Decision 'Process 'Prefab 'Alternate
     'Database 'Document))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-arrow-style-make : (Parameterof (Option Dia-Edge-Style-Make)) (make-parameter #false))
(define default-diaflow-decision-arrow-style-make : (Parameterof (Option Dia-Edge-Style-Make)) (make-parameter #false))
(define default-diaflow-success-arrow-style-make : (Parameterof (Option Dia-Edge-Style-Make)) (make-parameter #false))
(define default-diaflow-failure-arrow-style-make : (Parameterof (Option Dia-Edge-Style-Make)) (make-parameter #false))
(define default-diaflow-loop-arrow-style-make : (Parameterof (Option Dia-Edge-Style-Make)) (make-parameter #false))
(define default-diaflow-free-track-style-make : (Parameterof (Option Dia-Free-Edge-Style-Make)) (make-parameter #false))

(define default-diaflow-success-decision-labels : (Parameterof (U (Listof String) Regexp)) (make-parameter #px"^([Tt]([Rr][Uu][Ee])?|[Yy]([Ee][Ss])?)$"))
(define default-diaflow-failure-decision-labels : (Parameterof (U (Listof String) Regexp)) (make-parameter #px"^([Ff]([Aa][Ll][Ss][Ee])?|[Nn]([Oo])?)$"))
(define default-diaflow-loop-label-regexp : (Parameterof Regexp) (make-parameter #px"[Ll][Oo][Oo][Pp]$"))

(define-configuration diaflow-edge-base-style : DiaFlow-Edge-Style #:as dia-edge-base-style
  #:format "default-diaflow-edge-~a"
  ([font : (Option Font) default-edge-label-font]
   [font-paint : Option-Fill-Paint 'DimGray]
   [line-paint : Maybe-Stroke-Paint default-edge-stroke]
   [source-shape : Option-Edge-Tip-Shape #false]
   [target-shape : Option-Edge-Tip-Shape default-arrow-tip]
   [label-rotate? : Boolean #false]
   [label-inline? : Boolean #true]))

(define-configuration diaflow-arrow-style : DiaFlow-Arrow-Style #:as dia-edge-style
  #:format "default-diaflow-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) (void)]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]))

(define-configuration diaflow-decision-arrow-style : DiaFlow-Decision-Arrow-Style #:as dia-edge-style
  #:format "default-diaflow-decision-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'Sienna]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) #false]))

(define-configuration diaflow-success-arrow-style : DiaFlow-Success-Arrow-Style #:as dia-edge-style
  #:format "default-diaflow-success-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'LimeGreen]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) #false]))

(define-configuration diaflow-failure-arrow-style : DiaFlow-Failure-Arrow-Style #:as dia-edge-style
  #:format "default-diaflow-failure-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'Orange]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) #false]))

(define-configuration diaflow-loop-arrow-style : DiaFlow-Loop-Arrow-Style #:as dia-edge-style
  #:format "default-diaflow-loop-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'Teal]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]))

(define-configuration diaflow-free-track-style : DiaFlow-Free-Track-Style #:as dia-edge-style
  #:format "default-diaflow-free-track-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash-Datum) 'long-dash]
   [source-shape : Maybe-Edge-Tip-Shape #false]
   [target-shape : Maybe-Edge-Tip-Shape #false]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (DiaFlow-Node-Style-Make S) (Dia-Node-Style-Make* S (Option Symbol)))

(define default-diaflow-canonical-start-name : (Parameterof String) (make-parameter ""))
(define default-diaflow-canonical-stop-name : (Parameterof String) (make-parameter ""))

(define default-diaflow-node-label-string : (Parameterof (Option Dia-Node-Id->String)) (make-parameter #false))

(define default-diaflow-input-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Input-Style))) (make-parameter #false))
(define default-diaflow-output-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Output-Style))) (make-parameter #false))
(define default-diaflow-start-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Start-Style))) (make-parameter #false))
(define default-diaflow-stop-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Stop-Style))) (make-parameter #false))
(define default-diaflow-decision-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Decision-Style))) (make-parameter #false))
(define default-diaflow-process-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Process-Style))) (make-parameter #false))
(define default-diaflow-prefab-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Prefab-Style))) (make-parameter #false))
(define default-diaflow-alternate-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Alternate-Style))) (make-parameter #false))
(define default-diaflow-inspection-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Inspection-Style))) (make-parameter #false))
(define default-diaflow-reference-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Reference-Style))) (make-parameter #false))
(define default-diaflow-database-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Database-Style))) (make-parameter #false))
(define default-diaflow-document-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Document-Style))) (make-parameter #false))
(define default-diaflow-operation-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Operation-Style))) (make-parameter #false))
(define default-diaflow-preparation-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Preparation-Style))) (make-parameter #false))

(define-configuration diaflow-node-base-style : DiaFlow-Node-Style #:as dia-node-base-style
  #:format "default-diaflow-~a"
  ([block-width : Nonnegative-Flonum 200.0]
   [block-height : Nonnegative-Flonum 50.0]
   [font : (Option Font) default-node-label-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint default-node-stroke]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diaflow-start-style : DiaFlow-Start-Style #:as dia-node-style
  #:format "default-diaflow-start-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'ForestGreen]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]))

(define-configuration diaflow-stop-style : DiaFlow-Stop-Style #:as dia-node-style
  #:format "default-diaflow-stop-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DarkSlateGray]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'Gainsboro]))

(define-configuration diaflow-inspection-style : DiaFlow-Inspection-Style #:as dia-node-style
  #:format "default-diaflow-inspection-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* (default-diaflow-block-width)  0.384)]
   [block-height : (Option Nonnegative-Flonum) (* (default-diaflow-block-height) 0.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DeepSkyBlue]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]))

(define-configuration diaflow-reference-style : DiaFlow-Reference-Style #:as dia-node-style
  #:format "default-diaflow-reference-~a"
  ([block-width : (Option Nonnegative-Flonum) (* (default-diaflow-block-width) 0.24)]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'MediumSeaGreen]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'Honeydew]))

(define-configuration diaflow-input-style : DiaFlow-Input-Style #:as dia-node-style
  #:format "default-diaflow-input-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Chocolate]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-output-style : DiaFlow-Output-Style #:as dia-node-style
  #:format "default-diaflow-output-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Purple]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-decision-style : DiaFlow-Decision-Style #:as dia-node-style
  #:format "default-diaflow-decision-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* (default-diaflow-block-width)  0.85)]
   [block-height : (Option Nonnegative-Flonum) (* (default-diaflow-block-height) 0.85)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Crimson]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-process-style : DiaFlow-Process-Style #:as dia-node-style
  #:format "default-diaflow-process-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'RoyalBlue]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-prefab-style : DiaFlow-Prefab-Style #:as dia-node-style
  #:format "default-diaflow-prefab-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DodgerBlue]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-alternate-style : DiaFlow-Alternate-Style #:as dia-node-style
  #:format "default-diaflow-alternate-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'CornflowerBlue]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-database-style : DiaFlow-Database-Style #:as dia-node-style
  #:format "default-diaflow-database-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-document-style : DiaFlow-Document-Style #:as dia-node-style
  #:format "default-diaflow-document-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-operation-style : DiaFlow-Operation-Style #:as dia-node-style
  #:format "default-diaflow-operation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Burlywood]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-keyboard-style : DiaFlow-Keyboard-Style #:as dia-node-style
  #:format "default-diaflow-keyboard-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-preparation-style : DiaFlow-Preparation-Style #:as dia-node-style
  #:format "default-diaflow-preparation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'Maroon]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))
