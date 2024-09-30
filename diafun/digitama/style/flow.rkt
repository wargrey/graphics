#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../node/style.rkt"))
(provide (all-from-out "../edge/style.rkt" "../edge/arrow.rkt"))

(require digimon/struct)

(require geofun/font)
(require geofun/stroke)
(require geofun/paint)

(require "../node/style.rkt")
(require "../edge/style.rkt")
(require "../edge/tip.rkt")
(require "../edge/arrow.rkt")

(require "shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some standarded aliases
; Annotation -> Comment
; Predefined-Process -> Subroutine
; On-Page-Connector -> Inspection
; Off-Page-Connector -> Reference
; DataFile -> Database
; Manual-Operation -> Operation
; Manual-Input -> Keyboard
; Initialization -> Preparation
(define-type DiaFlow-Node-Type
  (U 'Input 'Output 'Start 'Stop 'Process 'Decision
     'Comment 'Subroutine 'Inspection 'Reference
     'Database 'Document 'Operation 'Keyboard 'Preparation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diaflow-arrow-style-make : (Parameterof (Option Dia-Edge-Style-Make)) (make-parameter #false))
(define default-diaflow-free-track-style-make : (Parameterof (Option Dia-Free-Edge-Style-Make)) (make-parameter #false))
(define default-diaflow-edge-label-rotate? : (Parameterof Boolean) (make-parameter #false))

(define-configuration diaflow-edge-base-style : DiaFlow-Edge-Style #:as dia-edge-base-style
  #:format "default-diaflow-edge-~a"
  ([font : (Option Font) default-edge-label-font]
   [font-paint : Option-Fill-Paint 'DimGray]
   [line-paint : Maybe-Stroke-Paint default-edge-stroke]
   [source-shape : Option-Edge-Tip-Shape #false]
   [target-shape : Option-Edge-Tip-Shape default-arrow-tip]))

(define-configuration diaflow-arrow-style : DiaFlow-Arrow-Style #:as dia-edge-style
  #:format "default-diaflow-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [line-paint : Maybe-Stroke-Paint (void)]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]))

(define-configuration diaflow-free-track-style : DiaFlow-Free-Track-Style #:as dia-edge-style
  #:format "default-diaflow-free-track-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [line-paint : Maybe-Stroke-Paint default-free-edge-stroke]
   [source-shape : Maybe-Edge-Tip-Shape #false]
   [target-shape : Maybe-Edge-Tip-Shape #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (DiaFlow-Node-Style-Make S) (Dia-Node-Style-Make* S (Option Symbol)))

(define default-diaflow-canonical-start-name : (Parameterof String) (make-parameter ""))
(define default-diaflow-canonical-stop-name : (Parameterof String) (make-parameter ""))

(define default-diaflow-node-label-string : (Parameterof (Option Dia-Node-Id->String)) (make-parameter #false))
(define default-diaflow-arrow-label-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Arrow-Label-Style))) (make-parameter #false))

(define default-diaflow-input-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Input-Style))) (make-parameter #false))
(define default-diaflow-output-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Output-Style))) (make-parameter #false))
(define default-diaflow-start-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Start-Style))) (make-parameter #false))
(define default-diaflow-stop-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Stop-Style))) (make-parameter #false))
(define default-diaflow-process-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Process-Style))) (make-parameter #false))
(define default-diaflow-decision-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Decision-Style))) (make-parameter #false))
(define default-diaflow-comment-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Comment-Style))) (make-parameter #false))
(define default-diaflow-subroutine-style-make : (Parameterof (Option (DiaFlow-Node-Style-Make DiaFlow-Subroutine-Style))) (make-parameter #false))
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

(define-configuration diaflow-arrow-label-style : DiaFlow-Arrow-Label-Style #:as dia-node-style
  #:format "default-diaflow-arrow-label-~a"
  ([block-width : (Option Flonum) -1.2]
   [block-height : (Option Flonum) -1.2]
   [font : (Option Font) default-arrow-label-font]
   [font-paint : Option-Fill-Paint 'DimGray]
   [stroke-paint : Maybe-Stroke-Paint #false]
   [fill-paint : Maybe-Fill-Paint #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diaflow-start-style : DiaFlow-Start-Style #:as dia-node-style
  #:format "default-diaflow-start-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'MediumSeaGreen]
   [fill-paint : Maybe-Fill-Paint 'Honeydew]))

(define-configuration diaflow-stop-style : DiaFlow-Stop-Style #:as dia-node-style
  #:format "default-diaflow-stop-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DarkSlateGray]
   [fill-paint : Maybe-Fill-Paint 'Gainsboro]))

(define-configuration diaflow-input-style : DiaFlow-Input-Style #:as dia-node-style
  #:format "default-diaflow-input-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Chocolate]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-output-style : DiaFlow-Output-Style #:as dia-node-style
  #:format "default-diaflow-output-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Purple]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-process-style : DiaFlow-Process-Style #:as dia-node-style
  #:format "default-diaflow-process-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'RoyalBlue]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-decision-style : DiaFlow-Decision-Style #:as dia-node-style
  #:format "default-diaflow-decision-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* (default-diaflow-block-width)  0.90)]
   [block-height : (Option Nonnegative-Flonum) (* (default-diaflow-block-height) 0.90)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Crimson]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-comment-style : DiaFlow-Comment-Style #:as dia-node-style
  #:format "default-diaflow-comment-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DimGray]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-subroutine-style : DiaFlow-Subroutine-Style #:as dia-node-style
  #:format "default-diaflow-subroutine-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DodgerBlue]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-inspection-style : DiaFlow-Inspection-Style #:as dia-node-style
  #:format "default-diaflow-inspection-~a"
  ([block-width : (Option Nonnegative-Flonum)  (* (default-diaflow-block-width)  0.384)]
   [block-height : (Option Nonnegative-Flonum) (* (default-diaflow-block-height) 0.618)]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'DeepSkyBlue]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]))

(define-configuration diaflow-reference-style : DiaFlow-Reference-Style #:as dia-node-style
  #:format "default-diaflow-reference-~a"
  ([block-width : (Option Nonnegative-Flonum) (* (default-diaflow-block-width) 0.24)]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'ForestGreen]
   [fill-paint : Maybe-Fill-Paint 'LightCyan]))

(define-configuration diaflow-database-style : DiaFlow-Database-Style #:as dia-node-style
  #:format "default-diaflow-database-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-document-style : DiaFlow-Document-Style #:as dia-node-style
  #:format "default-diaflow-document-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-operation-style : DiaFlow-Operation-Style #:as dia-node-style
  #:format "default-diaflow-operation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Burlywood]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-keyboard-style : DiaFlow-Keyboard-Style #:as dia-node-style
  #:format "default-diaflow-keyboard-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint (void)]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-configuration diaflow-preparation-style : DiaFlow-Preparation-Style #:as dia-node-style
  #:format "default-diaflow-preparation-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint 'Maroon]
   [fill-paint : Maybe-Fill-Paint (void)]))
