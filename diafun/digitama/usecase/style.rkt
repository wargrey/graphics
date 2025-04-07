#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../node/style.rkt"))
(provide (all-from-out "../edge/style.rkt"))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)
(require geofun/digitama/base)

(require "../node/style.rkt")
(require "../edge/style.rkt")
(require "../edge/tip.rkt")

(require "../shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-error-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaUC-Error-Arrow-Style))) (make-parameter #false))
(define default-diauc-association-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaUC-Association-Arrow-Style))) (make-parameter #false))
(define default-diauc-include-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaUC-Include-Arrow-Style))) (make-parameter #false))
(define default-diauc-extend-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaUC-Extend-Arrow-Style))) (make-parameter #false))
(define default-diauc-generalization-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaUC-Generalization-Arrow-Style))) (make-parameter #false))
(define default-diauc-free-track-style-make : (Parameterof (Option (Dia-Free-Edge-Style-Make DiaUC-Free-Track-Style))) (make-parameter #false))

(define-configuration diauc-edge-fallback-style : DiaUC-Edge-Style #:as dia-edge-base-style
  #:format "default-diauc-edge-~a"
  ([font : (Option Font) default-edge-label-font]
   [font-paint : Option-Fill-Paint 'DimGray]
   [line-paint : Maybe-Stroke-Paint default-edge-stroke]
   [source-shape : Option-Edge-Tip-Shape #false]
   [target-shape : Option-Edge-Tip-Shape default-arrow-tip]
   [label-rotate? : Boolean #true]
   [label-inline? : Boolean #false]
   [label-distance : (Option Flonum) #false]))

(define-configuration diauc-error-arrow-style : DiaUC-Error-Arrow-Style #:as dia-edge-style
  #:format "default-diauc-error-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'Crimson]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-association-arrow-style : DiaUC-Association-Arrow-Style #:as dia-edge-style
  #:format "default-diauc-association-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DarkSlateGray]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-include-arrow-style : DiaUC-Include-Arrow-Style #:as dia-edge-style
  #:format "default-diauc-include-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumSeaGreen]
   [dash : (Option Stroke-Dash-Datum) 'short-dash]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-extend-arrow-style : DiaUC-Extend-Arrow-Style #:as dia-edge-style
  #:format "default-diauc-extend-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'RoyalBlue]
   [dash : (Option Stroke-Dash-Datum) 'short-dash]
   [source-shape : Maybe-Edge-Tip-Shape default-arrow-tip]
   [target-shape : Maybe-Edge-Tip-Shape #false]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-generalization-arrow-style : DiaUC-Generalization-Arrow-Style #:as dia-edge-style
  #:format "default-diauc-generalization-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumPurple]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape default-generalization-tip]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-free-track-style : DiaUC-Free-Track-Style #:as dia-edge-style
  #:format "default-diauc-free-track-~a"
  ([font : (Option Font) default-node-label-font]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash-Datum) 'solid]
   [source-shape : Maybe-Edge-Tip-Shape #false]
   [target-shape : Maybe-Edge-Tip-Shape #false]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) -16.0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-actor-style-make : (Parameterof (Option (Dia-Path-Node-Style-Make DiaUC-Actor-Style))) (make-parameter #false))
(define default-diauc-ucase-style-make : (Parameterof (Option (Dia-Path-Node-Style-Make DiaUC-UCase-Style))) (make-parameter #false))

(define-configuration diauc-node-fallback-style : DiaUC-Node-Style #:as dia-node-base-style
  #:format "default-diauc-~a"
  ([block-width : Nonnegative-Flonum 160.0]
   [block-height : Nonnegative-Flonum 50.0]
   [font : (Option Font) default-node-label-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint default-node-stroke]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diauc-actor-style : DiaUC-Actor-Style #:as dia-node-style
  #:format "default-diauc-actor-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diauc-block-height)) 2.0)]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 1.0]
   [stroke-color : (U Color Void False) #false]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'LawnGreen]))

(define-configuration diauc-ucase-style : DiaUC-UCase-Style #:as dia-node-style
  #:format "default-diauc-ucase-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'Azure]))
