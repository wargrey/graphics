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
(require "../edge/tip/arrow.rkt")
(require "../edge/tip/diamond.rkt")

(require "../shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-association-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaCls-Association-Arrow-Style))) (make-parameter #false))
(define default-diacls-bidirection-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaCls-Bidirection-Arrow-Style))) (make-parameter #false))
(define default-diacls-composition-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaCls-Composition-Arrow-Style))) (make-parameter #false))
(define default-diacls-aggregation-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaCls-Aggregation-Arrow-Style))) (make-parameter #false))
(define default-diacls-generalization-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaCls-Generalization-Arrow-Style))) (make-parameter #false))
(define default-diacls-realization-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaCls-Realization-Arrow-Style))) (make-parameter #false))
(define default-diacls-dependency-arrow-style-make : (Parameterof (Option (Dia-Edge-Style-Make DiaCls-Dependency-Arrow-Style))) (make-parameter #false))
(define default-diacls-free-track-style-make : (Parameterof (Option (Dia-Free-Edge-Style-Make DiaCls-Free-Track-Style))) (make-parameter #false))

(define-configuration diacls-edge-fallback-style : DiaCls-Edge-Style #:as dia-edge-base-style
  #:format "default-diacls-edge-~a"
  ([font : (Option Font) default-edge-label-font]
   [font-paint : Option-Fill-Paint 'DimGray]
   [line-paint : Maybe-Stroke-Paint default-edge-stroke]
   [source-shape : Option-Edge-Tip-Shape #false]
   [target-shape : Option-Edge-Tip-Shape default-arrow-tip]
   [label-rotate? : Boolean #false]
   [label-inline? : Boolean #false]
   [label-distance : (Option Flonum) #false]))

(define-configuration diacls-generalization-arrow-style : DiaCls-Generalization-Arrow-Style #:as dia-edge-style
  #:format "default-diacls-generalization-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumPurple]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape default-generalization-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-realization-arrow-style : DiaCls-Realization-Arrow-Style #:as dia-edge-style
  #:format "default-diacls-realization-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumPurple]
   [dash : (Option Stroke-Dash-Datum) 'short-dash]
   [source-shape : Maybe-Edge-Tip-Shape (void)]
   [target-shape : Maybe-Edge-Tip-Shape default-generalization-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-composition-arrow-style : DiaCls-Composition-Arrow-Style #:as dia-edge-style
  #:format "default-diacls-include-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumAquamarine]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape default-composition-tip]
   [target-shape : Maybe-Edge-Tip-Shape default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-aggregation-arrow-style : DiaCls-Aggregation-Arrow-Style #:as dia-edge-style
  #:format "default-diacls-extend-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'Turquoise]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape default-aggregation-tip]
   [target-shape : Maybe-Edge-Tip-Shape default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-association-arrow-style : DiaCls-Association-Arrow-Style #:as dia-edge-style
  #:format "default-diacls-association-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape #false]
   [target-shape : Maybe-Edge-Tip-Shape default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-bidirection-arrow-style : DiaCls-Bidirection-Arrow-Style #:as dia-edge-style
  #:format "default-diacls-bidirection-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash-Datum) #false]
   [source-shape : Maybe-Edge-Tip-Shape #false]
   [target-shape : Maybe-Edge-Tip-Shape #false]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-dependency-arrow-style : DiaCls-Dependency-Arrow-Style #:as dia-edge-style
  #:format "default-diacls-dependency-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash-Datum) 'short-dash]
   [source-shape : Maybe-Edge-Tip-Shape #false]
   [target-shape : Maybe-Edge-Tip-Shape default-arrow-tip]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-free-track-style : DiaCls-Free-Track-Style #:as dia-edge-style
  #:format "default-diacls-free-track-~a"
  ([font : (Option Font) default-node-label-font]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'Gray]
   [dash : (Option Stroke-Dash-Datum) 'solid]
   [source-shape : Maybe-Edge-Tip-Shape #false]
   [target-shape : Maybe-Edge-Tip-Shape #false]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-interface-style-make : (Parameterof (Option (Dia-Path-Node-Style-Make DiaCls-Interface-Style))) (make-parameter #false))
(define default-diacls-class-style-make : (Parameterof (Option (Dia-Path-Node-Style-Make DiaCls-Class-Style))) (make-parameter #false))

(define-configuration diacls-node-fallback-style : DiaCls-Node-Style #:as dia-node-base-style
  #:format "default-diacls-~a"
  ([block-width : Nonnegative-Flonum 150.0]
   [block-height : Nonnegative-Flonum 45.0]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint 'GhostWhite]
   [stroke-paint : Maybe-Stroke-Paint #false]
   [fill-paint : Maybe-Fill-Paint 'CadetBlue]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diacls-interface-style : DiaCls-Interface-Style #:as dia-node-style
  #:format "default-diacls-interface-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) #false]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'DarkKhaki]))

(define-configuration diacls-class-style : DiaCls-Class-Style #:as dia-node-style
  #:format "default-diacls-class-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) #false]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))
