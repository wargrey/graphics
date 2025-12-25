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
(require geofun/digitama/path/tip/diamond)

(require "../block/style.rkt")
(require "../track/style.rkt")
(require "../shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-association-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaCls-Association-Arrow-Style))) (make-parameter #false))
(define default-diacls-bidirection-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaCls-Bidirection-Arrow-Style))) (make-parameter #false))
(define default-diacls-composition-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaCls-Composition-Arrow-Style))) (make-parameter #false))
(define default-diacls-aggregation-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaCls-Aggregation-Arrow-Style))) (make-parameter #false))
(define default-diacls-generalization-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaCls-Generalization-Arrow-Style))) (make-parameter #false))
(define default-diacls-realization-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaCls-Realization-Arrow-Style))) (make-parameter #false))
(define default-diacls-dependency-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaCls-Dependency-Arrow-Style))) (make-parameter #false))
(define default-diacls-free-track-style-make : (Parameterof (Option (Dia-Free-Track-Style-Make DiaCls-Free-Track-Style))) (make-parameter #false))

(define-configuration diacls-track-backstop-style : DiaCls-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-diacls-track-~a"
  ([font : Font default-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint default-track-stroke]
   [opacity : (Option Real) #false]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip default-arrow-tip]
   [label-rotate? : Boolean #false]
   [label-inline? : Boolean #false]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diacls-generalization-arrow-style : DiaCls-Generalization-Arrow-Style #:as dia-track-style
  #:format "default-diacls-generalization-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumPurple]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip default-generalization-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-realization-arrow-style : DiaCls-Realization-Arrow-Style #:as dia-track-style
  #:format "default-diacls-realization-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumPurple]
   [dash : (Option Stroke-Dash+Offset) 'short-dash]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip default-generalization-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-composition-arrow-style : DiaCls-Composition-Arrow-Style #:as dia-track-style
  #:format "default-diacls-include-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumAquamarine]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip default-composition-tip]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-aggregation-arrow-style : DiaCls-Aggregation-Arrow-Style #:as dia-track-style
  #:format "default-diacls-extend-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'Turquoise]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip default-aggregation-tip]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-association-arrow-style : DiaCls-Association-Arrow-Style #:as dia-track-style
  #:format "default-diacls-association-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-bidirection-arrow-style : DiaCls-Bidirection-Arrow-Style #:as dia-track-style
  #:format "default-diacls-bidirection-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip #false]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-dependency-arrow-style : DiaCls-Dependency-Arrow-Style #:as dia-track-style
  #:format "default-diacls-dependency-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash+Offset) 'short-dash]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diacls-free-track-style : DiaCls-Free-Track-Style #:as dia-track-style
  #:format "default-diacls-free-track-~a"
  ([font : (Option Font) default-block-brief-font]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'Gray]
   [dash : (Option Stroke-Dash+Offset) 'solid]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip #false]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diacls-interface-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaCls-Interface-Style))) (make-parameter #false))
(define default-diacls-class-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaCls-Class-Style))) (make-parameter #false))

(define-configuration diacls-block-backstop-style : DiaCls-Block-Style #:as dia-block-backstop-style
  #:format "default-diacls-~a"
  ([block-width : Nonnegative-Flonum 150.0]
   [block-height : Nonnegative-Flonum 45.0]
   [font : Font default-table-header-font]
   [font-paint : Fill-Paint 'GhostWhite]
   [stroke-paint : Option-Stroke-Paint #false]
   [fill-paint : Option-Fill-Paint 'CadetBlue]
   [opacity : (Option Real) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diacls-interface-style : DiaCls-Interface-Style #:as dia-block-style
  #:format "default-diacls-interface-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'DarkKhaki]
   [opacity : (Option Real) #false]))

(define-configuration diacls-class-style : DiaCls-Class-Style #:as dia-block-style
  #:format "default-diacls-class-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Real) #false]))
