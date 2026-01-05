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
(define default-diauc-association-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaUC-Association-Arrow-Style))) (make-parameter #false))
(define default-diauc-include-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaUC-Include-Arrow-Style))) (make-parameter #false))
(define default-diauc-extend-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaUC-Extend-Arrow-Style))) (make-parameter #false))
(define default-diauc-generalization-arrow-style-make : (Parameterof (Option (Dia-Track-Style-Make DiaUC-Generalization-Arrow-Style))) (make-parameter #false))
(define default-diauc-free-track-style-make : (Parameterof (Option (Dia-Free-Track-Style-Make DiaUC-Free-Track-Style))) (make-parameter #false))

(define-configuration diauc-track-backstop-style : DiaUC-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-diauc-track-~a"
  ([font : Font default-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint default-track-stroke]
   [opacity : (Option Real) #false]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip default-arrow-tip]
   [label-rotate? : Boolean #true]
   [label-inline? : Boolean #false]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diauc-association-arrow-style : DiaUC-Association-Arrow-Style #:as dia-track-style
  #:format "default-diauc-association-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DarkGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip #false]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-include-arrow-style : DiaUC-Include-Arrow-Style #:as dia-track-style
  #:format "default-diauc-include-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumAquamarine]
   [dash : (Option Stroke-Dash+Offset) 'short-dash]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-extend-arrow-style : DiaUC-Extend-Arrow-Style #:as dia-track-style
  #:format "default-diauc-extend-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'CornflowerBlue]
   [dash : (Option Stroke-Dash+Offset) 'short-dash]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-generalization-arrow-style : DiaUC-Generalization-Arrow-Style #:as dia-track-style
  #:format "default-diauc-generalization-arrow-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'MediumPurple]
   [dash : (Option Stroke-Dash+Offset) #false]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip default-generalization-tip]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-configuration diauc-free-track-style : DiaUC-Free-Track-Style #:as dia-track-style
  #:format "default-diauc-free-track-~a"
  ([font : (Option Font) default-block-caption-font]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : (U Color Void False) 'DimGray]
   [dash : (Option Stroke-Dash+Offset) 'solid]
   [opacity : (Option Real) #false]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip #false]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) -16.0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-diauc-actor-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaUC-Actor-Style))) (make-parameter #false))
(define default-diauc-ucase-style-make : (Parameterof (Option (Dia-Block-Style-Make DiaUC-UCase-Style))) (make-parameter #false))

(define-configuration diauc-block-backstop-style : DiaUC-Block-Style #:as dia-block-backstop-style
  #:format "default-diauc-~a"
  ([block-width : Nonnegative-Flonum 160.0]
   [block-height : Nonnegative-Flonum 50.0]
   [font : Font default-block-caption-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint default-block-stroke]
   [fill-paint : Option-Fill-Paint 'GhostWhite]
   [opacity : (Option Real) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diauc-actor-style : DiaUC-Actor-Style #:as dia-block-style
  #:format "default-diauc-actor-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) (* ((default-diauc-block-height)) 2.0)]
   [font : (Option Font) default-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 1.0]
   [stroke-color : (U Color Void False) #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LawnGreen]
   [opacity : (Option Real) #false]))

(define-configuration diauc-ucase-style : DiaUC-UCase-Style #:as dia-block-style
  #:format "default-diauc-ucase-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Azure]
   [opacity : (Option Real) #false]))
