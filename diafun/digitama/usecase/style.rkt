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
(struct uc-block-style () #:type-name UC-Block-Style)
(struct uc-track-style () #:type-name UC-Track-Style)

(define-type UC-Block-Metadata (Option Symbol))
(define-type UC-Block-Theme-Adjuster (Dia-Block-Theme-Adjuster UC-Block-Style UC-Block-Metadata))
(define-type UC-Track-Theme-Adjuster (Dia-Track-Theme-Adjuster UC-Track-Style))

(define default-uc-block-theme-adjuster : (Parameterof (Option UC-Block-Theme-Adjuster)) (make-parameter #false))
(define default-uc-track-theme-adjuster : (Parameterof (Option UC-Track-Theme-Adjuster)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration uc-block-backstop-style : UC-Block-Backstop-Style #:as dia-block-backstop-style
  #:format "default-uc-block-~a"
  ([width : Nonnegative-Flonum 160.0]
   [height : Nonnegative-Flonum 50.0]
   [padding : Dia-Block-Padding (&L 0.333 'em)]
   [font : Font dia-preset-block-caption-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint dia-preset-block-stroke]
   [fill-paint : Option-Fill-Paint 'GhostWhite]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct uc-actor-style : UC-Actor-Style #:-> uc-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size (&% 80)] ; ignored by its shape, but used to limit the length of its caption 
   [height : Dia-Block-Option-Size (&% 200)]
   [padding : Dia-Block-Option-Padding #false] ; the top padding would be used as the gapsize of its caption
   [font : (Option Dia-Block-Font-Style) dia-preset-header-font-tweak]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) 1.0]
   [stroke-color : Maybe-Color #false]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'LawnGreen]))

(define-phantom-struct uc-case-style : UC-Case-Style #:-> uc-block-style #:for dia-block-style
  ([width : Dia-Block-Option-Size #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Dia-Block-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'Azure]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration uc-track-backstop-style : UC-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-uc-track-~a"
  ([font : Font dia-preset-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint dia-preset-track-stroke]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip default-arrow-tip]
   [label-rotate? : Boolean #true]
   [label-inline? : Boolean #false]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct uc~association~style : UC~Association~Style #:-> uc-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'DarkGray]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip #false]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct uc~include~style : UC~Include~Style #:-> uc-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'MediumAquamarine]
   [dash : (Option Stroke-Dash+Offset) 'short-dash]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct uc~extend~style : UC~Extend~Style #:-> uc-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'CornflowerBlue]
   [dash : (Option Stroke-Dash+Offset) 'short-dash]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip default-arrow-tip]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))

(define-phantom-struct uc~generalization~style : UC~Generalization~Style #:-> uc-track-style #:for dia-track-style
  ([font : (Option Dia-Track-Font-Style) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color 'MediumPurple]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip default-generalization-tip]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Flonum) (void)]))
