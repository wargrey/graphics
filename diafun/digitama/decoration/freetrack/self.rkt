#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/dc/path)
(require geofun/digitama/track/self)
(require geofun/digitama/path/label)
(require geofun/digitama/path/tip/self)
(require geofun/digitama/geometry/footprint)

(require "../../track/style.rkt")
(require "../../track/interface.rkt")
(require "../../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Higher-Level API
; For three-value results,
; Void means use engine's fallback
; False means use denies the object

(define-type Dia-Free-Track-Adjuster
  (-> (Dia-Track-Style Dia-Free-Track-Style)
      Dia-Track-Endpoint Dia-Track-Endpoint Geo-Path-Clean-Prints*
      (Listof Geo-Path-Label-Datum) (Listof Geo-Track-Info-Datum)
      (U (Dia-Track-Style Dia-Free-Track-Style) False Void)))

(define-type Dia-Free-Track-Annotator (Dia-Track-Annotator Dia-Free-Track-Style))

(define-type Dia-Free-Track-Builder
  (-> Dia-Track-Endpoint Dia-Track-Endpoint Geo-Path-Clean-Prints* (Dia-Track-Style-Spec Dia-Free-Track-Style)
      (U Geo-Path Void False)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia-free-track-style () #:type-name Dia-Free-Track-Style)

(define default-free-track-theme-adjuster : (Parameterof (Option Dia-Free-Track-Adjuster)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration dia-free-track-backstop-style : Dia-Free-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-dia-free-track-~a"
  ([font : Font dia-preset-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint dia-preset-free-track-stroke]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip #false]
   [label-rotate? : Boolean #true]
   [label-inline? : Boolean #true]
   [label-distance : (Option Length+%) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct dia~open~track~style : Dia~Open~Track~Style #:-> dia-free-track-style #:for dia-track-style
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : Maybe-Color (void)]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Length+%) (void)]))

(define-phantom-struct dia~closed~track~style : Dia~Closed~Track~Style #:-> dia-free-track-style #:for dia-track-style
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : Maybe-Color (void)]
   [dash : (Option Stroke-Dash+Offset) 'long-dash]
   [source-tip : Maybe-Geo-Tip #false]
   [target-tip : Maybe-Geo-Tip #false]
   [label-rotate? : (U Boolean Void) #true]
   [label-inline? : (U Boolean Void) #false]
   [label-distance : (U Void Length+%) (&% -100)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter dia-free-track-factory : Dia-Free-Track-Factory
  ([adjuster : (Option Dia-Free-Track-Adjuster) #false]
   [annotator : (Option Dia-Free-Track-Annotator) #false]
   [builder : (Option Dia-Free-Track-Builder) #false]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-dia-free-track-backstop-style])
  #:transparent)
