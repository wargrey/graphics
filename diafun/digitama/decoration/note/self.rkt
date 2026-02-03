#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/self)
(require geofun/digitama/path/tip/self)
(require geofun/digitama/richtext/self)
(require geofun/digitama/geometry/sides)

(require "../../block/dc.rkt")
(require "../../block/style.rkt")
(require "../../block/interface.rkt")
(require "../../track/style.rkt")

(require "../../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Note-Metadata (Option Symbol))
(define-type Dia-Note-Typesetter (-> Symbol Geo-Rich-Text (Dia-Block-Style-Spec Dia-Note-Block-Style) Nonnegative-Flonum Nonnegative-Flonum (Option Geo)))
(define-type Dia-Note-Describer (Dia-Block-Describer Dia-Note-Block-Style Dia-Note-Metadata))

(define-type Dia-Note-Track-Theme-Adjuster (Dia-Track-Theme-Adjuster Dia-Note-Track-Style))
(define-type Dia-Note-Block-Theme-Adjuster (Dia-Block-Theme-Adjuster Dia-Note-Block-Style Dia-Note-Metadata))

(define-type Dia-Note-Builder
  (-> Symbol Geo (Dia-Block-Style-Spec Dia-Note-Block-Style) Geo-Standard-Insets (Option Flonum) Dia-Note-Metadata
      ; Yes, the engine doesn't provide a fallback
      (Option Dia:Block:Note)))

(struct dia-note-track-style () #:type-name Dia-Note-Track-Style #:transparent)

(define default-dia-note-track-theme-adjuster : (Parameterof (Option Dia-Note-Track-Theme-Adjuster)) (make-parameter #false))
(define default-dia-note-block-theme-adjuster : (Parameterof (Option Dia-Note-Block-Theme-Adjuster)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration dia-note-block-backstop-style : Dia-Note-Backstop-Style #:as dia-block-backstop-style
  #:format "default-dia-note-~a"
  ([width : Nonnegative-Flonum 128.0]
   [height : Nonnegative-Flonum +inf.0]
   [padding : Dia-Block-Padding (&L 1.0 'ex)]
   [font : Font dia-preset-note-font]
   [font-paint : Fill-Paint 'DimGray]
   [stroke-paint : Option-Stroke-Paint dia-preset-note-block-stroke]
   [fill-paint : Option-Fill-Paint 'WhiteSmoke]))

(define-configuration dia-note-track-backstop-style : Dia-Note-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-dia-note-track-~a"
  ([font : Font dia-preset-track-label-font]
   [font-paint : Fill-Paint 'DimGray]
   [line-paint : Stroke-Paint dia-preset-note-track-stroke]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip #false]
   [label-rotate? : Boolean #true]
   [label-inline? : Boolean #true]
   [label-distance : (Option Length+%) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct dia-note-block-style : Dia-Note-Block-Style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct dia~block~note~style : Dia~Block~Note~~Style #:-> dia-note-track-style #:for dia-track-style
  ([font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color #false]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Length+%) (void)]))

(define-phantom-struct dia~track~note~style : Dia~Track~Note~Style #:-> dia-note-track-style #:for dia-track-style
  ([font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Length+%) #false]
   [color : Maybe-Color #false]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip 'circle]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Length+%) (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct dia-note-factory : Dia-Note-Factory
  ([typesetter : (Option Dia-Note-Typesetter) #false]
   [builder : (Option Dia-Note-Builder) #false]
   [λtrack-backstop-style : (-> Dia-Note-Track-Backstop-Style) make-dia-note-track-backstop-style]
   [λblock-backstop-style : (-> Dia-Block-Backstop-Style) make-dia-note-block-backstop-style])
  #:transparent)
