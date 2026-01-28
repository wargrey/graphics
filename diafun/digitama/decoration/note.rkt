#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require geofun/font)
(require geofun/paint)

(require "interface.rkt")

(require "../block/style.rkt")
(require "../block/interface.rkt")
(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Note-Metadata (Option Symbol))
(define-type Dia-Note-Theme-Adjuster (Dia-Block-Theme-Adjuster Dia-Note-Style Dia-Note-Metadata))
(define-type Dia-Note-Typesetter (Dia-Block-Typesetter Dia-Note-Style))
(define-type Dia-Note-Describer (Dia-Block-Describer Dia-Note-Style Dia-Note-Metadata))

(define default-dia-note-theme-adjuster : (Parameterof (Option Dia-Note-Theme-Adjuster)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration dia-note-backstop-style : Dia-Note-Backstop-Style #:as dia-block-backstop-style
  #:format "default-dia-note-~a"
  ([min-width : Nonnegative-Flonum 200.0]
   [min-height : Nonnegative-Flonum 50.0]
   [padding : Dia-Block-Padding (&L 0.333 'em)]
   [font : Font dia-preset-note-font]
   [font-paint : Fill-Paint 'Gray]
   [stroke-paint : Option-Stroke-Paint dia-preset-block-stroke]
   [fill-paint : Option-Fill-Paint 'WhiteSmoke]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct dia-note-style : Dia-Note-Style #:for dia-block-style
  ([width : Dia-Block-Option-Size  #false]
   [height : Dia-Block-Option-Size #false]
   [padding : Dia-Block-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter dia-note-factory : Dia-Note-Factory
  ([typesetter : (Option Dia-Note-Typesetter) #false]
   [builder : (Option (Dia-Note-Builder Dia-Note-Style Dia-Note-Metadata)) #false]
   [λbackstop-style : (-> Dia-Block-Backstop-Style) make-dia-note-backstop-style])
  #:transparent)
