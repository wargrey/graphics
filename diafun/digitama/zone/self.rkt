#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)
(require digimon/measure)

(require geofun/font)
(require geofun/paint)

(require "style.rkt")
(require "interface.rkt")

(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Zone-Theme-Adjuster (#%Dia-Zone-Theme-Adjuster Dia-Zone-Style Dia-Zone-Metadata))

(define default-dia-rubber-zone-theme-adjuster  : (Parameterof (Option Dia-Zone-Theme-Adjuster)) (make-parameter #false))
(define default-dia-fixed-zone-theme-adjuster : (Parameterof (Option Dia-Zone-Theme-Adjuster)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration dia-zone-backstop-style : Dia-Zone-Backstop-Style #:as #%dia-zone-backstop-style
  #:format "default-dia-zone-~a"
  ([padding : Dia-Zone-Padding (&L 1.0 'em)]
   [font : Font dia-preset-zone-font]
   [font-paint : Fill-Paint 'DimGray]
   [stroke-paint : Option-Stroke-Paint dia-preset-zone-stroke]
   [fill-paint : Option-Fill-Paint #false]
   [corner-radius : Length+% 0.0]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct dia-rubber-zone-style : Dia-Rubber-Zone-Style #:-> dia-zone-style #:for #%dia-zone-style
  ([padding : Dia-Zone-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) 'long-dash]
   [fill-paint : Maybe-Fill-Paint (void)]
   [corner-radius : (Option Length+%) #false]))

(define-phantom-struct dia-fixed-zone-style : Dia-Fixed-Zone-Style #:-> dia-zone-style #:for #%dia-zone-style
  ([padding : Dia-Zone-Option-Padding #false]
   [font : (Option Font+Tweak) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Length+%) #false]
   [stroke-color : Maybe-Color (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [corner-radius : (Option Length+%) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct dia-zone-factory : Dia-Zone-Factory
  ([identifier : (Dia-Zone-Identifier Dia-Zone-Style) void]
   [typesetter : (Option (Dia-Zone-Typesetter Dia-Zone-Style)) #false]
   [builder : (Option (Dia-Zone-Builder Dia-Zone-Style)) #false]
   [fallback-builder : (Dia-Zone-Builder Dia-Zone-Style) void]
   [λbackstop-style : (-> Dia-Zone-Backstop-Style) make-dia-zone-backstop-style])
  #:transparent)
