#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/path/tip/self)

(require "interface.rkt")

(require "../track/style.rkt")
(require "../track/interface.rkt")
(require "../presets.rkt")

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
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct dia-free-track-style : Dia-Free-Track-Style #:for dia-track-style
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [width : (Option Flonum) #false]
   [color : Maybe-Color (void)]
   [dash : (Option Stroke-Dash+Offset) #false]
   [source-tip : Maybe-Geo-Tip (void)]
   [target-tip : Maybe-Geo-Tip (void)]
   [label-rotate? : (U Boolean Void) (void)]
   [label-inline? : (U Boolean Void) (void)]
   [label-distance : (U Void Flonum) (void)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct/parameter dia-free-track-factory : Dia-Free-Track-Factory
  ([adjuster : (Option (Dia-Free-Track-Adjuster Dia-Free-Track-Style)) #false]
   [annotator : (Option (Dia-Track-Annotator Dia-Free-Track-Style)) #false]
   [builder : (Option (Dia-Free-Track-Builder Dia-Free-Track-Style)) #false]
   [λbackstop-style : (-> Dia-Track-Backstop-Style) make-dia-free-track-backstop-style])
  #:transparent)
