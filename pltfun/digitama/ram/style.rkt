#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out exprfun/digitama/slot/style))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

(require exprfun/digitama/slot/style)
(require exprfun/digitama/presets)

(require "variable.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type RAM-Slot-Style-Spec (Expr-Slot-Style-Spec RAM-Block-Style))

(struct ram-slot-backstop-style expr-slot-backstop-style
  ([ignored-paint : Option-Fill-Paint])
  #:type-name RAM-Slot-Backstop-Style
  #:transparent)

(struct ram-block-style
  ([ignored-paint : Maybe-Fill-Paint])
  #:type-name RAM-Block-Style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-ram-fixnum-radix :  (Parameterof Positive-Byte) (make-parameter 10))
(define default-ram-padding-radix : (Parameterof Positive-Byte) (make-parameter 2))
(define default-ram-raw-data-radix : (Parameterof Positive-Byte) (make-parameter 16))

(define default-ram-human-readable? : (Parameterof Boolean) (make-parameter #false))
(define default-ram-no-padding? : (Parameterof Boolean) (make-parameter #false))
(define default-ram-padding-limit : (Parameterof Index) (make-parameter 4))
(define default-ram-address-mask : (Parameterof Natural) (make-parameter #xFFFFFFFF))

(define default-ram-optimize? : (Parameterof Boolean) (make-parameter #false))
(define default-ram-reverse-address? : (Parameterof Boolean) (make-parameter #true))

(define default-ram-entry : (Parameterof Symbol) (make-parameter 'main))
(define default-ram-lookahead-size : (Parameterof Index) (make-parameter 0))
(define default-ram-lookbehind-size : (Parameterof Index) (make-parameter 0))
(define default-ram-body-limit : (Parameterof Index) (make-parameter 1024))

(define default-ram-location-gapsize : (Parameterof Nonnegative-Flonum) (make-parameter 8.0))
(define default-ram-segment-gapsize : (Parameterof Nonnegative-Flonum) (make-parameter 16.0))
(define default-ram-snapshot-gapsize : (Parameterof Nonnegative-Flonum) (make-parameter 64.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (RAM-Location-Theme-Adjuster S) (Expr-Slot-Theme-Adjuster C-Placeholder S Symbol))

(define-type RAM-Variable-Theme-Adjuster (RAM-Location-Theme-Adjuster RAM-Variable-Style))
(define-type RAM-Pointer-Theme-Adjuster (RAM-Location-Theme-Adjuster RAM-Pointer-Style))
(define-type RAM-Array-Theme-Adjuster (RAM-Location-Theme-Adjuster RAM-Array-Style))
(define-type RAM-Padding-Theme-Adjuster (RAM-Location-Theme-Adjuster RAM-Padding-Style))

(define default-ram-variable-theme-adjuster  : (Parameterof (Option RAM-Variable-Theme-Adjuster)) (make-parameter #false))
(define default-ram-pointer-theme-adjuster : (Parameterof (Option RAM-Pointer-Theme-Adjuster)) (make-parameter #false))
(define default-ram-array-theme-adjuster : (Parameterof (Option RAM-Array-Theme-Adjuster)) (make-parameter #false))
(define default-ram-padding-theme-adjuster : (Parameterof (Option RAM-Padding-Theme-Adjuster)) (make-parameter #false))

(define-configuration ram-location-backstop-style : RAM-Location-Backstop-Style #:as ram-slot-backstop-style
  #:format "default-ram-location~a"
  ([padding : Expr-Slot-Padding 4.0]
   [font : Font expr-preset-expr-font]
   [font-paint : Fill-Paint 'DimGrey]
   [stroke-paint : Option-Stroke-Paint (default-stroke)]
   [fill-paint : Option-Fill-Paint 'GhostWhite]
   [opacity : (Option Real) #false]
   [ignored-paint : Option-Fill-Paint 'Grey]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct ram-variable-style : RAM-Variable-Style #:as ram-block-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'ForestGreen]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Real) #false])
  #:metadata
  ([ignored-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct ram-array-style : RAM-Array-Style #:as ram-block-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DodgerBlue]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Real) #false])
  #:metadata
  ([ignored-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct ram-pointer-style : RAM-Pointer-Style #:as ram-block-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'RoyalBlue]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Real) #false])
  #:metadata
  ([ignored-paint : Maybe-Fill-Paint (void)]))

(define-phantom-struct ram-padding-style : RAM-Padding-Style #:as ram-block-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DimGrey]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DimGrey]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'LightGrey]
   [opacity : (Option Real) #false])
  #:metadata
  ([ignored-paint : Maybe-Fill-Paint (void)]))
