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
(struct ram-block-backstop-style expr-slot-backstop-style
  ([ignored-paint : Option-Fill-Paint])
  #:type-name RAM-Block-Backstop-Style
  #:transparent)

(struct ram-block-style expr-slot-style
  ([ignored-paint : Maybe-Fill-Paint])
  #:type-name RAM-Block-Style
  #:transparent)

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
(define-type (RAM-Location-Style-Make S) (Expr-Slot-Style-Make C-Placeholder S Symbol))

(define default-ram-variable-style-make  : (Parameterof (Option (RAM-Location-Style-Make RAM-Variable-Style)))  (make-parameter #false))
(define default-ram-pointer-style-make : (Parameterof (Option (RAM-Location-Style-Make RAM-Pointer-Style))) (make-parameter #false))
(define default-ram-array-style-make : (Parameterof (Option (RAM-Location-Style-Make RAM-Array-Style))) (make-parameter #false))
(define default-ram-padding-style-make   : (Parameterof (Option (RAM-Location-Style-Make RAM-Padding-Style)))   (make-parameter #false))

(define-configuration ram-location-backstop-style : RAM-Location-Backstop-Style #:as ram-block-backstop-style
  #:format "default-ram-location~a"
  ([font : Font expr-preset-expr-font]
   [font-paint : Fill-Paint 'DimGrey]
   [stroke-paint : Option-Stroke-Paint (default-stroke)]
   [fill-paint : Option-Fill-Paint 'GhostWhite]
   [opacity : (Option Real) #false]
   [ignored-paint : Option-Fill-Paint 'Grey]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration ram-variable-style : RAM-Variable-Style #:as ram-block-style
  #:format "default-ram-variable-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'ForestGreen]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Real) #false]
   [ignored-paint : Maybe-Fill-Paint (void)]))

(define-configuration ram-array-style : RAM-Array-Style #:as ram-block-style
  #:format "default-ram-array-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DodgerBlue]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Real) #false]
   [ignored-paint : Maybe-Fill-Paint (void)]))

(define-configuration ram-pointer-style : RAM-Pointer-Style #:as ram-block-style
  #:format "default-ram-pointer-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'RoyalBlue]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [opacity : (Option Real) #false]
   [ignored-paint : Maybe-Fill-Paint (void)]))

(define-configuration ram-padding-style : RAM-Padding-Style #:as ram-block-style
  #:format "default-ram-padding-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DimGrey]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DimGrey]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'LightGrey]
   [opacity : (Option Real) #false]
   [ignored-paint : Maybe-Fill-Paint (void)]))
