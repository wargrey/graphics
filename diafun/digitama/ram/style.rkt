#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../node/style.rkt"))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)
(require geofun/digitama/base)

(require "../node/style.rkt")
(require "../shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct ram-location-base-style dia-node-base-style
  ([ignored-paint : Option-Fill-Paint])
  #:type-name RAM-Location-Base-Style
  #:transparent)

(struct ram-location-style dia-node-style
  ([ignored-paint : Option-Fill-Paint])
  #:type-name RAM-Location-Style
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
(define-type (RAM-Location-Style-Make S) (Dia-Node-Style-Make* S Symbol))

(define default-ram-variable-style-make  : (Parameterof (Option (RAM-Location-Style-Make RAM-Variable-Style)))  (make-parameter #false))
(define default-ram-pointer-style-make : (Parameterof (Option (RAM-Location-Style-Make RAM-Pointer-Style))) (make-parameter #false))
(define default-ram-array-style-make : (Parameterof (Option (RAM-Location-Style-Make RAM-Array-Style))) (make-parameter #false))
(define default-ram-padding-style-make   : (Parameterof (Option (RAM-Location-Style-Make RAM-Padding-Style)))   (make-parameter #false))

(define-configuration ram-location-fallback-style : RAM-Node-Style #:as ram-location-base-style
  #:format "default-ram-location~a"
  ([width : Nonnegative-Flonum 80.0]
   [height : Nonnegative-Flonum 36.0]
   [font : (Option Font) default-number-font]
   [font-paint : Option-Fill-Paint 'DimGrey]
   [stroke-paint : Maybe-Stroke-Paint default-black-stroke]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]
   [ignored-paint : Option-Fill-Paint 'Grey]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration ram-variable-style : RAM-Variable-Style #:as ram-location-style
  #:format "default-ram-variable-~a"
  ([width : False #false]
   [height : False #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'ForestGreen]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [ignored-paint : Option-Fill-Paint #false]))

(define-configuration ram-array-style : RAM-Array-Style #:as ram-location-style
  #:format "default-ram-array-~a"
  ([width : False #false]
   [height : False #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DodgerBlue]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [ignored-paint : Option-Fill-Paint #false]))

(define-configuration ram-pointer-style : RAM-Pointer-Style #:as ram-location-style
  #:format "default-ram-pointer-~a"
  ([width : False #false]
   [height : False #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'RoyalBlue]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [ignored-paint : Option-Fill-Paint #false]))

(define-configuration ram-padding-style : RAM-Padding-Style #:as ram-location-style
  #:format "default-ram-padding-~a"
  ([width : False #false]
   [height : False #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DimGrey]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DimGrey]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'LightGrey]
   [ignored-paint : Option-Fill-Paint #false]))
