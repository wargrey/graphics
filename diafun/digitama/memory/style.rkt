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
(struct memory-location-base-style dia-node-base-style
  ([ignored-paint : Option-Fill-Paint])
  #:type-name Memory-Location-Base-Style
  #:transparent)

(struct memory-location-style dia-node-style
  ([ignored-paint : Option-Fill-Paint])
  #:type-name Memory-Location-Style
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-memory-fixnum-radix :  (Parameterof Positive-Byte) (make-parameter 10))
(define default-memory-padding-radix : (Parameterof Positive-Byte) (make-parameter 2))
(define default-memory-raw-data-radix : (Parameterof Positive-Byte) (make-parameter 16))

(define default-memory-human-readable? : (Parameterof Boolean) (make-parameter #false))
(define default-memory-no-padding? : (Parameterof Boolean) (make-parameter #false))
(define default-memory-padding-limit : (Parameterof Index) (make-parameter 4))
(define default-memory-address-mask : (Parameterof Natural) (make-parameter #xFFFFFFFF))

(define default-memory-optimize? : (Parameterof Boolean) (make-parameter #false))
(define default-memory-reverse-address? : (Parameterof Boolean) (make-parameter #true))

(define default-memory-entry : (Parameterof Symbol) (make-parameter 'main))
(define default-memory-lookahead-size : (Parameterof Index) (make-parameter 0))
(define default-memory-lookbehind-size : (Parameterof Index) (make-parameter 0))
(define default-memory-body-limit : (Parameterof Index) (make-parameter 1024))

(define default-memory-location-gapsize : (Parameterof Nonnegative-Flonum) (make-parameter 8.0))
(define default-memory-segment-gapsize : (Parameterof Nonnegative-Flonum) (make-parameter 16.0))
(define default-memory-snapshot-gapsize : (Parameterof Nonnegative-Flonum) (make-parameter 64.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Memory-Location-Style-Make S) (Dia-Node-Style-Make* S Symbol))

(define default-memory-variable-style-make  : (Parameterof (Option (Memory-Location-Style-Make Memory-Variable-Style)))  (make-parameter #false))
(define default-memory-pointer-style-make : (Parameterof (Option (Memory-Location-Style-Make Memory-Pointer-Style))) (make-parameter #false))
(define default-memory-array-style-make : (Parameterof (Option (Memory-Location-Style-Make Memory-Array-Style))) (make-parameter #false))
(define default-memory-padding-style-make   : (Parameterof (Option (Memory-Location-Style-Make Memory-Padding-Style)))   (make-parameter #false))

(define-configuration memory-location-fallback-style : Memory-Node-Style #:as memory-location-base-style
  #:format "default-memory-location~a"
  ([width : Nonnegative-Flonum 80.0]
   [height : Nonnegative-Flonum 36.0]
   [font : (Option Font) default-number-font]
   [font-paint : Option-Fill-Paint 'DimGrey]
   [stroke-paint : Maybe-Stroke-Paint default-black-stroke]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]
   [ignored-paint : Option-Fill-Paint 'Grey]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration memory-variable-style : Memory-Variable-Style #:as memory-location-style
  #:format "default-memory-variable-~a"
  ([width : False #false]
   [height : False #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'ForestGreen]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [ignored-paint : Option-Fill-Paint #false]))

(define-configuration memory-array-style : Memory-Array-Style #:as memory-location-style
  #:format "default-memory-array-~a"
  ([width : False #false]
   [height : False #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DodgerBlue]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [ignored-paint : Option-Fill-Paint #false]))

(define-configuration memory-pointer-style : Memory-Pointer-Style #:as memory-location-style
  #:format "default-memory-pointer-~a"
  ([width : False #false]
   [height : False #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'RoyalBlue]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint (void)]
   [ignored-paint : Option-Fill-Paint #false]))

(define-configuration memory-padding-style : Memory-Padding-Style #:as memory-location-style
  #:format "default-memory-padding-~a"
  ([width : False #false]
   [height : False #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint 'DimGrey]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) 'DimGrey]
   [stroke-dash : (Option Stroke-Dash-Datum) #false]
   [fill-paint : Maybe-Fill-Paint 'LightGrey]
   [ignored-paint : Option-Fill-Paint #false]))
