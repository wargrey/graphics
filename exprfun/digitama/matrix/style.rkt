#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../slot/style.rkt"))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)
(require geofun/digitama/base)

(require "types.rkt")
(require "../slot/style.rkt")
(require "../shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-mtx-hole-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Hole-Style))) (make-parameter #false))
(define default-mtx-mask-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Mask-Style))) (make-parameter #false))
(define default-mtx-entry-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Entry-Style))) (make-parameter #false))
(define default-mtx-row-header-style-make : (Parameterof (Option (Mtx-Header-Style-Make Mtx-Row-Header-Style))) (make-parameter #false))
(define default-mtx-col-header-style-make : (Parameterof (Option (Mtx-Header-Style-Make Mtx-Col-Header-Style))) (make-parameter #false))
(define default-mtx-corner-style-make : (Parameterof (Option (Mtx-Header-Style-Make Mtx-Corner-Style))) (make-parameter #false))

(define-configuration mtx-backstop-style : Mtx-Backstop-Style #:as expr-slot-backstop-style
  #:format "default-mtx-~a"
  ([font : Font default-expr-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint default-slot-stroke]
   [fill-paint : Option-Fill-Paint #false]
   [opacity : (Option Real) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration mtx-row-header-style : Mtx-Row-Header-Style #:as expr-slot-style
  #:format "default-mtx-row-header-~a"
  ([font : (Option Font) default-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-configuration mtx-col-header-style : Mtx-Col-Header-Style #:as expr-slot-style
  #:format "default-mtx-col-header-~a"
  ([font : (Option Font) default-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-configuration mtx-corner-style : Mtx-Corner-Style #:as expr-slot-style
  #:format "default-mtx-corner-~a"
  ([font : (Option Font) default-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-configuration mtx-hole-style : Mtx-Hole-Style #:as expr-slot-style
  #:format "default-mtx-hole-~a"
  ([font : (Option Font) default-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) 'dot]
   [fill-paint : Maybe-Fill-Paint 'WhiteSmoke]
   [opacity : (Option Real) #false]))

(define-configuration mtx-mask-style : Mtx-Mask-Style #:as expr-slot-style
  #:format "default-mtx-mask-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]
   [opacity : (Option Real) #false]))

(define-configuration mtx-entry-style : Mtx-Entry-Style #:as expr-slot-style
  #:format "default-mtx-entry-~a"
  ([font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))
