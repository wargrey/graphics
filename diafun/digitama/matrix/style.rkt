#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../block/style.rkt"))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)
(require geofun/digitama/base)

(require "self.rkt")
(require "../block/style.rkt")
(require "../shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-mtx-hole-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Hole-Style))) (make-parameter #false))
(define default-mtx-mask-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Mask-Style))) (make-parameter #false))
(define default-mtx-entry-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Entry-Style))) (make-parameter #false))
(define default-mtx-row-header-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Row-Header-Style))) (make-parameter #false))
(define default-mtx-col-header-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Col-Header-Style))) (make-parameter #false))
(define default-mtx-corner-style-make : (Parameterof (Option (Mtx-Style-Make Mtx-Corner-Style))) (make-parameter #false))

(define-configuration mtx-backstop-style : Mtx-Backstop-Style #:as dia-block-backstop-style
  #:format "default-mtx-~a"
  ([block-width : Nonnegative-Flonum 32.0]
   [block-height : Nonnegative-Flonum 32.0]
   [font : Font default-block-brief-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint default-cell-stroke]
   [fill-paint : Option-Fill-Paint #false]
   [opacity : (Option Real) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration mtx-row-header-style : Mtx-Row-Header-Style #:as dia-block-style
  #:format "default-mtx-row-header-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-configuration mtx-col-header-style : Mtx-Col-Header-Style #:as dia-block-style
  #:format "default-mtx-col-header-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-configuration mtx-corner-style : Mtx-Corner-Style #:as dia-block-style
  #:format "default-mtx-corner-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-configuration mtx-hole-style : Mtx-Hole-Style #:as dia-block-style
  #:format "default-mtx-hole-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) 'dot]
   [fill-paint : Maybe-Fill-Paint 'WhiteSmoke]
   [opacity : (Option Real) #false]))

(define-configuration mtx-mask-style : Mtx-Mask-Style #:as dia-block-style
  #:format "default-mtx-mask-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]
   [opacity : (Option Real) #false]))

(define-configuration mtx-entry-style : Mtx-Entry-Style #:as dia-block-style
  #:format "default-mtx-entry-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))
