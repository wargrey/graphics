#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../slot/style.rkt"))

(require digimon/struct)

(require geofun/font)
(require geofun/paint)
(require geofun/digitama/base)

(require "../slot/style.rkt")
(require "../presets.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct mtx-slot-style () #:type-name Mtx-Slot-Style)

(define-configuration mtx-backstop-style : Mtx-Backstop-Style #:as expr-slot-backstop-style
  #:format "default-mtx-~a"
  ([padding : Expr-Slot-Padding 4.0]
   [font : Font expr-preset-expr-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint expr-preset-slot-stroke]
   [fill-paint : Option-Fill-Paint #false]
   [opacity : (Option Real) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-phantom-struct mtx-row-header-style : Mtx-Row-Header-Style #:-> mtx-slot-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) expr-preset-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-phantom-struct mtx-col-header-style : Mtx-Col-Header-Style #:-> mtx-slot-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) expr-preset-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-phantom-struct mtx-corner-style : Mtx-Corner-Style #:-> mtx-slot-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) expr-preset-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))

(define-phantom-struct mtx-hole-style : Mtx-Hole-Style #:-> mtx-slot-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) expr-preset-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) 'dot]
   [fill-paint : Maybe-Fill-Paint 'WhiteSmoke]
   [opacity : (Option Real) #false]))

(define-phantom-struct mtx-mask-style : Mtx-Mask-Style #:-> mtx-slot-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]
   [opacity : (Option Real) #false]))

(define-phantom-struct mtx-entry-style : Mtx-Entry-Style #:-> mtx-slot-style #:for expr-slot-style
  ([padding : Expr-Slot-Option-Padding #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]
   [opacity : (Option Real) #false]))
