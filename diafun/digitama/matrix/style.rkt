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
(define default-diamtx-hole-style-make : (Parameterof (Option (Dia-Matrix-Block-Style-Make DiaMtx-Hole-Style))) (make-parameter #false))
(define default-diamtx-mask-style-make : (Parameterof (Option (Dia-Matrix-Block-Style-Make DiaMtx-Mask-Style))) (make-parameter #false))
(define default-diamtx-entry-style-make : (Parameterof (Option (Dia-Matrix-Block-Style-Make DiaMtx-Entry-Style))) (make-parameter #false))
(define default-diamtx-row-header-style-make : (Parameterof (Option (Dia-Matrix-Block-Style-Make DiaMtx-Row-Header-Style))) (make-parameter #false))
(define default-diamtx-column-header-style-make : (Parameterof (Option (Dia-Matrix-Block-Style-Make DiaMtx-Column-Header-Style))) (make-parameter #false))
(define default-diamtx-corner-style-make : (Parameterof (Option (Dia-Matrix-Block-Style-Make DiaMtx-Corner-Style))) (make-parameter #false))

(define-configuration diamtx-fallback-style : DiaMtx-Style #:as dia-block-base-style
  #:format "default-diamtx-~a"
  ([block-width : Nonnegative-Flonum 32.0]
   [block-height : Nonnegative-Flonum 32.0]
   [font : (Option Font) default-block-brief-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-paint : Maybe-Stroke-Paint default-cell-stroke]
   [fill-paint : Maybe-Fill-Paint #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration diamtx-row-header-style : DiaMtx-Row-Header-Style #:as dia-block-style
  #:format "default-diamtx-row-header-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]))

(define-configuration diamtx-column-header-style : DiaMtx-Column-Header-Style #:as dia-block-style
  #:format "default-diamtx-column-header-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]))

(define-configuration diamtx-corner-style : DiaMtx-Corner-Style #:as dia-block-style
  #:format "default-diamtx-corner-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]))

(define-configuration diamtx-hole-style : DiaMtx-Hole-Style #:as dia-block-style
  #:format "default-diamtx-hole-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) default-table-header-font]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) 'dot]
   [fill-paint : Maybe-Fill-Paint 'AliceBlue]))

(define-configuration diamtx-mask-style : DiaMtx-Mask-Style #:as dia-block-style
  #:format "default-diamtx-mask-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) 0.0]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint 'GhostWhite]))

(define-configuration diamtx-entry-style : DiaMtx-Entry-Style #:as dia-block-style
  #:format "default-diamtx-entry-~a"
  ([block-width : (Option Nonnegative-Flonum) #false]
   [block-height : (Option Nonnegative-Flonum) #false]
   [font : (Option Font) #false]
   [font-paint : Option-Fill-Paint #false]
   [stroke-width : (Option Flonum) #false]
   [stroke-color : (U Color Void False) (void)]
   [stroke-dash : (Option Stroke-Dash+Offset) #false]
   [fill-paint : Maybe-Fill-Paint #false]))
