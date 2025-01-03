#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/color)

(require geofun/digitama/convert)
(require geofun/digitama/layer/type)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Digit->Sticker (-> (Option Symbol) Integer Font Color (U Geo Void False)))
(define-type Plot-Axis-Real->Sticker (-> (Option Symbol) Flonum Any Nonnegative-Flonum Font Color (U Geo (Pairof Geo Geo-Pin-Anchor) Void False)))
(define-type Plot-Axis-Real->Dot (-> (Option Symbol) Flonum Any Nonnegative-Flonum Color (U Geo (Pairof Geo Geo-Pin-Anchor) Void False)))

(define-type Plot-Axis-Real-Filter (-> Plot-Axis-Real-Datum (Values (Option Flonum) (U Complex Any))))

(define default-plot-axis-digit-filter : (Parameterof (-> Integer (Option String))) (make-parameter number->string))
(define default-plot-axis-real-filter : (Parameterof Plot-Axis-Real-Filter) (make-parameter plot-axis-real-values))

(define default-axis-color : (Parameterof Color) (make-parameter (rgb* 'DarkSlateGray)))
(define default-axis-digit-font : (Parameterof Font) (make-parameter (font (font-family->face 'sans-serif) 12.0 'normal 'normal 'normal 'normal)))
(define default-axis-real-font : (Parameterof (Option Font)) (make-parameter #false))
