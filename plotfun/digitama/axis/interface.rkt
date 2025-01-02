#lang typed/racket/base

(provide (all-defined-out))

(require geofun/paint)
(require geofun/font)

(require geofun/digitama/convert)

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Digit->Sticker (-> (Option Symbol) Integer (Option Font) Option-Fill-Paint (U Geo Void False)))
(define-type Plot-Axis-Real->Sticker (-> (Option Symbol) Plot-Axis-Real-Datum (Option Font) Option-Fill-Paint (U (Pairof Flonum Geo) Void False)))
(define-type Plot-Axis-Real->Dot (-> (Option Symbol) Plot-Axis-Real-Datum Nonnegative-Flonum Option-Fill-Paint (U Geo Void False)))
(define-type Plot-Axis-Real-Filter (-> Plot-Axis-Real-Datum (Values (Option Flonum) (U Complex Any))))

(define default-plot-axis-digit-filter : (Parameterof (-> Integer (Option String))) (make-parameter number->string))
(define default-plot-axis-real-filter : (Parameterof Plot-Axis-Real-Filter) (make-parameter plot-axis-real-values))

(define default-axis-digit-font : (Parameterof Font) (make-parameter (font (font-family->face 'sans-serif) 12.0 'normal 'normal 'normal 'normal)))
(define default-axis-real-font : (Parameterof Font) (make-parameter (font (font-family->face 'math) 12.0 'normal 'normal 'normal 'normal)))
