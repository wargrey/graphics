#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [plot-desc-stroke plot-desc-pen]))

(require geofun/font)
(require geofun/stroke)

(require geofun/digitama/base)
(require geofun/digitama/convert)

(require geofun/digitama/paint/self)
(require geofun/digitama/layer/type)

(require "real.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Tick->Sticker (-> (Option Symbol) String Font Color (U Geo Void False)))
(define-type Plot-Axis-Real->Sticker (-> (Option Symbol) Real Any Nonnegative-Flonum Font Color (U Geo (Pairof Geo Geo-Pin-Anchor) Void False)))
(define-type Plot-Axis-Real->Dot (-> (Option Symbol) Real Any Nonnegative-Flonum Color Nonnegative-Flonum (U Geo (Pairof Geo Geo-Pin-Anchor) Void False)))

(define-type Plot-Axis-Real-Filter (-> Plot-Axis-Real-Datum (Values (Option Flonum) Any)))
(define-type Plot-Axis-Integer-Filter (-> Plot-Axis-Integer-Datum (Values (Option Integer) Any)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-axis-nonzero-values-wrap : (->* () (Plot-Axis-Integer-Filter) Plot-Axis-Integer-Filter)
  (lambda [[integer-values plot-axis-integer-values]]
    (λ [[r : Plot-Axis-Integer-Datum]]
      (define-values (val obj) (integer-values r))
      (values (and val (if (zero? val) #false val))
              obj))))

(define plot-cartesian-dot : (-> Flonum Flonum Float-Complex)
  (lambda [x y]
    (make-rectangular x (- y))))

(define plot-desc-stroke : (->* ()
                                (Stroke #:color (Option Color) #:opacity (Option Real) #:width (Option Real)
                                        #:cap (Option Symbol) #:join (U Symbol Real False)
                                        #:dash (Option Stroke-Dash-Datum) #:offset (Option Real))
                                Stroke)
  (lambda [#:color [color #false] #:opacity [opacity #false] #:width [width #false]
           #:cap [cap #false] #:join [join #false] #:dash [dash #false] #:offset [offset #false]
           [baseline (default-plot-function-stroke)]]
    (desc-stroke #:color color #:opacity opacity #:width width
                 #:cap cap #:join join #:dash dash #:offset offset
                 baseline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-plot-axis-real-filter : (Parameterof Plot-Axis-Real-Filter) (make-parameter plot-axis-real-values))
(define default-plot-axis-integer-filter : (Parameterof Plot-Axis-Integer-Filter) (make-parameter plot-axis-integer-values))

(define default-plot-axis-desired-ticks : (Parameterof Positive-Index) (make-parameter 7))
(define default-plot-axis-real-tick-steps : (Parameterof (Listof Positive-Index)) (make-parameter (list 1 2 4 5)))
(define default-plot-axis-length : (Parameterof Real) (make-parameter 400.0))
(define default-plot-axis-unit-length : (Parameterof (Option Real)) (make-parameter #false))

(define default-plot-cartesian-width : (Parameterof Real) (make-parameter 400.0))
(define default-plot-cartesian-height : (Parameterof Real) (make-parameter +inf.0))
(define default-plot-visualizer-domain-range : (Parameterof (Pairof Real Real)) (make-parameter (cons -5 5)))
(define default-plot-visualizer-samples : (Parameterof Positive-Index) (make-parameter 512))

(define default-plot-function-stroke : (Parameterof Stroke) (make-parameter (desc-stroke #:width 1.5 #:join 'round #:cap 'round #:opacity 0.75)))
