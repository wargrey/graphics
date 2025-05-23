#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "self.rkt")
(require "layout.rkt")

(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-integer-ticks
  (lambda [#:format [format : Plot-Tick-Format plot-tick->label-string]
           #:desired-ticks [desired-ticks : Positive-Index (default-plot-axis-desired-ticks)]
           #:base [base : Positive-Byte 10]] : Plot-Tick-Engine
    (unsafe-plot-tick-engine
     (λ [[tmin : Real] [tmax : Real]]
       (plot-integer-tick-layout (exact-floor tmin) (exact-ceiling tmax) base desired-ticks))
     format
     #false)))

(define plot-real-ticks
  (lambda [#:format [format : Plot-Tick-Format plot-tick->label-string]
           #:desired-ticks [desired-ticks : Positive-Index (default-plot-axis-desired-ticks)]
           #:steps [divisors : (Listof Positive-Index) (default-plot-axis-real-tick-steps)]
           #:base [base : Positive-Byte 10]] : Plot-Tick-Engine
    (unsafe-plot-tick-engine
     (λ [[tmin : Real] [tmax : Real]]
       (plot-real-tick-layout tmin tmax base desired-ticks divisors))
     format
     #false)))

(define plot-real-ticks*
  (lambda [#:format [format : Plot-Tick-Format plot-tick->label-string]
           #:desired-ticks [desired-ticks : Positive-Index (default-plot-axis-desired-ticks)]
           #:steps [divisors : (Listof Positive-Index) (default-plot-axis-real-tick-steps)]
           #:base [base : Positive-Byte 10]] : Plot-Tick-Engine
    (unsafe-plot-tick-engine
     (λ [[tmin : Real] [tmax : Real]]
       (if (and (exact-integer? tmin) (exact-integer? tmax))
           (plot-integer-tick-layout tmin tmax base desired-ticks)
           (plot-real-tick-layout tmin tmax base desired-ticks divisors)))
     format
     #false)))
