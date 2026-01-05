#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "self.rkt")
(require "format.rkt")

(require "layout/real.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-integer-ticks
  (lambda [#:format [format : Plot-Tick-Format plot-tick->label-text]
           #:desired-ticks [desired-ticks : Positive-Index (default-plot-desired-ticks)]
           #:base [base : Positive-Byte 10]] : Plot-Tick-Engine
    (unsafe-plot-tick-engine
     (λ [[tmin : Real] [tmax : Real]]
       (plot-integer-tick-layout (exact-floor tmin) (exact-ceiling tmax) base desired-ticks))
     format #false 0 1.0)))

(define plot-real-ticks
  (lambda [#:format [format : Plot-Tick-Format plot-tick->label-text]
           #:desired-ticks [desired-ticks : Positive-Index (default-plot-desired-ticks)]
           #:steps [divisors : (Listof Positive-Index) (default-plot-real-tick-steps)]
           #:minor-count [minor-count : Index 1]
           #:base [base : Positive-Byte 10]] : Plot-Tick-Engine
    (unsafe-plot-tick-engine
     (λ [[tmin : Real] [tmax : Real]]
       (plot-real-tick-layout tmin tmax base desired-ticks divisors))
     format #false minor-count 1.0)))

(define plot-real-ticks*
  (lambda [#:format [format : Plot-Tick-Format plot-tick->label-text]
           #:desired-ticks [desired-ticks : Positive-Index (default-plot-desired-ticks)]
           #:steps [divisors : (Listof Positive-Index) (default-plot-real-tick-steps)]
           #:minor-count [minor-count : Index 1]
           #:base [base : Positive-Byte 10]] : Plot-Tick-Engine
    (unsafe-plot-tick-engine
     (λ [[tmin : Real] [tmax : Real]]
       (if (and (exact-integer? tmin) (exact-integer? tmax))
           (if (= (- tmax tmin) 1)
               (plot-rational-tick-layout tmin tmax base desired-ticks 100 real->double-flonum)
               (plot-integer-tick-layout tmin tmax base desired-ticks))
           (plot-real-tick-layout tmin tmax base desired-ticks divisors)))
     format #false minor-count 1.0)))
