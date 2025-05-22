#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "self.rkt")
(require "layout.rkt")

(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-integer-ticks : (-> [#:base Positive-Byte] [#:desired-ticks Positive-Index] (Plot-Ticks-Generate Integer))
  (lambda [#:base [base 10] #:desired-ticks [desired-ticks (default-plot-axis-desired-ticks)]]
    (plot-ticks-generator
     (λ [[tmin : Integer] [tmax : Integer]]
       (plot-integer-tick-layout tmin tmax base desired-ticks)))))

(define plot-real-ticks : (-> [#:base Positive-Byte] [#:desired-ticks Positive-Index] [#:steps (Listof Positive-Index)] (Plot-Ticks-Generate Real))
  (lambda [#:base [base 10] #:desired-ticks [desired-ticks (default-plot-axis-desired-ticks)] #:steps [divisors (default-plot-axis-real-tick-steps)]]
    (plot-ticks-generator
     (λ [[tmin : Real] [tmax : Real]]
       (plot-real-tick-layout tmin tmax base desired-ticks divisors)))))

(define plot-real-ticks* : (-> [#:base Positive-Byte] [#:desired-ticks Positive-Index] [#:steps (Listof Positive-Index)] (Plot-Ticks-Generate Real))
  (lambda [#:base [base 10] #:desired-ticks [desired-ticks (default-plot-axis-desired-ticks)] #:steps [divisors (default-plot-axis-real-tick-steps)]]
    (plot-ticks-generator
     (λ [[tmin : Real] [tmax : Real]]
       (if (and (exact-integer? tmin) (exact-integer? tmax))
           (plot-integer-tick-layout tmin tmax base desired-ticks)
           (plot-real-tick-layout tmin tmax base desired-ticks divisors))))))
