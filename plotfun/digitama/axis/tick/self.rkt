#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../../arithmetics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Ticks (Listof Plot-Tick))
(define-type Plot-Minor-Tick-Count (U Index (-> Real Real Index)))
(define-type Plot-Ticks-Generate
  (->* (Plot-Tick-Engine (Option (Pairof Real Real)))
       ((Option Plot-Tick-Format))
       (Values Plot-Ticks (Option Real) Plot-Minor-Tick-Count)))

(define-type Plot-Ticks-Layout (-> Real Real (Values #;#:values (Listof Real) #;#:step (Option Real))))
(define-type Plot-Tick-Format (-> Real Integer String))
;(define-type (Plot-Ticks-Layout* Arg) (-> Real Real (Values (Listof Plot-Tick) Arg)))
;(define-type (Plot-Ticks-Format* Arg) (-> Real Real Plot-Tick Arg String))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot-tick-engine
  ([layout : Plot-Ticks-Layout]
   [format : Plot-Tick-Format]
   [range : (Option (Pairof Real Real))]
   [minor-count : Plot-Minor-Tick-Count]
   [unit-scale : Positive-Real])
  #:constructor-name unsafe-plot-tick-engine
  #:type-name Plot-Tick-Engine
  #:transparent)

(struct plot-tick
  ([value : Real]
   [desc : String]
   [major? : Boolean])
  #:type-name Plot-Tick
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-ticks-generate : Plot-Ticks-Generate
  (lambda [self major-range [alt-format #false]]
    (define rng (or (plot-tick-engine-range self) major-range))
    
    (if (pair? rng)
        (let*-values ([(g) (plot-tick-engine-layout self)]
                      [(f) (or alt-format (plot-tick-engine-format self))]
                      [(tmin tmax) (values (car rng) (cdr rng))]
                      [(precision) (tick-precision tmin tmax)]
                      [(ticks maybe-stable-step) (g tmin tmax)]
                      [(minor-count) (plot-tick-engine-minor-count self)])
          (values (if (or (eq? minor-count 0) (null? ticks) (null? (cdr ticks)))
                      (for/list ([val (in-list ticks)])
                        (plot-tick val (f val precision) #true))
                      (for/fold ([ts : Plot-Ticks (list (plot-tick (car ticks) (f (car ticks) precision) #true))])
                                ([prev (in-list ticks)]
                                 [val (in-list (cdr ticks))])
                        (define mc (if (index? minor-count) minor-count (minor-count prev val)))

                        (if (> mc 0)
                            (append ts
                                    (let ([step (/ (- val prev) (+ mc 1))])
                                      (for/list : Plot-Ticks ([minor (in-range (+ prev step) val step)])
                                        (plot-tick minor (f minor precision) #false)))
                                    (list (plot-tick val (f val precision) #true)))
                            ts)))
                  maybe-stable-step
                  minor-count))
        (values null #false 1))))
