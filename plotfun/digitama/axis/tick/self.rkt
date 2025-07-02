#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/list)

(require "../../arithmetics.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Ticks (Listof (Pairof Plot-Tick String)))
(define-type Plot-Ticks-Generate (->* (Plot-Tick-Engine (Option (Pairof Real Real))) ((Option Plot-Tick-Format)) (Values Plot-Ticks (Option Real))))
(define-type Plot-Ticks-Layout (-> Real Real (Values (Listof Real) (Option Real))))
(define-type Plot-Tick-Format (-> Real Integer String))
;(define-type (Plot-Ticks-Layout* Arg) (-> Real Real (Values (Listof Plot-Tick) Arg)))
;(define-type (Plot-Ticks-Format* Arg) (-> Real Real Plot-Tick Arg String))

(define-type Plot-Fixed-Tick-Format (U False Plot-Tick-Format (Listof (Option String))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot-tick-engine
  ([layout : Plot-Ticks-Layout]
   [format : Plot-Tick-Format]
   [range : (Option (Pairof Real Real))])
  #:constructor-name unsafe-plot-tick-engine
  #:type-name Plot-Tick-Engine
  #:transparent)

(struct plot-tick
  ([value : Real])
  #:type-name Plot-Tick
  #:transparent)

(define plot-tick-value* : (-> (U Plot-Tick (Pairof Plot-Tick String)) Real)
  (lambda [self]
    (if (pair? self)
        (plot-tick-value* (car self))
        (plot-tick-value self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-ticks-generate : Plot-Ticks-Generate
  (lambda [self range [alt-format #false]]
    (define rng (or (plot-tick-engine-range self) range))
    
    (if (pair? rng)
        (let*-values ([(g) (plot-tick-engine-layout self)]
                      [(f) (or alt-format (plot-tick-engine-format self))]
                      [(tmin tmax) (values (car rng) (cdr rng))]
                      [(precision) (tick-precision tmin tmax)]
                      [(ticks maybe-stable-step) (g tmin tmax)])
          (values (for/list ([val (in-list ticks)])
                    (cons (plot-tick val)
                          (f val precision)))
                  maybe-stable-step))
        (values null #false))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-fixed-ticks : (case-> [(Listof Real) -> Plot-Tick-Engine]
                                   [(Listof Real) Plot-Fixed-Tick-Format -> Plot-Tick-Engine]
                                   [Real Real Real -> Plot-Tick-Engine]
                                   [Real Real Real Plot-Fixed-Tick-Format -> Plot-Tick-Engine])
  (case-lambda
    [(ticks labels)
     (define ordered-ticks : (Listof Real) (remove-duplicates (sort ticks <) =))
     
     (if (pair? ordered-ticks)
         (let ([rng (and #;(pair? (cdr ordered-ticks)) (cons (car ordered-ticks) (last ordered-ticks)))]
               [layout (λ [tmin tmax] (values ticks #false))])
           (cond [(and (list? labels) (pair? (filter values labels)))
                  (let* ([lbls (for/hasheqv : (HashTable Real (Option String)) ([t (in-list ticks)]
                                                                                [s (in-list labels)]
                                                                                #:when s)
                                 (values t s))]
                         [predefine-labels (lambda [[tval : Real] [para : Integer]]
                                             (or (hash-ref lbls tval (λ [] #false))
                                                 (plot-tick->label-string tval para)))])
                    (unsafe-plot-tick-engine layout predefine-labels rng))]
                 [(procedure? labels) (unsafe-plot-tick-engine layout labels rng)]
                 [else (unsafe-plot-tick-engine layout plot-tick->label-string rng)]))
         plot-no-ticks)]
    [(ticks) (plot-fixed-ticks ticks plot-tick->label-string)]
    [(tmin tmax step) (plot-fixed-ticks (range tmin tmax step) plot-tick->label-string)]
    [(tmin tmax step format) (plot-fixed-ticks (range tmin tmax step) format)]))

(define plot-single-ticks : (->* (Real) ((Option String)) Plot-Tick-Engine)
  (lambda [tick [label #false]]
    (plot-fixed-ticks (list tick) (list label))))

(define plot-interval-ticks : (->* (Real) ((Option Plot-Tick-Format)) Plot-Tick-Engine)
  (lambda [step [format #false]]
    (unsafe-plot-tick-engine
     (λ [[tmin : Real] [tmax : Real]]
       (if (positive? step)
           (values (range tmin (+ tmax (* step 0.5)) step) step)
           (values (list tmin) #false)))
     (or format plot-tick->label-string)
     #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-tick->label-string : Plot-Tick-Format
  (lambda [tval para]
    (cond [(exact? tval) (number->string tval)]
          [(integer? tval) (number->string (inexact->exact tval))]
          [(>= para 0) (~r tval #:precision para)]
          [else (~a tval)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-no-ticks : Plot-Tick-Engine
  (unsafe-plot-tick-engine
   (λ [[tmin : Real] [tmax : Real]] (values null #false))
   plot-tick->label-string
   #false))
