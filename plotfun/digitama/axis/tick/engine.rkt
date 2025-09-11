#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "real.rkt"))

(require racket/list)
(require racket/math)

(require "self.rkt")
(require "format.rkt")

(require "real.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Fixed-Tick-Format (U False Plot-Tick-Format (Listof (Option String))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-fixed-ticks : (case-> [(Listof Real) -> Plot-Tick-Engine]
                                   [(Listof Real) Plot-Fixed-Tick-Format -> Plot-Tick-Engine]
                                   [Real Real Real -> Plot-Tick-Engine]
                                   [Real Real Real Plot-Fixed-Tick-Format -> Plot-Tick-Engine])
  (case-lambda
    [(ticks) (plot-fixed-ticks* ticks 0 plot-tick->label-string)]
    [(ticks labels) (plot-fixed-ticks* ticks 0 labels)]
    [(tmin tmax step) (plot-fixed-ticks* (range tmin (+ tmax step) step) 0 plot-tick->label-string)]
    [(tmin tmax step format) (plot-fixed-ticks* (range tmin (+ tmax step) step) 0 format)]))

(define plot-fixed-ticks* : (case-> [(Listof Real) Plot-Minor-Tick-Count -> Plot-Tick-Engine]
                                    [(Listof Real) Plot-Minor-Tick-Count Plot-Fixed-Tick-Format -> Plot-Tick-Engine]
                                    [Real Real Real Plot-Minor-Tick-Count -> Plot-Tick-Engine]
                                    [Real Real Real Plot-Minor-Tick-Count Plot-Fixed-Tick-Format -> Plot-Tick-Engine])
  (case-lambda
    [(ticks minor-count labels)
     (if (pair? ticks)
         (let ([rng (and #;(pair? (cdr ticks)) (cons (apply min ticks) (apply max ticks)))]
               [layout (λ [tmin tmax] (values ticks #false))])
           (cond [(and (list? labels) (pair? (filter values labels)))
                  (let* ([lbls (for/hasheqv : (HashTable Real (Option String)) ([t (in-list ticks)]
                                                                                [s (in-list labels)]
                                                                                #:when s)
                                 (values t s))]
                         [predefine-labels (lambda [[tval : Real] [para : Integer]]
                                             (or (hash-ref lbls tval (λ [] #false))
                                                 (plot-tick->label-string tval para)))])
                    (unsafe-plot-tick-engine layout predefine-labels rng minor-count 1.0))]
                 [(procedure? labels) (unsafe-plot-tick-engine layout labels rng minor-count 1.0)]
                 [else (unsafe-plot-tick-engine layout plot-tick->label-string rng minor-count 1.0)]))
         plot-no-ticks)]
    [(ticks minor-count) (plot-fixed-ticks* ticks minor-count plot-tick->label-string)]
    [(tmin tmax step minor-count) (plot-fixed-ticks* (range tmin (+ tmax step) step) minor-count plot-tick->label-string)]
    [(tmin tmax step minor-count format) (plot-fixed-ticks* (range tmin (+ tmax step) step) minor-count format)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-symbol-ticks : (case-> [(Listof Real) -> Plot-Tick-Engine]
                                    [(Listof Real) (Pairof Positive-Real String) -> Plot-Tick-Engine]
                                    [Real Real Real -> Plot-Tick-Engine]
                                    [Real Real Real (Pairof Positive-Real String) -> Plot-Tick-Engine])
  (case-lambda
    [(ticks) (plot-symbol-ticks* ticks 0)]
    [(ticks val.sym) (plot-symbol-ticks* ticks 0 val.sym)]
    [(tmin tmax step) (plot-symbol-ticks* (range tmin (+ tmax step) step) 0)]
    [(tmin tmax step val.sym) (plot-symbol-ticks* (range tmin (+ tmax step) step) 0 val.sym)]))

(define plot-symbol-ticks* : (case-> [(Listof Real) Plot-Minor-Tick-Count -> Plot-Tick-Engine]
                                     [(Listof Real) Plot-Minor-Tick-Count (Pairof Positive-Real String) -> Plot-Tick-Engine]
                                     [Real Real Real Plot-Minor-Tick-Count -> Plot-Tick-Engine]
                                     [Real Real Real Plot-Minor-Tick-Count (Pairof Positive-Real String) -> Plot-Tick-Engine])
  (case-lambda
    [(ticks minor-count) (plot-symbol-ticks* ticks minor-count (cons pi "π"))]
    [(ticks minor-count val.sym)
     (define mlp (car val.sym))
     (define vals (map (λ [[r : Real]] (* r mlp)) ticks))
     
     (unsafe-plot-tick-engine (λ [tmin tmax] (values vals #false))
                              (plot-symbol-tick->label-string (cdr val.sym)
                                                              (for/hasheqv : (HashTable Real Real) ([t (in-list ticks)]
                                                                                                    [v (in-list vals)])
                                                                (values v t)))
                              (cons (apply min vals) (apply max vals))
                              minor-count
                              mlp)]
    [(tmin tmax step minor-count) (plot-symbol-ticks* (range tmin (+ tmax step) step) minor-count)]
    [(tmin tmax step minor-count val.sym) (plot-symbol-ticks* (range tmin (+ tmax step) step) minor-count val.sym)]))

(define plot-single-ticks : (->* (Real) ((Option String)) Plot-Tick-Engine)
  (lambda [tick [label #false]]
    (plot-fixed-ticks (list tick) (list label))))

(define plot-interval-ticks : (->* (Real) ((Option Plot-Tick-Format) #:minor-count Plot-Minor-Tick-Count) Plot-Tick-Engine)
  (lambda [step [format #false] #:minor-count [minor-counts 0]]
    (unsafe-plot-tick-engine
     (λ [[tmin : Real] [tmax : Real]]
       (if (positive? step)
           (values (range tmin (+ tmax (* step 0.5)) step) step)
           (values (list tmin) #false)))
     (or format plot-tick->label-string)
     #false
     minor-counts
     1.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-no-ticks : Plot-Tick-Engine
  (unsafe-plot-tick-engine
   (λ [[tmin : Real] [tmax : Real]] (values null #false))
   plot-tick->label-string
   #false
   0
   1.0))
