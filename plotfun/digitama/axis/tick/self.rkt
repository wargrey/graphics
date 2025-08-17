#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/list)
(require racket/math)

(require "../../arithmetics.rkt")

(require (for-syntax racket/base))

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

(define-type Plot-Fixed-Tick-Format (U False Plot-Tick-Format (Listof (Option String))))

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
(define plot-tick->label-string : Plot-Tick-Format
  (lambda [tval para]
    (cond [(exact? tval) (number->string tval)]
          [(integer? tval) (number->string (inexact->exact tval))]
          [(>= para 0) (~r tval #:precision para)]
          [else (~a tval)])))

(define plot-symbol-tick->label-string : (-> String (HashTable Real Real) Plot-Tick-Format)
  (lambda [sym db]
    (define (symbol-tick->string [tval0 : Real] [para : Integer]) : String
      (define tval (hash-ref db tval0 (λ [] tval0)))
      
      (cond [(zero? tval) (number->string tval)]
            [(integer? tval)
             (cond [(= tval 1) sym]
                   [(= tval -1) (~a #\- sym)]
                   [else (~a (inexact->exact tval) sym)])]
            [(exact? tval) (~a (symbol-tick->string (numerator tval) para) #\/ (denominator tval))]
            [(>= para 0) (~r tval #:precision para)]
            [else (~a tval sym)]))

    symbol-tick->string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-no-ticks : Plot-Tick-Engine
  (unsafe-plot-tick-engine
   (λ [[tmin : Real] [tmax : Real]] (values null #false))
   plot-tick->label-string
   #false
   0
   1.0))
