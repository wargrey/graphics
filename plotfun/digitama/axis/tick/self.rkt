#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/list)

(require digimon/sequence)

(require "../../arithmetics.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Plot-Ticks R) (Listof (Pairof (Plot-Tick R) String)))
(define-type (Plot-Ticks-Generate R) (-> (Pairof (∩ R Real) (∩ R Real)) (Plot-Ticks (∩ R Real))))
(define-type (Plot-Ticks-Layout R) (-> R R (Listof (∩ R Real))))
(define-type (Plot-Tick-Format R) (-> (∩ R Real) Integer String))
(define-type (Plot-Ticks-Layout* R Arg) (-> R R (Values (Listof Plot-Tick) Arg)))
(define-type (Plot-Ticks-Format* R Arg) (-> R R Plot-Tick Arg String))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct (R) plot-tick
  ([value : R])
  #:type-name Plot-Tick
  #:transparent)

(define #:forall (R) plot-tick-value* : (-> (U (Plot-Tick R) (Pairof (Plot-Tick R) String)) R)
  (lambda [self]
    (if (pair? self)
        (plot-tick-value* (car self))
        (plot-tick-value self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (R) plot-fixed-ticks-generate : (->* ((Listof (∩ R Real))) ((Option (Listof (Option String)))) (Plot-Ticks R))
  (lambda [ticks [labels #false]]
    (if (pair? ticks)
        (((inst plot-fixed-ticks R) ticks labels) (cons (car ticks) (last ticks)))
        null)))

(define #:forall (R) plot-ticks-generator : (->* ((Plot-Ticks-Layout R)) ((Option (Plot-Tick-Format R))) (Plot-Ticks-Generate R))
  (lambda [g [f0 #false]]
    (define f (or f0 plot-tick->label-string))
    
    (λ [tick-range]
      (define-values (tmin tmax) (values (car tick-range) (cdr tick-range)))
      (define precision : Integer (tick-precision tmin tmax))

      (for/list ([val (in-list (g tmin tmax))])
        (cons (plot-tick val) (f val precision))))))

(define #:forall (R Arg) plot-ticks-generator* : (-> (Plot-Ticks-Layout* R Arg) (Plot-Ticks-Format* R Arg) (Plot-Ticks-Generate R))
  (lambda [g f]
    (λ [tick-range]
      (define-values (tmin tmax) (values (car tick-range) (cdr tick-range)))
      (define-values (ticks para) (g tmin tmax))

      (for/list ([val (in-list ticks)])
        (cons val (f tmin tmax val para))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (R) plot-fixed-ticks : (->* ((Listof (∩ R Real))) ((Option (Listof (Option String)))) (Plot-Ticks-Generate R))
  (lambda [ticks [labels #false]]
    (define n : Index (length ticks))
    (define ls : (Vectorof (Option String)) (if (list? labels) (list->n:vector labels n #false) (make-vector n #false)))
    
    (λ [who-cares]
      (define-values (tmin tmax) (values (car who-cares) (cdr who-cares)))
      (define precision : Integer (tick-precision tmin tmax))

      (for/list ([t (in-list ticks)]
                 [l (in-vector ls)])
        (cons (plot-tick t)
              (or l (plot-tick->label-string t precision)))))))

(define #:forall (R) plot-single-ticks : (->* ((∩ R Real)) ((Option String)) (Plot-Ticks-Generate R))
  (lambda [tick [label #false]]
    ((inst plot-fixed-ticks R) (list tick) (list label))))

(define #:forall (R) plot-no-ticks : (Plot-Ticks-Generate R)
  (λ [who-cares]
    null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (R) plot-tick->label-string : (Plot-Tick-Format R)
  (lambda [tval para]
    (cond [(exact? tval) (number->string tval)]
          [(integer? tval) (number->string (inexact->exact tval))]
          [else (~r tval #:precision para)])))
