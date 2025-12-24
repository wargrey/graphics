#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [fill-paint->source* background->source*]))

(require "self.rkt")
(require "../base.rkt")

(require "../../paint.rkt")
(require "../../stroke.rkt")
(require "../../fill.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke-paint->source : (->* (Maybe-Stroke-Paint) (Pen) Pen)
  (lambda [paint [base-stroke (default-stroke)]]
    (cond [(pen? paint) paint]
          [(or (not paint) (void? paint)) base-stroke]
          [else (desc-stroke base-stroke #:color paint)])))

(define border-paint->source : (-> Maybe-Stroke-Paint Pen)
  (lambda [paint]
    (stroke-paint->source paint (default-border))))

(define stroke-paint->source* : (-> Maybe-Stroke-Paint (Option Pen))
  (lambda [paint]
    (cond [(not paint) #false]
          [else (stroke-paint->source paint)])))

(define border-paint->source* : (-> Maybe-Stroke-Paint (Option Pen))
  (lambda [paint]
    (cond [(not paint) #false]
          [else (stroke-paint->source paint (default-border))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fill-paint->source : (->* (Maybe-Fill-Paint) (Brush) Brush)
  (lambda [paint [base-brush (default-winding-brush)]]
    (cond [(brush? paint) paint]
          [(or (not paint) (void? paint)) base-brush]
          [(color? paint) (desc-brush base-brush #:color paint)]
          [else (desc-brush base-brush #:pattern paint)])))

(define fill-paint->source* : (->* (Maybe-Fill-Paint) (Brush) (Option Brush))
  (lambda [paint [base-brush (default-winding-brush)]]
    (cond [(brush? paint) paint]
          [(not paint) #false]
          [else (fill-paint->source paint base-brush)])))

(define font-paint->source : (-> Option-Fill-Paint Brush)
  (lambda [paint]
    (fill-paint->source
     (if (not paint) (default-font-paint) paint)
     (default-winding-brush))))
