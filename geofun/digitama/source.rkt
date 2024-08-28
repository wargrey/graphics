#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [fill-paint->source* background->source*]))

(require "../paint.rkt")
(require "../color.rkt")
(require "../stroke.rkt")

(require "base.rkt")
(require "unsafe/source.rkt")
(require "unsafe/visual/object.rkt")
(require "unsafe/visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke-paint->source : (-> Maybe-Stroke-Paint Stroke)
  (lambda [paint]
    (cond [(stroke? paint) paint]
          [(or (not paint) (void? paint)) (default-stroke)]
          [else (desc-stroke (default-stroke) #:color paint)])))

(define border-paint->source : (-> Maybe-Stroke-Paint Stroke)
  (lambda [paint]
    (cond [(stroke? paint) paint]
          [(or (not paint) (void? paint)) (default-border)]
          [else (desc-stroke (default-border) #:color paint)])))

(define stroke-paint->source* : (-> Maybe-Stroke-Paint (Option Stroke))
  (lambda [paint]
    (cond [(not paint) #false]
          [else (stroke-paint->source paint)])))

(define border-paint->source* : (-> Maybe-Stroke-Paint (Option Stroke))
  (lambda [paint]
    (cond [(not paint) #false]
          [else (border-paint->source paint)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) fill-paint->source : (case-> [Fill-Paint -> Fill-Source]
                                                  [Fill-Paint S -> (U Fill-Source S)])
  (lambda [paint [fallback transparent]]
    (cond [(color? paint) (rgb* paint)]
          [(visual-object<%>? paint)
           (let ([maybe-surface (vobject-convert paint 'cairo-surface #false)])
             (cond [(cairo-surface? maybe-surface) maybe-surface]
                   [else fallback]))]
          [else paint])))

(define font-paint->source : (-> Option-Fill-Paint Fill-Source)
  (lambda [paint]
    (fill-paint->source
     (if (not paint)
         (default-font-paint)
         paint))))

(define fill-paint->source* : (-> Option-Fill-Paint (Option Fill-Source))
  (lambda [paint]
    (cond [(not paint) #false]
          [else (fill-paint->source paint #false)])))
