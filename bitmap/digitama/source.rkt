#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../paint.rkt"))
(provide (all-from-out "../color.rkt"))

(require "base.rkt")
(require "unsafe/source.rkt")
(require "unsafe/convert.rkt")
(require "../paint.rkt")
(require "../color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Stroke-Paint (U Color Paint))
(define-type Fill-Paint (U Color Bitmap Bitmap-Pattern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke-paint->source : (-> Stroke-Paint Paint)
  (lambda [paint]
    (cond [(paint? paint) paint]
          [else (desc-stroke (default-stroke) #:color paint)])))

(define stroke-paint->source* : (-> (Option Stroke-Paint) (Option Paint))
  (lambda [paint]
    (and paint (stroke-paint->source paint))))

(define fill-paint->source : (-> Fill-Paint Bitmap-Source)
  (lambda [paint]
    (cond [(bitmap? paint) (bitmap-surface paint)]
          [(color? paint) (rgb* paint)]
          [else paint])))

(define fill-paint->source* : (-> (Option Fill-Paint) (Option Bitmap-Source))
  (lambda [paint]
    (and paint (fill-paint->source paint))))
