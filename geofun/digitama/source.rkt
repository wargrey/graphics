#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../paint.rkt"))
(provide (all-from-out "../color.rkt"))

(require "../paint.rkt")
(require "../color.rkt")

(require "base.rkt")
(require "unsafe/source.rkt")
(require "unsafe/visual/object.rkt")
(require "unsafe/visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Stroke-Paint (U Color Paint))
(define-type Fill-Paint (U Color Visual-Object<%> Fill-Pattern))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define stroke-paint->source : (-> Stroke-Paint Paint)
  (lambda [paint]
    (cond [(paint? paint) paint]
          [else (desc-stroke (default-stroke) #:color paint)])))

(define stroke-paint->source* : (-> (Option Stroke-Paint) (Option Paint))
  (lambda [paint]
    (and paint (stroke-paint->source paint))))

(define #:forall (S) fill-paint->source : (case-> [Fill-Paint -> Fill-Source]
                                                  [Fill-Paint S -> (U Fill-Source S)])
  (lambda [paint [fallback transparent]]
    (cond [(color? paint) (rgb* paint)]
          [(visual-object<%>? paint)
           (let ([maybe-surface (vobject-convert paint 'cairo-surface #false)])
             (cond [(cairo-surface? maybe-surface) maybe-surface]
                   [else fallback]))]
          [else paint])))

(define fill-paint->source* : (-> (Option Fill-Paint) (Option Fill-Source))
  (lambda [paint]
    (and paint
         (fill-paint->source paint #false))))
