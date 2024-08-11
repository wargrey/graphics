#lang typed/racket/base

(provide (all-defined-out))

(require "../../paint.rkt")
(require "../../stroke.rkt")

(require "../source.rkt")
(require "../unsafe/source.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Maybe-Stroke-Source (U Stroke False Void))
(define-type Maybe-Fill-Source (U Fill-Source False Void))

(define default-stroke-source : (Parameterof Maybe-Stroke-Source) (make-parameter (void)))
(define default-border-source : (Parameterof Maybe-Stroke-Source) (make-parameter (void)))
(define default-background-source : (Parameterof Maybe-Fill-Source) (make-parameter (void)))
(define default-fill-source : (Parameterof Maybe-Fill-Source) (make-parameter (void)))

(define default-foreground-source : (Parameterof (Option Fill-Source)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-stroke-source* : (-> Stroke)
  (lambda []
    (define src (default-stroke-source))
    (cond [(void? src) (default-stroke)]
          [(not src) (default-stroke)]
          [else src])))

(define current-stroke-source : (-> (Option Stroke))
  (lambda []
    (define src (default-stroke-source))
    (if (void? src) (default-stroke) src)))

(define current-border-source : (-> (Option Stroke))
  (lambda []
    (define src (default-border-source))
    (if (void? src) (default-border) src)))

(define current-background-source : (-> (Option Fill-Source))
  (lambda []
    (define src (default-background-source))
    (if (void? src) #false src)))

(define current-fill-source : (-> (Option Fill-Source))
  (lambda []
    (define src (default-fill-source))
    (if (void? src) #false src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-select-foreground : (-> Option-Fill-Paint Fill-Source)
  (lambda [alt-fg]
    (cond [(and alt-fg) (foreground->source alt-fg)]
          [else (let ([fg (default-foreground-source)])
                  (or fg (foreground->source #false)))])))

(define geo-select-background : (-> Maybe-Fill-Paint (Option Fill-Source))
  (lambda [alt-bg]
    (if (void? alt-bg)
        (current-background-source)
        (fill-paint->source* alt-bg))))
