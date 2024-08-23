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
    (cond [(or (void? src) (not src)) (stroke-paint->source (default-stroke-paint))]
          [else src])))

(define current-stroke-source : (-> (Option Stroke))
  (lambda []
    (define src (default-stroke-source))
    (if (void? src) (stroke-paint->source* (default-stroke-paint)) src)))

(define current-border-source : (-> (Option Stroke))
  (lambda []
    (define src (default-border-source))
    (if (void? src) (border-paint->source* (default-border-paint)) src)))

(define current-background-source : (-> (Option Fill-Source))
  (lambda []
    (define src (default-background-source))
    (if (void? src) (fill-paint->source* (default-background-paint)) src)))

(define current-fill-source : (-> (Option Fill-Source))
  (lambda []
    (define src (default-fill-source))
    (if (void? src) (fill-paint->source* (default-fill-paint)) src)))

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

(define geo-select-fill : (-> Maybe-Fill-Paint (Option Fill-Source))
  (lambda [alt-fill]
    (if (void? alt-fill)
        (current-fill-source)
        (fill-paint->source* alt-fill))))

(define geo-select-border-paint : (-> Maybe-Stroke-Paint (Option Stroke))
  (lambda [alt-bdr]
    (if (void? alt-bdr)
        (current-border-source)
        (border-paint->source* alt-bdr))))

(define geo-select-stroke-paint : (-> Maybe-Stroke-Paint (Option Stroke))
  (lambda [alt-strk]
    (if (void? alt-strk)
        (current-stroke-source)
        (stroke-paint->source* alt-strk))))
