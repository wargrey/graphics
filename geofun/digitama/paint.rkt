#lang typed/racket/base

(provide (all-defined-out))

(require "../paint.rkt")
(require "../fill.rkt")

(require "paint/self.rkt")
(require "paint/source.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Maybe-Stroke-Source (U Pen False Void))
(define-type Maybe-Fill-Source (U Brush False Void))

;;; These parameters are used as the hooks to configure the renderer.
(define default-stroke-source : (Parameterof Maybe-Stroke-Source) (make-parameter (void)))
(define default-border-source : (Parameterof Maybe-Stroke-Source) (make-parameter (void)))
(define default-background-source : (Parameterof Maybe-Fill-Source) (make-parameter (void)))
(define default-fill-source : (Parameterof Maybe-Fill-Source) (make-parameter (void)))
(define default-font-source : (Parameterof (Option Brush)) (make-parameter #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-stroke-source* : (-> Pen)
  (lambda []
    (define src (default-stroke-source))
    (cond [(pen? src) src]
          [else (stroke-paint->source (default-stroke-paint))])))

(define current-stroke-source : (-> (Option Pen))
  (lambda []
    (define src (default-stroke-source))
    (if (void? src) (stroke-paint->source* (default-stroke-paint)) src)))

(define current-border-source : (-> (Option Pen))
  (lambda []
    (define src (default-border-source))
    (if (void? src) (border-paint->source* (default-border-paint)) src)))

(define current-background-source : (-> Brush (Option Brush))
  (lambda [base-brush]
    (define src (default-background-source))
    (if (void? src) (fill-paint->source* (default-background-paint) base-brush) src)))

(define current-fill-source : (-> Brush (Option Brush))
  (lambda [base-brush]
    (define src (default-fill-source))
    (if (void? src) (fill-paint->source* (default-fill-paint) base-brush) src)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-select-font-source : (-> Option-Fill-Paint Brush)
  (lambda [alt-fg]
    (cond [(and alt-fg) (font-paint->source alt-fg)]
          [else (or (default-font-source)
                    (font-paint->source #false))])))

(define geo-select-background-source : (->* (Maybe-Fill-Paint) (Brush) (Option Brush))
  (lambda [alt-bg [base-brush (default-winding-brush)]]
    (if (void? alt-bg)
        (current-background-source base-brush)
        (fill-paint->source* alt-bg base-brush))))

(define geo-select-fill-source : (->* (Maybe-Fill-Paint) (Brush) (Option Brush))
  (lambda [alt-fill [base-brush (default-winding-brush)]]
    (if (void? alt-fill)
        (current-fill-source base-brush)
        (fill-paint->source* alt-fill base-brush))))

(define geo-select-border-paint : (-> Maybe-Stroke-Paint (Option Pen))
  (lambda [alt-bdr]
    (if (void? alt-bdr)
        (current-border-source)
        (border-paint->source* alt-bdr))))

(define geo-select-stroke-paint : (-> Maybe-Stroke-Paint (Option Pen))
  (lambda [alt-strk]
    (if (void? alt-strk)
        (current-stroke-source)
        (stroke-paint->source* alt-strk))))

(define geo-select-stroke-paint* : (-> Maybe-Stroke-Paint Pen)
  (lambda [alt-strk]
    (if (void? alt-strk)
        (current-stroke-source*)
        (stroke-paint->source alt-strk))))
