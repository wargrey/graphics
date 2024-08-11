#lang typed/racket/base

(provide (all-defined-out))

(require "stroke.rkt")
(require "color.rkt")

(require "digitama/color.rkt")
(require "digitama/composite.rkt")

(require "digitama/unsafe/source.rkt")
(require "digitama/unsafe/visual/object.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Stroke-Paint (U Color Stroke))
(define-type Fill-Paint (U Color Visual-Object<%> Fill-Pattern))

(define-type Maybe-Stroke-Paint (U Stroke-Paint False Void))
(define-type Maybe-Fill-Paint (U Fill-Paint Void False))

(define default-stroke-paint : (Parameterof Maybe-Stroke-Paint) (make-parameter (void)))
(define default-border-paint : (Parameterof Maybe-Stroke-Paint) (make-parameter (void)))
(define default-foreground-paint : (Parameterof Fill-Paint) (make-parameter black))
(define default-background-paint : (Parameterof Maybe-Fill-Paint) (make-parameter (void)))
(define default-fill-paint : (Parameterof Maybe-Fill-Paint) (make-parameter (void)))
(define default-fill-rule : (Parameterof Symbol) (make-parameter 'winding))
(define default-pin-operator : (Parameterof Geo-Pin-Operator) (make-parameter 'over))
