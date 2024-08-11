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

(define-type Option-Stroke-Paint (Option Stroke-Paint))
(define-type Option-Fill-Paint (Option Fill-Paint))

(define-type Maybe-Stroke-Paint (U Option-Stroke-Paint Void))
(define-type Maybe-Fill-Paint (U Option-Fill-Paint Void))

(define default-stroke-paint : (Parameterof Option-Stroke-Paint) (make-parameter #false))
(define default-border-paint : (Parameterof Option-Stroke-Paint) (make-parameter #false))
(define default-foreground-paint : (Parameterof Fill-Paint) (make-parameter black))
(define default-background-paint : (Parameterof Option-Fill-Paint) (make-parameter #false))
(define default-fill-paint : (Parameterof Option-Fill-Paint) (make-parameter #false))
(define default-fill-rule : (Parameterof Symbol) (make-parameter 'winding))
(define default-pin-operator : (Parameterof Geo-Pin-Operator) (make-parameter 'over))
