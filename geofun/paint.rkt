#lang typed/racket/base

(provide (all-defined-out) Fill-Rule Geo-Pin-Operator Geo-Pattern-Filter)

(require "stroke.rkt")
(require "color.rkt")

(require "digitama/color.rkt")
(require "digitama/pattern.rkt")
(require "digitama/composite.rkt")

(require "digitama/unsafe/source.rkt")
(require "digitama/unsafe/visual.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Stroke-Paint (U Color Stroke))
(define-type Fill-Paint (U Color Visual-Object<%> Fill-Pattern))

(define-type Option-Stroke-Paint (Option Stroke-Paint))
(define-type Option-Fill-Paint (Option Fill-Paint))

(define-type Maybe-Stroke-Paint (U Option-Stroke-Paint Void))
(define-type Maybe-Fill-Paint (U Option-Fill-Paint Void))

(define default-background-paint : (Parameterof Option-Fill-Paint) (make-parameter #false))
(define default-stroke-paint : (Parameterof Maybe-Stroke-Paint) (make-parameter (void)))
(define default-border-paint : (Parameterof Maybe-Stroke-Paint) (make-parameter (void)))
(define default-font-paint : (Parameterof Fill-Paint) (make-parameter black))
(define default-fill-paint : (Parameterof Option-Fill-Paint) (make-parameter #false))
(define default-fill-rule : (Parameterof Fill-Rule) (make-parameter 'winding))

(define default-pin-operator : (Parameterof Geo-Pin-Operator) (make-parameter 'over))
(define default-pattern-filter : (Parameterof Geo-Pattern-Filter) (make-parameter 'bilinear))
(define default-pattern-extend : (Parameterof (Option Geo-Pattern-Extend)) (make-parameter #false))
