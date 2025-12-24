#lang typed/racket/base

(provide (all-defined-out))
(provide Geo-Pin-Operator default-pin-operator)
(provide Geo-Pattern-Extend Geo-Pattern-Filter)
(provide Pen Stroke-Paint Option-Stroke-Paint Maybe-Stroke-Paint)
(provide Brush Fill-Paint Option-Fill-Paint Maybe-Fill-Paint)

(require "digitama/color.rkt")
(require "digitama/composite.rkt")
(require "digitama/paint/self.rkt")
(require "digitama/paint/pattern.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These parameters are used as the default arguments of visual-objects' constructors
(define default-background-paint : (Parameterof Option-Fill-Paint) (make-parameter #false))
(define default-stroke-paint : (Parameterof Maybe-Stroke-Paint) (make-parameter (void)))
(define default-border-paint : (Parameterof Maybe-Stroke-Paint) (make-parameter (void)))
(define default-font-paint : (Parameterof Fill-Paint) (make-parameter black))
(define default-fill-paint : (Parameterof Option-Fill-Paint) (make-parameter #false))
(define default-pattern-filter : (Parameterof Geo-Pattern-Filter) (make-parameter 'bilinear))
(define default-pattern-extend : (Parameterof (Option Geo-Pattern-Extend)) (make-parameter #false))
