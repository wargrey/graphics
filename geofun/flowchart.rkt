#lang typed/racket/base

(provide (all-defined-out))
(provide Flow-Chart Culumon)
(provide flow-chart? culumon?)

(require "digitama/dot.rkt")
(require "digitama/bbox.rkt")
(require "digitama/trail.rkt")
(require "digitama/convert.rkt")

(require "digitama/diagram/flowchart.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-culumon! stx)
  (syntax-case stx []
    [(_ name [args ...] #:- flow-expr ...)
     (syntax/loc stx
       (define name : Culumon
         (with-culumon! (make-culumon args ...)
           flow-expr ...)))]))

(define-syntax (with-culumon! stx)
  (syntax-case stx []
    [(_ wani (flow argl ...) ...)
     (with-syntax* ([(culumon-flow ...)
                     (for/list ([<flow> (in-list (syntax->list #'(flow ...)))])
                       (format-id <flow> "culumon:~a!" (syntax->datum <flow>)))])
       (syntax/loc stx
         (let ([self wani])
           (culumon-flow self argl ...)
           ...
           self)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-culumon : (->* (Real) (Real #:start Keyword #:at Point2D #:id (Option Symbol)) Culumon)
  (lambda [#:start [start '#:start] #:at [home 0] #:id [name #false]
           xstepsize [ystepsize 0.0]]
    (define xstep : Nonnegative-Flonum
      (cond [(<= xstepsize 0.0) 1.0]
            [else (max (real->double-flonum xstepsize) 0.0)]))
    
    (define ystep : Nonnegative-Flonum
      (cond [(= ystepsize 0.0) xstep]
            [(< ystepsize 0.0) (* xstep (- (real->double-flonum ystepsize)))]
            [else (max (real->double-flonum ystepsize) 0.0)]))
    
    (define home-pos : Float-Complex (~point2d home))
    
    (create-geometry-object culumon #:with flow-chart-surface #:id name
                            null (make-geo-trail home-pos start)
                            (make-geo-bbox home-pos) start
                            xstep ystep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-culumon-flow! l- #:-> -1)
(define-culumon-flow! r- #:->  1)
(define-culumon-flow! u- #:!> -1)
(define-culumon-flow! d- #:!>  1)

(define culumon-jump-back! : (->* (Culumon) ((Option Geo-Anchor-Name)) Void)
  (lambda [wani [target #false]]
    (flow-chart-jump-to wani target)))
