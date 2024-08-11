#lang typed/racket/base

(provide (all-defined-out))

(require "../bbox.rkt")
(require "../anchor.rkt")
(require "../source.rkt")
(require "../convert.rkt")

(require "../unsafe/path.rkt")

(require "../../paint.rkt")
(require "../../stroke.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Flow-Chart-Edge (Vector Geo-Path-Anchor-Name (Pairof Geo-Path-Anchor-Name (Listof Geo-Path-Anchor-Name)) Boolean))

(struct flow-chart geo
  ([edges : (Listof Flow-Chart-Edge)]
   [nodes : Geo-Path]
   [bbox : Geo-BBox]
   [here : Geo-Path-Anchor-Name])
  #:type-name Flow-Chart
  #:transparent
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-culumon-flow! stx)
  (syntax-case stx []
    [(_ flow #:-> xsgn)
     (with-syntax ([di-flow! (format-id #'flow "culumon:~a>!" (syntax->datum #'flow))]
                   [ud-flow! (format-id #'flow "culumon:~a-!" (syntax->datum #'flow))])
       (syntax/loc stx
         (begin (define (di-flow! [self : Culumon] [step : Integer 1] . [nodes : Geo-Path-Anchor-Name *]) : Void
                  (flow-chart-flow self (* step xsgn) 0 #true nodes))
                
                (define (ud-flow! [self : Culumon] [step : Integer 1] . [nodes : Geo-Path-Anchor-Name *]) : Void
                  (flow-chart-flow self (* step xsgn) 0 #false nodes)))))]
    [(_ flow #:!> ysgn)
     (with-syntax ([di-flow! (format-id #'flow "culumon:~a>!" (syntax->datum #'flow))]
                   [ud-flow! (format-id #'flow "culumon:~a-!" (syntax->datum #'flow))])
       (syntax/loc stx
         (begin (define (di-flow! [self : Culumon] [step : Integer 1] . [nodes : Geo-Path-Anchor-Name *]) : Void
                  (flow-chart-flow self 0 (* step ysgn) #true nodes))
                
                (define (ud-flow! [self : Culumon] [step : Integer 1] . [nodes : Geo-Path-Anchor-Name *]) : Void
                  (flow-chart-flow self 0 (* step ysgn) #false nodes)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct culumon flow-chart
  ([xstepsize : Nonnegative-Flonum]
   [ystepsize : Nonnegative-Flonum])
  #:type-name Culumon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow-chart-flow : (-> Flow-Chart Integer Integer Boolean (Listof Geo-Path-Anchor-Name) Void)
  (lambda [self dx dy direct? maybe-nodes]
    (define nodes (if (null? maybe-nodes) (list (gensym 'node)) maybe-nodes))
    (define dltpt (make-rectangular (real->double-flonum dx) (real->double-flonum dy)))
    (define sttpt (geo-path-ref (flow-chart-nodes self) (flow-chart-here self)))
    
    (set-flow-chart-edges! self (cons (vector (flow-chart-here self) nodes direct?) (flow-chart-edges self)))
    
    (let path-add ([pt : Float-Complex (+ sttpt dltpt)]
                   [all : (Pairof Geo-Path-Anchor-Name (Listof Geo-Path-Anchor-Name)) nodes])
      (define-values (node rest) (values (car all) (cdr all)))
      
      (geo-path-set! (flow-chart-nodes self) node pt)
      (if (pair? rest)
          (path-add (+ pt dltpt) rest)
          (begin (geo-bbox-fit! (flow-chart-bbox self) pt)
                 (set-flow-chart-here! self node))))))

(define flow-chart-jump-to : (-> Flow-Chart (Option Geo-Path-Anchor-Name) Void)
  (lambda [self auto-anchor]
    (define anchor (or auto-anchor (geo-path-head-anchor (flow-chart-nodes self))))

    (geo-path-pop! (flow-chart-nodes self) anchor)
    
    (set-flow-chart-here! self anchor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow-chart-surface : Geo-Surface-Create
  (lambda [self [opt #false]]
    (with-asserts ([self flow-chart?])
      (define-values (xoff yoff) (geo-bbox-offset-values (flow-chart-bbox self)))
      (path_stamp null xoff yoff
                  (default-stroke) (fill-paint->source* (default-fill-paint)) (default-fill-rule)))))
