#lang typed/racket/base

(provide (all-defined-out))


(require "../convert.rkt")
(require "../unsafe/path.rkt")

(require "../geometry/bbox.rkt")
(require "../geometry/trail.rkt")

(require "../dc/paint.rkt")
(require "../../paint.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Flow-Chart-Edge (Vector Geo-Anchor-Name (Pairof Geo-Anchor-Name (Listof Geo-Anchor-Name)) Boolean))

(struct flow-chart geo
  ([edges : (Listof Flow-Chart-Edge)]
   [nodes : Geo-Trail]
   [bbox : Geo-BBox]
   [here : Geo-Anchor-Name])
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
         (begin (define (di-flow! [self : Culumon] [step : Integer 1] . [nodes : Geo-Anchor-Name *]) : Void
                  (flow-chart-flow self (* step xsgn) 0 #true nodes))
                
                (define (ud-flow! [self : Culumon] [step : Integer 1] . [nodes : Geo-Anchor-Name *]) : Void
                  (flow-chart-flow self (* step xsgn) 0 #false nodes)))))]
    [(_ flow #:!> ysgn)
     (with-syntax ([di-flow! (format-id #'flow "culumon:~a>!" (syntax->datum #'flow))]
                   [ud-flow! (format-id #'flow "culumon:~a-!" (syntax->datum #'flow))])
       (syntax/loc stx
         (begin (define (di-flow! [self : Culumon] [step : Integer 1] . [nodes : Geo-Anchor-Name *]) : Void
                  (flow-chart-flow self 0 (* step ysgn) #true nodes))
                
                (define (ud-flow! [self : Culumon] [step : Integer 1] . [nodes : Geo-Anchor-Name *]) : Void
                  (flow-chart-flow self 0 (* step ysgn) #false nodes)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct culumon flow-chart
  ([xstepsize : Nonnegative-Flonum]
   [ystepsize : Nonnegative-Flonum])
  #:type-name Culumon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow-chart-flow : (-> Flow-Chart Integer Integer Boolean (Listof Geo-Anchor-Name) Void)
  (lambda [self dx dy direct? maybe-nodes]
    (define nodes (if (null? maybe-nodes) (list (gensym 'node)) maybe-nodes))
    (define dltpt (make-rectangular (real->double-flonum dx) (real->double-flonum dy)))
    (define sttpt (geo-trail-ref (flow-chart-nodes self) (flow-chart-here self)))
    
    (set-flow-chart-edges! self (cons (vector (flow-chart-here self) nodes direct?) (flow-chart-edges self)))
    
    (let path-add ([pt : Float-Complex (+ sttpt dltpt)]
                   [all : (Pairof Geo-Anchor-Name (Listof Geo-Anchor-Name)) nodes])
      (define-values (node rest) (values (car all) (cdr all)))
      
      (geo-trail-set! (flow-chart-nodes self) node pt)
      (if (pair? rest)
          (path-add (+ pt dltpt) rest)
          (begin (geo-bbox-fit! (flow-chart-bbox self) pt)
                 (set-flow-chart-here! self node))))))

(define flow-chart-jump-to : (-> Flow-Chart (Option Geo-Anchor-Name) Void)
  (lambda [self auto-anchor]
    (define anchor (or auto-anchor (geo-trail-head-anchor (flow-chart-nodes self))))

    (geo-trail-pop! (flow-chart-nodes self) anchor)
    (set-flow-chart-here! self anchor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow-chart-surface : Geo-Surface-Create
  (lambda [self [opt #false]]
    (with-asserts ([self flow-chart?])
      (define-values (xoff yoff) (geo-bbox-offset-values (flow-chart-bbox self)))
      (path_stamp null xoff yoff
                  (current-stroke-source) (current-fill-source) (default-fill-rule)
                  (default-geometry-density)))))
