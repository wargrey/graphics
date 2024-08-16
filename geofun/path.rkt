#lang typed/racket/base

(provide (all-defined-out))

(provide Geo-Path Dryland-Wani)
(provide Geo-Sticker Geo-Anchor->Sticker)
(provide geo-path? dryland-wani?)
(provide geo-path-close geo-path-stick)
(provide default-track-anchor->sticker)

(provide
 (rename-out [dryland-wani-step-up-right! dryland-wani-step-right-up!]
             [dryland-wani-step-right-down! dryland-wani-step-down-right!]
             [dryland-wani-step-down-left! dryland-wani-step-left-down!]
             [dryland-wani-step-left-up! dryland-wani-step-up-left!])

 (rename-out [dryland-wani-jump-up-right! dryland-wani-jump-right-up!]
             [dryland-wani-jump-right-down! dryland-wani-jump-down-right!]
             [dryland-wani-jump-down-left! dryland-wani-jump-left-down!]
             [dryland-wani-jump-left-up! dryland-wani-jump-up-left!])

 (rename-out [geo-path-close dryland-wani-close!]))

(require "digitama/dot.rkt")
(require "digitama/bbox.rkt")
(require "digitama/trail.rkt")
(require "digitama/convert.rkt")
(require "digitama/dc/path.rkt")
(require "digitama/layer/sticker.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-dryland-wani! stx)
  (syntax-case stx []
    [(_ name [args ...] #:- move-expr ...)
     (syntax/loc stx
       (define name : Dryland-Wani
         (with-dryland-wani! (make-dryland-wani args ...)
           move-expr ...)))]))

(define-syntax (with-dryland-wani! stx)
  (syntax-case stx []
    [(_ wani (move argl ...) ...)
     (with-syntax* ([(dryland-wani-move ...)
                     (for/list ([<move> (in-list (syntax->list #'(move ...)))])
                       (format-id <move> "dryland-wani-~a!" (syntax->datum <move>)))])
       (syntax/loc stx
         (let ([self wani])
           (dryland-wani-move self argl ...)
           ...
           self)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dryland-wani : (->* (Real)
                                 (Real #:turn-scale Geo-Print-Datum #:U-scale Geo-Print-Datum
                                       #:anchor Keyword #:at Geo-Print-Datum #:id (Option Symbol))
                                 Dryland-Wani)
  (lambda [#:turn-scale [t-scale +nan.0] #:U-scale [u-scale +nan.0]
           #:anchor [anchor '#:home] #:at [home 0] #:id [name #false]
           xstepsize [ystepsize 0.0]]
    (define xstep : Nonnegative-Flonum
      (cond [(<= xstepsize 0.0) 1.0]
            [else (max (real->double-flonum xstepsize) 0.0)]))
    
    (define ystep : Nonnegative-Flonum
      (cond [(= ystepsize 0.0) xstep]
            [(< ystepsize 0.0) (* xstep (- (real->double-flonum ystepsize)))]
            [else (max (real->double-flonum ystepsize) 0.0)]))
    
    (define home-pos : Float-Complex (~point2d home))
    (define-values (tsx tsy) (geo-path-turn-scales t-scale 0.5))
    (define-values (usx usy) (geo-path-turn-scales u-scale 0.25))
    
    (create-geometry-object dryland-wani #:with [geo-path-surface (geo-path-bbox-wrapper geo-path-bounding-box)] #:id name
                            (list (cons start-of-track home-pos)) (make-geo-trail home-pos anchor)
                            (make-geo-bbox home-pos) home-pos home-pos
                            xstep ystep (* tsx xstep) (* tsy ystep) (* usx xstep) (* usy ystep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani-line-move! left            #:-> -1.0)
(define-dryland-wani-line-move! right           #:->  1.0)
(define-dryland-wani-line-move! up              #:!> -1.0)
(define-dryland-wani-line-move! down            #:!>  1.0)
(define-dryland-wani-line-move! up-right        #:+>  1.0-1.0i)
(define-dryland-wani-line-move! right-down      #:+>  1.0+1.0i)
(define-dryland-wani-line-move! down-left       #:+> -1.0+1.0i)
(define-dryland-wani-line-move! left-up         #:+> -1.0-1.0i)

(define-dryland-wani-turn-move! up-right-down   #:+> [180.0 360.0  1.0  0.0  2.0  0.0] #:boundary-guard -1.0i)
(define-dryland-wani-turn-move! up-left-down    #:-> [360.0 180.0 -1.0  0.0 -2.0  0.0] #:boundary-guard -1.0i)
(define-dryland-wani-turn-move! right-down-left #:+> [-90.0  90.0  0.0  1.0  0.0  2.0] #:boundary-guard 1.0)
(define-dryland-wani-turn-move! right-up-left   #:-> [ 90.0 -90.0  0.0 -1.0  0.0 -2.0] #:boundary-guard 1.0)
(define-dryland-wani-turn-move! down-left-up    #:+> [  0.0 180.0 -1.0  0.0 -2.0  0.0] #:boundary-guard +1.0i)
(define-dryland-wani-turn-move! down-right-up   #:-> [180.0   0.0  1.0  0.0  2.0  0.0] #:boundary-guard +1.0i)
(define-dryland-wani-turn-move! left-up-right   #:+> [ 90.0 270.0  0.0 -1.0  0.0 -2.0] #:boundary-guard -1.0)
(define-dryland-wani-turn-move! left-down-right #:-> [270.0  90.0  0.0  1.0  0.0  2.0] #:boundary-guard -1.0)

(define-dryland-wani-turn-move! up right
  #:+> [180.0 270.0 1.0  0.0 1.0 -1.0]
  #:-> [ 90.0   0.0 0.0 -1.0 1.0 -1.0])

(define-dryland-wani-turn-move! right down
  #:+> [270.0 360.0 0.0 1.0 1.0 1.0]
  #:-> [180.0  90.0 1.0 0.0 1.0 1.0])

(define-dryland-wani-turn-move! down left
  #:+> [  0.0  90.0 -1.0 0.0 -1.0 1.0]
  #:-> [270.0 180.0  0.0 1.0 -1.0 1.0])

(define-dryland-wani-turn-move! left up
  #:+> [ 90.0 180.0  0.0 -1.0 -1.0 -1.0]
  #:-> [360.0 270.0 -1.0  0.0 -1.0 -1.0])

(define dryland-wani-drift! : (->* (Dryland-Wani Geo-Bezier-Datum (Listof Geo-Bezier-Datum)) ((Option Geo-Anchor-Name)) Void)
  (lambda [wani end-step ctrl-steps [anchor #false]]
    (define xsize : Flonum (dryland-wani-xstepsize wani))
    (define ysize : Flonum (dryland-wani-ystepsize wani))
    (define endpt : Float-Complex (geo-path-bezier-point wani end-step xsize ysize))
    (define ctrls : (Listof Float-Complex)
      (for/list : (Listof Float-Complex) ([ctrl (in-list ctrl-steps)])
        (geo-path-bezier-point wani ctrl xsize ysize)))

    (cond [(null? ctrls) (geo-path-linear-bezier wani endpt anchor)]
          [(null? (cdr ctrls)) (geo-path-quadratic-bezier wani endpt (car ctrls) anchor)]
          [else (geo-path-cubic-bezier wani endpt (car ctrls) (cadr ctrls) anchor)])))

(define dryland-wani-step-to! : (-> Dryland-Wani Geo-Anchor-Name Void)
  (lambda [wani target]
    (geo-path-connect-to wani target)))

(define dryland-wani-jump-back! : (->* (Dryland-Wani) ((Option Geo-Anchor-Name)) Void)
  (lambda [wani [target #false]]
    (geo-path-jump-to wani target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-anchor-position : (->* (Geo-Path Geo-Anchor-Name) (#:translate? Boolean) Float-Complex)
  (lambda [self anchor #:translate? [translate? #false]]
    (define abspos : Float-Complex (geo-trail-ref (geo-path-trail self) anchor))
    
    (cond [(not translate?) abspos]
          [else (- abspos (geo-bbox-position (geo-path-bbox self)))])))

