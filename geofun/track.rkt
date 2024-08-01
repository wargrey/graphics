#lang typed/racket/base

(provide (all-defined-out))
(provide Track Dryland-Wani)
(provide track? dryland-wani?)
(provide track-close)

(provide
 (rename-out [dryland-wani-step-up-right! dryland-wani-step-right-up!]
             [dryland-wani-step-right-down! dryland-wani-step-down-right!]
             [dryland-wani-step-down-left! dryland-wani-step-left-down!]
             [dryland-wani-step-left-up! dryland-wani-step-up-left!])

 (rename-out [dryland-wani-jump-up-right! dryland-wani-jump-right-up!]
             [dryland-wani-jump-right-down! dryland-wani-jump-down-right!]
             [dryland-wani-jump-down-left! dryland-wani-jump-left-down!]
             [dryland-wani-jump-left-up! dryland-wani-jump-up-left!])

 (rename-out [track-close dryland-wani-close!]))

(require "digitama/bbox.rkt")
(require "digitama/anchor.rkt")
(require "digitama/track.rkt")
(require "digitama/unsafe/convert.rkt")

(require bitmap/digitama/dot)

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
                                 (Real #:turn-scale Track-Print-Datum #:U-scale Track-Print-Datum
                                       #:anchor Keyword #:at Track-Print-Datum #:id Geo-Unique-Identifier)
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
    (define-values (tsx tsy) (track-turn-scales t-scale 0.5))
    (define-values (usx usy) (track-turn-scales u-scale 0.25))
    
    (create-geometry-object dryland-wani #:with track-surface #:id (or name (gensym 'track))
                            (list (cons start-of-track home-pos)) (make-geo-path home-pos)
                            home-pos home-pos (make-geo-bbox home-pos)
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

(define dryland-wani-drift! : (->* (Dryland-Wani Track-Bezier-Datum (Listof Track-Bezier-Datum)) ((Option Track-Anchor)) Void)
  (lambda [wani end-step ctrl-steps [anchor #false]]
    (define xsize : Flonum (dryland-wani-xstepsize wani))
    (define ysize : Flonum (dryland-wani-ystepsize wani))
    (define endpt : Float-Complex (track-bezier-point wani end-step xsize ysize))
    (define ctrls : (Listof Float-Complex)
      (for/list : (Listof Float-Complex) ([ctrl (in-list ctrl-steps)])
        (track-bezier-point wani ctrl xsize ysize)))

    (cond [(null? ctrls) (track-linear-bezier wani endpt anchor)]
          [(null? (cdr ctrls)) (track-quadratic-bezier wani endpt (car ctrls) anchor)]
          [else (track-cubic-bezier wani endpt (car ctrls) (cadr ctrls) anchor)])))

(define dryland-wani-step-to! : (-> Dryland-Wani Track-Anchor Void)
  (lambda [wani target]
    (track-connect-to wani target)))

(define dryland-wani-jump-back! : (->* (Dryland-Wani) ((Option Track-Anchor)) Void)
  (lambda [wani [target #false]]
    (track-jump-to wani target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define track-anchor-position : (->* (Track Track-Anchor) (#:translate? Boolean) Float-Complex)
  (lambda [self anchor #:translate? [translate? #false]]
    (define abspos : Float-Complex (geo-path-ref (track-path self) anchor))
    
    (cond [(not translate?) abspos]
          [else (- abspos (geo-bbox-position (track-bbox self)))])))

