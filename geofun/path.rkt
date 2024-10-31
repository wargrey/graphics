#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/geometry/anchor.rkt"))

(provide Geo:Path Gomamon)
(provide Geo-Sticker Geo-Anchor->Sticker)
(provide geo:path? gomamon? geo-sticker?)
(provide make-sticker default-anchor->sticker)
(provide geo-path-close)

(provide
 (rename-out [gomamon-move-up-right! gomamon-move-right-up!]
             [gomamon-move-right-down! gomamon-move-down-right!]
             [gomamon-move-down-left! gomamon-move-left-down!]
             [gomamon-move-left-up! gomamon-move-up-left!])

 (rename-out [gomamon-jump-up-right! gomamon-jump-right-up!]
             [gomamon-jump-right-down! gomamon-jump-down-right!]
             [gomamon-jump-down-left! gomamon-jump-left-down!]
             [gomamon-jump-left-up! gomamon-jump-up-left!])

 (rename-out [geo-path-close gomamon-close!]
             [make-sticker make-geo-sticker]))

(require "paint.rkt")
(require "stroke.rkt")

(require "digitama/convert.rkt")
(require "digitama/geometry/dot.rkt")
(require "digitama/geometry/bbox.rkt")
(require "digitama/geometry/trail.rkt")
(require "digitama/geometry/anchor.rkt")
(require "digitama/geometry/footprint.rkt")

(require "digitama/dc/path.rkt")
(require "digitama/dc/composite.rkt")
(require "digitama/layer/sticker.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-gomamon! stx)
  (syntax-case stx []
    [(_ name [args ...] #:- move-expr ...)
     (syntax/loc stx
       (define name : Gomamon
         (with-gomamon! (make-gomamon args ...)
           move-expr ...)))]))

(define-syntax (with-gomamon! stx)
  (syntax-case stx []
    [(_ wani (move argl ...) ...)
     (with-syntax* ([(gomamon-move! ...)
                     (for/list ([<move> (in-list (syntax->list #'(move ...)))])
                       (format-id <move> "gomamon-~a!" (syntax->datum <move>)))])
       (quasisyntax/loc stx
         (let ([self wani])
           (gomamon-move! self argl ...)
           ...
           self)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-gomamon : (->* (Real)
                            (Real #:T-scale Geo-Print-Datum #:U-scale Geo-Print-Datum
                                  #:anchor Geo-Anchor-Name #:at Geo-Print-Datum #:id (Option Symbol)
                                  #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:fill-rule Fill-Rule)
                            Gomamon)
  (lambda [#:T-scale [t-scale +nan.0] #:U-scale [u-scale +nan.0]
           #:anchor [anchor '#:home] #:at [home 0] #:id [name #false]
           #:stroke [stroke (void)] #:fill [fill (void)] #:fill-rule [frule (default-fill-rule)]
           xstepsize [ystepsize 0.0]]
    (define xstep : Nonnegative-Flonum
      (cond [(<= xstepsize 0.0) 1.0]
            [else (max (real->double-flonum xstepsize) 0.0)]))
    
    (define ystep : Nonnegative-Flonum
      (cond [(= ystepsize 0.0) xstep]
            [(< ystepsize 0.0) (* xstep (- (real->double-flonum ystepsize)))]
            [else (max (real->double-flonum ystepsize) 0.0)]))
    
    (define loc : Float-Complex (~point2d home))
    (define home-pos : Float-Complex (make-rectangular (* (real-part loc) xstep) (* (imag-part loc) ystep)))
    (define-values (tsx tsy) (geo-path-turn-scales t-scale 0.5))
    (define-values (usx usy) (geo-path-turn-scales u-scale 0.25))
    
    (create-geometry-object gomamon
                            #:with [name (geo-draw-path! stroke fill frule) geo-path-extent (geo-shape-outline stroke)]
                            (make-geo-trail home-pos anchor)
                            (make-geo-bbox home-pos) home-pos home-pos
                            (list (gpp:point #\M home-pos)) (make-hash)
                            xstep ystep (* tsx xstep) (* tsy ystep) (* usx xstep) (* usy ystep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-gomamon-line-move! left            #:-> -1.0)
(define-gomamon-line-move! right           #:-> +1.0)
(define-gomamon-line-move! up              #:!> -1.0)
(define-gomamon-line-move! down            #:!> +1.0)
(define-gomamon-line-move! up right        #:+> +1.0 -1.0)
(define-gomamon-line-move! right down      #:+> +1.0 +1.0)
(define-gomamon-line-move! down left       #:+> -1.0 +1.0)
(define-gomamon-line-move! left up         #:+> -1.0 -1.0)

(define-gomamon-turn-move! up-right-down   #:+> [180.0 360.0  1.0  0.0  2.0  0.0] #:boundary-guard -1.0i)
(define-gomamon-turn-move! up-left-down    #:-> [360.0 180.0 -1.0  0.0 -2.0  0.0] #:boundary-guard -1.0i)
(define-gomamon-turn-move! right-down-left #:+> [-90.0  90.0  0.0  1.0  0.0  2.0] #:boundary-guard +1.0)
(define-gomamon-turn-move! right-up-left   #:-> [ 90.0 -90.0  0.0 -1.0  0.0 -2.0] #:boundary-guard +1.0)
(define-gomamon-turn-move! down-left-up    #:+> [  0.0 180.0 -1.0  0.0 -2.0  0.0] #:boundary-guard +1.0i)
(define-gomamon-turn-move! down-right-up   #:-> [180.0   0.0  1.0  0.0  2.0  0.0] #:boundary-guard +1.0i)
(define-gomamon-turn-move! left-up-right   #:+> [ 90.0 270.0  0.0 -1.0  0.0 -2.0] #:boundary-guard -1.0)
(define-gomamon-turn-move! left-down-right #:-> [270.0  90.0  0.0  1.0  0.0  2.0] #:boundary-guard -1.0)

(define-gomamon-turn-move! up right
  #:+> [180.0 270.0 1.0  0.0 1.0 -1.0]
  #:-> [ 90.0   0.0 0.0 -1.0 1.0 -1.0])

(define-gomamon-turn-move! right down
  #:+> [270.0 360.0 0.0 1.0 1.0 1.0]
  #:-> [180.0  90.0 1.0 0.0 1.0 1.0])

(define-gomamon-turn-move! down left
  #:+> [  0.0  90.0 -1.0 0.0 -1.0 1.0]
  #:-> [270.0 180.0  0.0 1.0 -1.0 1.0])

(define-gomamon-turn-move! left up
  #:+> [ 90.0 180.0  0.0 -1.0 -1.0 -1.0]
  #:-> [360.0 270.0 -1.0  0.0 -1.0 -1.0])

(define gomamon-drift! : (->* (Gomamon Geo-Bezier-Datum (Listof Geo-Bezier-Datum)) ((Option Geo-Anchor-Name)) Void)
  (lambda [wani end-step ctrl-steps [anchor #false]]
    (define xsize : Flonum (gomamon-xstepsize wani))
    (define ysize : Flonum (gomamon-ystepsize wani))
    (define endpt : Float-Complex (geo-path-bezier-point wani end-step xsize ysize))
    (define ctrls : (Listof Float-Complex)
      (for/list : (Listof Float-Complex) ([ctrl (in-list ctrl-steps)])
        (geo-path-bezier-point wani ctrl xsize ysize)))

    (cond [(null? ctrls) (geo-path-linear-bezier wani endpt anchor)]
          [(null? (cdr ctrls)) (geo-path-quadratic-bezier wani endpt (car ctrls) anchor)]
          [else (geo-path-cubic-bezier wani endpt (car ctrls) (cadr ctrls) anchor)])))

(define gomamon-move-to! : (->* (Gomamon (U Geo-Anchor-Name Complex)) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [wani target [anchor #false] [info #false]]
    (geo-path-connect-to wani target anchor info)))

(define gomamon-jump-to! : (->* (Gomamon (U Geo-Anchor-Name Complex)) ((Option Geo-Anchor-Name)) Void)
  (lambda [wani target [anchor #false]]
    (geo-path-jump-to wani target anchor)))

(define gomamon-jump-back! : (->* (Gomamon) ((U Geo-Anchor-Name Complex) (Option Geo-Anchor-Name)) Void)
  (lambda [wani [target #false] [anchor #false]]
    (geo-path-jump-to wani target anchor)))

(define gomamon-T-step! : (->* (Gomamon (U Geo-Anchor-Name Complex)) (Any Any) Void)
  (lambda [goma target [info1 #false] [info2 #false]]
    (define-values (hstep vstep)
      (cond [(not (complex? target)) (values target target)]
            [(real? target) (values target target)]
            [else (values (real-part target) (imag-part target))]))
    
    (gomamon-move-right! goma hstep #false info1)
    (gomamon-move-down!  goma vstep #false info2)))
  
(define gomamon-L-step! : (->* (Gomamon (U Geo-Anchor-Name Complex)) (Any Any) Void)
  (lambda [goma target [info1 #false] [info2 #false]]
    (define-values (hstep vstep)
      (cond [(not (complex? target)) (values target target)]
            [(real? target) (values target target)]
            [else (values (real-part target) (imag-part target))]))
    
    (gomamon-move-down!  goma vstep #false info1)
    (gomamon-move-right! goma hstep #false info2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-path-stick : (->* (Geo:Path)
                              (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator)
                               #:trusted-anchors (Option Geo-Trusted-Anchors) #:truncate? Boolean
                               Geo-Anchor->Sticker)
                              (U Geo:Group Geo:Path))
  (lambda [#:trusted-anchors [trusted-anchors #false] #:id [id #false] #:operator [op #false] #:truncate? [truncate? #true]
           self [anchor->sticker default-anchor->sticker]]
    (geo:path-stick self anchor->sticker trusted-anchors truncate?
                    (or id (gensym 'geo:path:)) op 0.0+0.0i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-anchor-position : (->* (Geo:Path Geo-Anchor-Name) (#:translate? Boolean) Float-Complex)
  (lambda [self anchor #:translate? [translate? #false]]
    (define abspos : Float-Complex (geo-trail-ref (geo:path-trail self) anchor))
    
    (cond [(not translate?) abspos]
          [else (- abspos (geo-bbox-position (geo:path-bbox self)))])))

