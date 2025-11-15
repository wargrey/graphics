#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/track/self.rkt"))
(provide (all-from-out "digitama/geometry/anchor.rkt"))

(provide Geo:Track Gomamon)
(provide geo:track? gomamon?)
(provide Geo-Track-Anchor->Sticker)
(provide Geo-Sticker geo-sticker?)
(provide make-sticker default-track-anchor->sticker)
(provide current-master-track geo-track-close)

(provide
 (rename-out [gomamon-move-up-right! gomamon-move-right-up!]
             [gomamon-move-right-down! gomamon-move-down-right!]
             [gomamon-move-down-left! gomamon-move-left-down!]
             [gomamon-move-left-up! gomamon-move-up-left!])

 (rename-out [gomamon-jump-up-right! gomamon-jump-right-up!]
             [gomamon-jump-right-down! gomamon-jump-down-right!]
             [gomamon-jump-down-left! gomamon-jump-left-down!]
             [gomamon-jump-left-up! gomamon-jump-up-left!])

 (rename-out [geo-track-close gomamon-close!]
             [make-sticker make-geo-sticker]))

(require racket/math)
(require digimon/metrics)

(require "paint.rkt")

(require "digitama/track/dsl.rkt")
(require "digitama/track/self.rkt")
(require "digitama/track/datum.rkt")
(require "digitama/track/gomamon.rkt")

(require "digitama/convert.rkt")
(require "digitama/geometry/dot.rkt")
(require "digitama/geometry/bbox.rkt")
(require "digitama/geometry/trail.rkt")
(require "digitama/geometry/anchor.rkt")
(require "digitama/geometry/footprint.rkt")

(require "digitama/dc/track.rkt")
(require "digitama/dc/composite.rkt")
(require "digitama/path/label.rkt")
(require "digitama/layer/sticker.rkt")
(require "digitama/track/sticker.rkt")

(require (for-syntax racket/base))

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
    [(_ goma move-expr ...)
     (quasisyntax/loc stx
       (let ([self goma])
         (gomamon-dsl self move-expr)
         ...
         self))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-gomamon : (->* (Real)
                            (Real+% #:T-scale Geo-Print-Datum #:U-scale Geo-Print-Datum
                                    #:anchor Geo-Anchor-Name #:at Geo-Print-Datum #:id (Option Symbol)
                                    #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Gomamon)
  (lambda [#:T-scale [t-scale +nan.0] #:U-scale [u-scale +nan.0]
           #:anchor [anchor '#:home] #:at [home 0] #:id [name #false]
           #:stroke [stroke (void)] #:fill [fill (void)]
           xstepsize [ystepsize '(100.0 %)]]
    (define xstep : Nonnegative-Flonum
      (cond [(<= xstepsize 0.0) 1.0]
            [else (max (real->double-flonum xstepsize) 0.0)]))
    
    (define ystep : Nonnegative-Flonum (~length ystepsize xstep))
    
    (define loc : Float-Complex (~point2d home))
    (define home-pos : Float-Complex (make-rectangular (* (real-part loc) xstep) (* (imag-part loc) ystep)))
    (define-values (tsx tsy) (geo-track-turn-scales t-scale 0.50))
    (define-values (usx usy) (geo-track-turn-scales u-scale 0.25))
    
    (create-geometry-object gomamon
                            #:with [name (geo-draw-track! stroke fill) geo-track-extent (geo-shape-outline stroke)]
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
  (lambda [goma end-step ctrl-steps [anchor #false]]
    (define xsize : Flonum (gomamon-xstepsize goma))
    (define ysize : Flonum (gomamon-ystepsize goma))
    (define endpt : Float-Complex (geo-track-bezier-point goma end-step xsize ysize))
    (define ctrls : (Listof Float-Complex)
      (for/list : (Listof Float-Complex) ([ctrl (in-list ctrl-steps)])
        (geo-track-bezier-point goma ctrl xsize ysize)))

    (cond [(null? ctrls) (geo-track-linear-bezier goma endpt anchor)]
          [(null? (cdr ctrls)) (geo-track-quadratic-bezier goma endpt (car ctrls) anchor)]
          [else (geo-track-cubic-bezier goma endpt (car ctrls) (cadr ctrls) anchor)])))

(define gomamon-move-to! : (->* (Gomamon (U Geo-Anchor-Name Complex)) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [goma target [anchor #false] [info #false]]
    (geo-track-connect-to goma target anchor info)))

(define gomamon-jump-to! : (->* (Gomamon (U Geo-Anchor-Name Complex)) ((Option Geo-Anchor-Name)) Void)
  (lambda [goma target [anchor #false]]
    (geo-track-jump-to goma target anchor)))

(define gomamon-radial-move*! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [goma length degrees [anchor #false] [info #false]]
    (define here : Float-Complex (geo:track-here goma))
    (define delta : Complex (make-polar length (degrees->radians degrees)))

    (geo-track-connect-to goma delta anchor info here)))

(define gomamon-radial-move! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [goma length degrees [anchor #false] [info #false]]
    (define here : Float-Complex (geo:track-here goma))
    (define delta : Complex (make-polar length (degrees->radians degrees)))

    (geo-track-connect-to goma delta anchor info here)
    (geo-track-jump-to goma here)))

(define gomamon-radial-back! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [goma length degrees [anchor #false] [info #false]]
    (define here : Float-Complex (geo:track-here goma))
    (define delta : Complex (make-polar length (degrees->radians degrees)))

    (geo-track-jump-to goma delta anchor here)
    (geo-track-connect-to goma here info)))

(define gomamon-random-walk! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [goma xstep ystep [anchor #false] [info #false]]
    (geo-track-L goma (* (- (* (random) 2.0) 1.0) xstep) (* (- (* (random) 2.0) 1.0) ystep) 1.0 1.0 anchor info)))

(define gomamon-random-jump! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name)) Void)
  (lambda [goma xstep ystep [anchor #false]]
    (geo-track-M goma (* (- (* (random) 2.0) 1.0) xstep) (* (- (* (random) 2.0) 1.0) ystep) 1.0 1.0 anchor)))

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
(define geo-track-stick : (->* (Geo:Track)
                               (Geo-Track-Anchor->Sticker
                                #:id (Option Symbol) #:base-operator (Option Geo-Pin-Operator) #:operator (Option Geo-Pin-Operator)
                                #:trusted-anchors (Option Geo-Trusted-Anchors) #:truncate? Boolean #:offset Float-Complex)
                               (U Geo:Group Geo:Track))
  (lambda [#:trusted-anchors [trusted-anchors #false] #:truncate? [truncate? #true] #:offset [offset 0.0+0.0i]
           #:id [id #false] #:base-operator [base-op #false] #:operator [sibs-op #false]
           self [anchor->sticker default-track-anchor->sticker]]
    (geo:track-stick self anchor->sticker trusted-anchors truncate?
                     (or id (gensym 'geo:track:)) base-op sibs-op offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gtmult : (->* (Geo-Track-Multiplicity+Label Geo-Track-Multiplicity+Label) (#:t Nonnegative-Flonum) #:rest Geo-Track-Info-Datum Geo:Track:Info)
  (lambda [start end #:t [t (default-geo-track-multiplicity-base-position)] . extra]
    (define-values (mstart lstart) (if (pair? start) (values (car start) (cdr start)) (values start #false)))
    (define-values (m-end  l-end)  (if (pair? end)   (values (car end)   (cdr end))   (values end #false)))
    
    (geo-track-info/paired-labels (default-geo-track-label-base-position) lstart l-end
                                  (and (or mstart m-end)
                                       (geo:track:multiplicity mstart m-end t))
                                  extra)))

(define gtlabel : (->* (Geo-Path-Label Geo-Path-Label) (#:t Nonnegative-Flonum) #:rest Geo-Track-Info-Datum Geo:Track:Info)
  (lambda [start end #:t [t (default-geo-track-label-base-position)] . extra]
    (geo-track-info/paired-labels t start end #false extra)))

(define gtlabel* : (->* ((Listof Geo-Path-Label)) (#:t Nonnegative-Flonum) #:rest Geo-Track-Info-Datum Geo:Track:Info)
  (lambda [labels #:t [t (default-geo-track-label-base-position)] . extra]
    (geo-track-info/labels t labels #false extra)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-track-anchor-position : (->* (Geo:Track Geo-Anchor-Name) (#:translate? Boolean) Float-Complex)
  (lambda [self anchor #:translate? [translate? #false]]
    (define abspos : Float-Complex (geo-trail-ref (geo:track-trail self) anchor))
    
    (cond [(not translate?) abspos]
          [else (- abspos (geo-bbox-position (geo:track-bbox self)))])))
