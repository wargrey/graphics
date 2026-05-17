#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "base.rkt"))

(provide Gomamon gomamon?)

(provide
 (rename-out [geo-track-close gomamon-close!])
 
 (rename-out [gomamon-move-upwards! gomamon-move-upward!]
             [gomamon-move-rightwards! gomamon-move-rightward!]
             [gomamon-move-downwards! gomamon-move-downward!]
             [gomamon-move-leftwards! gomamon-move-leftward!])

 (rename-out [gomamon-jump-upwards! gomamon-jump-upward!]
             [gomamon-jump-rightwards! gomamon-jump-rightward!]
             [gomamon-jump-downwards! gomamon-jump-downward!]
             [gomamon-jump-leftwards! gomamon-jump-leftward!]))

(require digimon/measure)

(require "base.rkt")
(require "gomamon/dsl.rkt")
(require "gomamon/primitive.rkt")

(require "self.rkt")
(require "trail.rkt")
(require "anchor.rkt")
(require "primitive.rkt")

(require "../self.rkt")
(require "../convert.rkt")
(require "../layer/type.rkt")
(require "../layer/sticker.rkt")
(require "../paint/self.rkt")

(require "../geometry/dot.rkt")
(require "../geometry/bbox.rkt")
(require "../geometry/footprint.rkt")

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
(define make-gomamon : (->* (Real-Length)
                            (Length+% #:turn-radius% Geo-Print-Datum
                                      #:anchor Geo-Anchor-Name #:at Geo-Print-Datum #:id (Option Symbol)
                                      #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint)
                            Gomamon)
  (lambda [#:turn-radius% [t-radius% +nan.0]
           #:anchor [anchor '#:home] #:at [home 0] #:id [name #false]
           #:stroke [stroke (void)] #:fill [fill (void)]
           xstepsize0 [ystepsize (&% 100.0)]]
    (define xstepsize (~dimension xstepsize0))
    (define xstep : Nonnegative-Flonum
      (cond [(<= xstepsize 0.0) 1.0]
            [else (max (real->double-flonum xstepsize) 0.0)]))
    
    (define ystep : Nonnegative-Flonum (~dimension ystepsize xstep))
    
    (define loc : Float-Complex (~point2d home))
    (define home-pos : Float-Complex (make-rectangular (* (real-part loc) xstep) (* (imag-part loc) ystep)))
    (define-values (tsx tsy) (gomamon-turn-scales t-radius% 0.25))
    
    (create-geometry-object gomamon
                            #:with [name (geo-draw-track! stroke fill) geo-track-extent (geo-shape-outline stroke)]
                            ; track fields
                            (make-geo-trail home-pos anchor)
                            (make-geo-bbox home-pos) home-pos home-pos
                            (list (gpp:point #\M home-pos)) (make-hash)
                            null null
                            
                            ; gomamon fields
                            xstep ystep (* tsx xstep) (* tsy ystep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-gomamon-line-move! left            #:-> -1.0)
(define-gomamon-line-move! right           #:-> +1.0)
(define-gomamon-line-move! up              #:!> -1.0)
(define-gomamon-line-move! down            #:!> +1.0)
(define-gomamon-line-move! right up        #:+> +1.0 -1.0)
(define-gomamon-line-move! right down      #:+> +1.0 +1.0)
(define-gomamon-line-move! left down       #:+> -1.0 +1.0)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gomamon-drift! : (->* (Gomamon Geo-Bezier-Datum (Listof Geo-Bezier-Datum)) ((Option Geo-Anchor-Name)) Void)
  (lambda [goma end-step ctrl-steps [anchor #false]]
    (geo-track-drift goma end-step ctrl-steps anchor
                     (gomamon-xstepsize goma) (gomamon-ystepsize goma))))

(define gomamon-jump! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name)) Void)
  (lambda [goma length radians [anchor #false]]
    (geo-track-jump-to goma (make-polar length radians) anchor
                       (geo:track-here goma) gomamon-grid-position)))

(define gomamon-jump-to! : (->* (Gomamon (U Geo-Anchor-Name Complex)) ((Option Geo-Anchor-Name)) Void)
  (lambda [goma target [anchor #false]]
    (geo-track-jump-to goma target anchor gomamon-grid-position)))

(define gomamon-move! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [goma length radians [anchor #false] [info #false]]
    (geo-track-connect-to goma (make-polar length radians) anchor info
                          (geo:track-here goma) gomamon-grid-position)))

(define gomamon-move-to! : (case-> [Gomamon (U Geo-Anchor-Name Complex) -> Void]
                                   [Gomamon Geo-Anchor-Name Any -> Void]
                                   [Gomamon Complex (Option Geo-Anchor-Name) -> Void]
                                   [Gomamon Complex (Option Geo-Anchor-Name) Any -> Void])
  (case-lambda
    [(goma target) (geo-track-connect-to goma target #false #false gomamon-grid-position)]
    [(goma target argument)
     (if (complex? target)
         (geo-track-connect-to goma target argument #false gomamon-grid-position)
         (geo-track-connect-to goma target #false argument gomamon-grid-position))]
    [(goma target anchor info) (geo-track-connect-to goma target anchor info gomamon-grid-position)]))

(define gomamon-move-back! : (case-> [Gomamon (U Geo-Anchor-Name Complex) -> Void]
                                     [Gomamon Geo-Anchor-Name Any -> Void]
                                     [Gomamon Complex (Option Geo-Anchor-Name) -> Void]
                                     [Gomamon Complex (Option Geo-Anchor-Name) Any -> Void])
  (case-lambda
    [(goma target)
     (geo-track-jump-to goma target #false gomamon-grid-position)
     (geo-track-connect-to goma (geo:track-here goma) #false)]
    [(goma target argument)
     (define here : Float-Complex (geo:track-here goma))
     (if (complex? target)
         (begin (geo-track-jump-to goma target argument gomamon-grid-position)
                (geo-track-connect-to goma here #false))
         (begin (geo-track-jump-to goma target #false gomamon-grid-position)
                (geo-track-connect-to goma here argument)))]
    [(goma target anchor info)
     (geo-track-jump-to goma target anchor gomamon-grid-position)
     (geo-track-connect-to goma (geo:track-here goma) info)]))

(define gomamon-radial-move! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [goma length radians [anchor #false] [info #false]]
    (define here : Float-Complex (geo:track-here goma))
    (define delta : Complex (make-polar length radians))

    (geo-track-connect-to goma delta anchor info here gomamon-grid-position)
    (geo-track-jump-to goma here)))

(define gomamon-radial-back! : (->* (Gomamon Real Real) ((Option Geo-Anchor-Name) Any) Void)
  (lambda [goma length radians [anchor #false] [info #false]]
    (define here : Float-Complex (geo:track-here goma))
    (define delta : Complex (make-polar length radians))

    (geo-track-jump-to goma delta anchor here gomamon-grid-position)
    (geo-track-connect-to goma here info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define gomamon-stamp! : (case-> [Gomamon Geo-Sticker -> Void]
                                 [Gomamon Geo -> Void]
                                 [Gomamon Geo Geo-Pin-Anchor -> Void]
                                 [Gomamon Geo Geo-Pin-Anchor Complex -> Void])
  (case-lambda
    [(goma gobj) (geo-track-stamp goma gobj)]
    [(goma gobj anchor) (geo-track-stamp goma gobj anchor)]
    [(goma gobj anchor offset) (geo-track-stamp goma (make-sticker gobj anchor (gomamon-grid-position goma offset)))]))

(define gomamon-focus! : (->* (Gomamon Geo-Anchor-Name) ((Option Geo-Anchor-Name)) Void)
  (lambda [goma target [anchor #false]]
    (geo-track-jump-to goma target anchor gomamon-grid-position)))
