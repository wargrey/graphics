#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/measure))
(provide (all-from-out "base.rkt"))

(provide Renamon renamon? Renamon-Ribbon)
(provide Geo-Print-Datum Geo-Step-Datum Geo-Bezier-Datum)
(provide Option-Track-Halo-Paint Maybe-Track-Halo-Paint)
(provide default-track-halo-stroke renamon-heading)
(provide default-renamon-description-format)
(provide default-renamon-angle default-renamon-order default-renamon-terminal-order)
(provide define-renamon-rule! define-renamon-generator!)

(provide
 (rename-out [define-renamon-rule! define-renamon-primitive!])
 
 (rename-out [geo-track-close renamon-close!]
             [renamon-stamp renamon-stamp!]
             [renamon-teleport! renamon-jump-to!])
 
 (rename-out [renamon-forward! renamon-F!]
             [renamon-jump! renamon-f!]
             [renamon-turn-left! renamon-+!]
             [renamon-turn-right! renamon--!]
             [renamon-backward! renamon-B!]))

(require digimon/measure)

(require "base.rkt")
(require "renamon/dsl.rkt")
(require "renamon/primitive.rkt")

(require "self.rkt")
(require "trace.rkt")
(require "anchor.rkt")
(require "primitives.rkt")

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
(define-syntax (define-renamon! stx)
  (syntax-case stx []
    [(_ name [args ...] #:- move-expr ...)
     (syntax/loc stx
       (define name : Renamon
         (with-renamon! (make-renamon args ...)
           move-expr ...)))]))

(define-syntax (with-renamon! stx)
  (syntax-case stx []
    [(_ rena move-expr ...)
     (quasisyntax/loc stx
       (let ([self rena])
         (renamon-dsl self move-expr)
         ...
         self))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-renamon
  (lambda [#:id [name : (Option Symbol) #false]
           #:desc [desc : (Option String) #false]
           #:avatar [avatar : (Option Geo) #false]
           #:ribbon [ribbon : (Option Renamon-Ribbon) #false]
           #:angle [angle0 : (Option Real) #false]
           #:anchor [anchor : Geo-Anchor-Name '#:home]
           #:at [home : Geo-Print-Datum 0]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [fill : Maybe-Fill-Paint (void)]
           #:halo-stroke [halo-stroke : Maybe-Track-Halo-Paint (void)]
           [stepsize0 : Real-Length]
           [heading0 : Real pi/2]
           [angle-unit : Angle-Unit 'rad]]
    (define stepsize : Positive-Flonum (let ([size (~dimension stepsize0)]) (if (> size 0.0) size 1.0)))
    (define heading : Flonum (~wrap (~rad heading0 angle-unit) 2pi))
    (define angle : (Option Flonum) (and angle0 (~rad angle0 angle-unit)))
    (define home-pos : Float-Complex (* (~point2d home) stepsize))

    (define rena : Renamon
      (create-geometry-object renamon
                              #:with [name (geo-draw-track! stroke fill halo-stroke)
                                           geo-track-extent (geo-track-bleed stroke halo-stroke)]
                              #:desc desc
                              ; track fields
                              (make-geo-trace home-pos anchor)
                              (make-geo-bbox home-pos) home-pos home-pos
                              (list (gpp:point #\M home-pos)) (make-hash)
                              null null
                              
                              ; renamon fields
                              (box (make-renamon-info #:heading heading #:avatar avatar #:ribbon ribbon))
                              stepsize angle -1.0))

    (when (and avatar) (renamon-stamp rena avatar))
    rena))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define renamon-jump! : (->* (Renamon) (Real (Option Geo-Anchor-Name)) Void)
  (lambda [rena [steps 1.0] [anchor #false]]
    (geo-track-jump-to rena (make-polar steps (renamon-heading rena))
                       (renamon-anchor-rewrite anchor) (geo:track-here rena)
                       renamon-position)))

(define renamon-forward! : (->* (Renamon) (Real (Option Geo-Anchor-Name) Any) Void)
  (lambda [rena [steps 1.0] [anchor #false] [info #false]]
    (geo-track-connect-to rena (make-polar (+ steps) (renamon-heading rena))
                          (renamon-anchor-rewrite anchor) info (geo:track-here rena)
                          renamon-position)))

(define renamon-backward! : (->* (Renamon) (Real (Option Geo-Anchor-Name) Any) Void)
  (lambda [rena [steps 1.0] [anchor #false] [info #false]]
    (geo-track-connect-to rena (make-polar (- steps) (renamon-heading rena))
                          (renamon-anchor-rewrite anchor) info (geo:track-here rena)
                          renamon-position)))

(define renamon-left! : (->* (Renamon Real) (Angle-Unit) Void)
  (lambda [rena angle [unit 'rad]]
    (renamon-turn rena (+ (~rad angle unit)))))

(define renamon-right! : (->* (Renamon Real) (Angle-Unit) Void)
  (lambda [rena angle [unit 'rad]]
    (renamon-turn rena (- (~rad angle unit)))))

(define renamon-turn-to! : (->* (Renamon Real) (Angle-Unit) Void)
  (lambda [rena angle [unit 'rad]]
    (renamon-turn-to rena (~rad angle unit))))

(define renamon-turn-towards! : (-> Renamon (U Geo-Anchor-Name Complex) Void)
  (lambda [rena target]
    (define theta (renamon-towards rena target))

    (when (and theta)
      (renamon-turn-to rena theta))))
  
(define renamon-turn-left! : (->* (Renamon) (Real) Void)
  (lambda [rena [steps 1.0]]
    (renamon-turn rena
                  (+ (* (real->double-flonum steps)
                        (or (renamon-angle-delta rena)
                            (default-renamon-angle)))))))

(define renamon-turn-right! : (->* (Renamon) (Real) Void)
  (lambda [rena [steps 1.0]]
    (renamon-turn rena
                  (- (* (real->double-flonum steps)
                        (or (renamon-angle-delta rena)
                            (default-renamon-angle)))))))

(define renamon-teleport! : (case-> [Renamon (U Geo-Anchor-Name Complex) -> Void]
                                    [Renamon Complex (Option Geo-Anchor-Name) -> Void])
  (case-lambda
    [(rena target) (geo-track-jump-to rena target renamon-position)]
    [(rena target anchor) (geo-track-jump-to rena target anchor renamon-position)]))

(define renamon-slide-to! : (case-> [Renamon (U Geo-Anchor-Name Complex) -> Void]
                                    [Renamon Geo-Anchor-Name Any -> Void]
                                    [Renamon Complex (Option Geo-Anchor-Name) -> Void]
                                    [Renamon Complex (Option Geo-Anchor-Name) Any -> Void])
  (case-lambda
    [(rena target) (geo-track-connect-to rena target #false renamon-position)]
    [(rena target argument)
     (if (complex? target)
         (geo-track-connect-to rena target argument #false renamon-position)
         (geo-track-connect-to rena target argument))]
    [(rena target anchor info) (geo-track-connect-to rena target (renamon-anchor-rewrite anchor) info renamon-position)]))

(define renamon-move-to! : (case-> [Renamon (U Geo-Anchor-Name Complex) -> Void]
                                   [Renamon Geo-Anchor-Name Any -> Void]
                                   [Renamon Complex (Option Geo-Anchor-Name) -> Void]
                                   [Renamon Complex (Option Geo-Anchor-Name) Any -> Void])
  (case-lambda
    [(rena target) (renamon-turn-towards! rena target) (renamon-slide-to! rena target)]
    [(rena target argument) (renamon-turn-towards! rena target) (renamon-slide-to! rena target argument)]
    [(rena target anchor info) (renamon-turn-towards! rena target) (renamon-slide-to! rena target anchor info)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define renamon-stick! : (case-> [Renamon Geo-Sticker -> Void]
                                 [Renamon Geo -> Void]
                                 [Renamon Geo Geo-Pin-Anchor -> Void]
                                 [Renamon Geo Geo-Pin-Anchor Complex -> Void])
  (case-lambda
    [(rena gobj) (geo-track-stamp rena gobj)]
    [(rena gobj anchor) (geo-track-stamp rena gobj anchor)]
    [(rena gobj anchor offset) (geo-track-stamp rena (make-sticker gobj anchor (renamon-position rena offset)))]))
