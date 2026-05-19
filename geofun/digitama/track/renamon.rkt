#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/measure))
(provide (all-from-out "base.rkt"))

(provide Renamon renamon?)
(provide Geo-Print-Datum Geo-Step-Datum Geo-Bezier-Datum Geo-Track-Halo-Datum)
(provide default-halo-paints default-halo-round? renamon-heading)

(provide
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
(require "trail.rkt")
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
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

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

(define-syntax (define-renamon-script! stx)
  (syntax-parse stx #:datum-literals []
    [(_ id (~alt (~optional (~seq #:with [argv-expr ...]) #:defaults [((argv-expr 1) null)])) ...
        #:- move-expr ...)
     (syntax/loc stx
       (define (id [self : Renamon] argv-expr ... #:order [order : Byte (current-renamon-order)]) : Void
         (parameterize ([current-renamon-order order])
           (renamon-dsl self move-expr) ...)))]))

(define-syntax (define-renamon-primitive! stx)
  (syntax-parse stx #:datum-literals []
    [(_ name
        (~alt (~optional (~seq #:with [argv-expr ...]) #:defaults [((argv-expr 1) null)])
              (~optional (~seq #:anchor-format fmt) #:defaults ([fmt #'"~a::~a"]))
              (~optional (~seq #:abbr abbr) #:defaults ([abbr #'#false])))
        ...
        #:= rule-expand-expr ...
        #:- terminate-expr ...)
     (with-syntax ([rena-move! (format-id #'name "renamon-~a!" (syntax->datum #'name))])
       (syntax/loc stx
         (define (rena-move! [self : Renamon] argv-expr ...) : Void
           (define order (current-renamon-order))

           (parameterize* ([current-renamon-anchor-format fmt]
                           [current-renamon-primitive-name (renamon-anchor-prefix (or 'abbr 'name))])
                 (if (> order 0)
                     (parameterize ([current-renamon-order (- order 1)])
                       (renamon-dsl self rule-expand-expr) ... (void))
                     (begin (renamon-dsl self terminate-expr) ... (void)))))))]
    [(_ name
        (~alt (~optional (~seq #:with [argv-expr ...]) #:defaults [((argv-expr 1) null)])
              (~optional (~seq #:anchor-format fmt) #:defaults ([fmt #'"~a::~a"]))
              (~optional (~seq #:abbr abbr) #:defaults ([abbr #'#false])))
        ...
        #:- move-expr ...)
     (with-syntax ([rena-move! (format-id #'name "renamon-~a!" (syntax->datum #'name))])
       (syntax/loc stx
         (define (rena-move! [self : Renamon] argv-expr ...) : Void
           (parameterize* ([current-renamon-anchor-format fmt]
                           [current-renamon-primitive-name (renamon-anchor-prefix (or 'abbr 'name))])
             (renamon-dsl self move-expr) ...
             (void)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-renamon : (->* (Real-Length Nonnegative-Real)
                            (Real Angle-Unit
                                  #:anchor Geo-Anchor-Name #:at Geo-Print-Datum #:id (Option Symbol) #:avatar (Option Geo)
                                  #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint
                                  #:halo-strokes Geo-Track-Halo-Datum
                                  #:halo-round? (U Void Boolean))
                            Renamon)
  (lambda [#:anchor [anchor '#:home] #:at [home 0] #:id [name #false] #:avatar [avatar #false]
           #:stroke [stroke (void)] #:fill [fill (void)]
           #:halo-strokes [halo-strokes (void)] #:halo-round? [round? (void)]
           stepsize0 delta0 [heading0 0.0] [angle-unit 'rad]]
    (define stepsize : Positive-Flonum (let ([size (~dimension stepsize0)]) (if (> size 0.0) size 1.0)))
    (define angle-delta : Flonum (~rad delta0 angle-unit))
    (define heading : Flonum (~wrap (~rad heading0 angle-unit) 2pi))
    (define home-pos : Float-Complex (* (~point2d home) stepsize))

    (define rena : Renamon
      (create-geometry-object renamon
                              #:with [name (geo-draw-track! stroke fill halo-strokes round?)
                                           geo-track-extent (geo-track-bleed stroke halo-strokes)]
                              ; track fields
                              (make-geo-trail home-pos anchor)
                              (make-geo-bbox home-pos) home-pos home-pos
                              (list (gpp:point #\M home-pos)) (make-hash)
                              null null
                              
                              ; renamon fields
                              (box (make-renamon-info #:heading heading
                                                      #:avatar avatar))
                              stepsize angle-delta -1.0))

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
    (renamon-turn rena (+ (* (real->double-flonum steps) (renamon-angle-delta rena))))))

(define renamon-turn-right! : (->* (Renamon) (Real) Void)
  (lambda [rena [steps 1.0]]
    (renamon-turn rena (- (* (real->double-flonum steps) (renamon-angle-delta rena))))))

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
