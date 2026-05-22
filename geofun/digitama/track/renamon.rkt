#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out digimon/measure))
(provide (all-from-out "base.rkt"))

(provide Renamon renamon?)
(provide Geo-Print-Datum Geo-Step-Datum Geo-Bezier-Datum)
(provide Geo-Track-Halo-Datum Option-Track-Halo-Paint Maybe-Track-Halo-Paint)
(provide default-halo-paints default-halo-round? renamon-heading)
(provide default-renamon-description-format default-renamon-order)

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

(require "../dc/composite.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(require (for-syntax racket/string))
(require (for-syntax racket/symbol))

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

(define-syntax (define-renamon-generator! stx)
  (syntax-parse stx #:datum-literals [:]
    [(_ id
        (~alt (~optional (~seq #:desc description) #:defaults ([description #'#false]))
              (~optional (~seq #:with [(argv-expr : Type defval ...) ...])
                         #:defaults ([(argv-expr 1) null]
                                     [(Type 1) null]
                                     [(defval 2) null])))
        ...
        #:- move-expr ...)
     (with-syntax ([id! (format-id #'id "~a!" (syntax->datum #'id))]
                   [id: (format-id #'id "~a:" (syntax->datum #'id))]
                   [id*! (format-id #'id "~a*!" (syntax->datum #'id))]
                   [fallback-desc (datum->syntax #'id (string-titlecase (string-replace (symbol->immutable-string (syntax-e #'id)) #px"[-_]" " ")))])
       (syntax/loc stx
         (begin (define (id! #:order [order : Byte (default-renamon-order)]
                             [self : Renamon] (argv-expr : Type defval ...) ...) : Renamon
                  (parameterize ([default-renamon-order order])
                    (renamon-dsl self move-expr) ...)
                  self)
                
                (define (id*! #:order [order : Byte (default-renamon-order)]
                              #:id [name : (Option Symbol) #false]
                              #:desc [desc : (Option String) #false]
                              #:desc-format [desc-fmt : String (default-renamon-description-format)]
                              #:frame [frame : Geo-Frame-Datum #false]
                              #:trusted-anchors [trusted-anchors : (Option Geo-Trusted-Anchors) #false]
                              #:truncate? [truncate? : Boolean #true]
                              #:anchor->sticker [anchor->sticker : Geo-Track-Anchor->Sticker void]
                              [self : Renamon] (argv-expr : Type defval ...) ...) : Geo:Trail
                  (geo-track-stick #:id (or name (gensym 'id:)) #:frame frame
                                   #:desc (format desc-fmt (or desc description (geo-desc self) fallback-desc) order)
                                   #:trusted-anchors trusted-anchors #:truncate? truncate?
                                   (id! self argv-expr ... #:order order)
                                   anchor->sticker)))))]))

(define-syntax (define-renamon-primitive! stx)
  (syntax-parse stx #:datum-literals []
    [(_ name
        (~alt (~optional (~seq #:with [argv-expr ...]) #:defaults [((argv-expr 1) null)])
              (~optional (~seq #:anchor-format fmt) #:defaults ([fmt #'"~a::~a"]))
              (~optional (~seq #:anchor-abbr abbr) #:defaults ([abbr #'#false])))
        ...
        #:= rule-expand-expr ...
        #:- terminate-expr ...)
     (with-syntax ([rena-move! (format-id #'name "renamon-~a!" (syntax->datum #'name))])
       (syntax/loc stx
         (define (rena-move! [self : Renamon] argv-expr ...) : Void
           (define order (default-renamon-order))

           (parameterize* ([current-renamon-anchor-format fmt]
                           [current-renamon-primitive-name (renamon-anchor-prefix (or abbr 'name))])
                 (if (> order 0)
                     (parameterize ([default-renamon-order (- order 1)])
                       (renamon-dsl self rule-expand-expr) ... (void))
                     (begin (renamon-dsl self terminate-expr) ... (void)))))))]
    [(_ name alt-expr ... #:= rule-expand-expr ...)
     (syntax/loc stx
       (define-renamon-primitive! name alt-expr ...
         #:= rule-expand-expr ...
         #:-))]
    [(_ name
        (~alt (~optional (~seq #:with [argv-expr ...]) #:defaults [((argv-expr 1) null)])
              (~optional (~seq #:anchor-format fmt) #:defaults ([fmt #'"~a::~a"]))
              (~optional (~seq #:anchor-abbr abbr) #:defaults ([abbr #'#false])))
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
(define make-renamon
  (lambda [#:id [name : (Option Symbol) #false]
           #:desc [desc : (Option String) #false]
           #:avatar [avatar : (Option Geo) #false]
           #:anchor [anchor : Geo-Anchor-Name '#:_]
           #:at [home : Geo-Print-Datum 0]
           #:stroke [stroke : Maybe-Stroke-Paint (void)]
           #:fill [fill : Maybe-Fill-Paint (void)]
           #:halo-strokes [halo-strokes : Maybe-Track-Halo-Paint (void)]
           #:halo-round? [round? : (U Void Boolean) (void)]
           [stepsize0 : Real-Length]
           [delta0 : Nonnegative-Real]
           [heading0 : Real pi/2]
           [angle-unit : Angle-Unit 'rad]]
    (define stepsize : Positive-Flonum (let ([size (~dimension stepsize0)]) (if (> size 0.0) size 1.0)))
    (define angle-delta : Flonum (~rad delta0 angle-unit))
    (define heading : Flonum (~wrap (~rad heading0 angle-unit) 2pi))
    (define home-pos : Float-Complex (* (~point2d home) stepsize))

    (define rena : Renamon
      (create-geometry-object renamon
                              #:with [name (geo-draw-track! stroke fill halo-strokes round?)
                                           geo-track-extent (geo-track-bleed stroke halo-strokes)]
                              #:desc desc
                              ; track fields
                              (make-geo-trace home-pos anchor)
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
