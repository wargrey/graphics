#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/shape.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:stadium geo
  ([length : Nonnegative-Flonum]
   [radius : Nonnegative-Flonum])
  #:type-name Geo:Stadium
  #:transparent)

(struct geo:sandglass geo
  ([neck-width : Nonnegative-Flonum]
   [neck-height : Nonnegative-Flonum]
   [tube-height : Nonnegative-Flonum])
  #:type-name Geo:Sandglass
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-stadium : (->* (Real Real) (#:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Stadium)
  (lambda [length radius #:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]]
    (define flength : Nonnegative-Flonum (~length length))
    (define flradius : Nonnegative-Flonum (~length radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    
    (create-geometry-object geo:stadium
                            #:surface geo-stadium-surface stroke pattern
                            #:extent (geo-shape-plain-extent (+ d flength) d 0.0 0.0)
                            #:id id
                            flength flradius)))

(define geo-sandglass : (->* (Real)
                             (Real #:id (Option Symbol) #:neck-width Real #:neck-height Real #:tube-height Real
                                   #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint)
                             Geo:Sandglass)
  (lambda [#:neck-width [neck-width -0.1618] #:neck-height [neck-height -0.0618] #:tube-height [tube-height 0]
           #:stroke [stroke (void)] #:fill [pattern (void)] #:id [id #false]
           width [height -1.618]]
    (define-values (flwidth flheight) (~size width height))
    (define neck-flwidth (~length neck-width flwidth))
    (define neck-flheight (~length neck-height flheight))
    (define tube-flheight (~length tube-height flheight))
    
    (create-geometry-object geo:sandglass
                            #:surface (geo-sandglass-surface flwidth flheight) stroke pattern
                            #:extent (geo-shape-plain-extent flwidth flheight 0.0 0.0)
                            #:id id
                            neck-flwidth neck-flheight tube-flheight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-stadium-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:stadium?])
      (dc_stadium create-abstract-surface
                  (geo:stadium-length self) (geo:stadium-radius self)
                  (current-stroke-source) (current-fill-source)
                  (default-geometry-density)))))

(define geo-sandglass-surface : (-> Nonnegative-Flonum Nonnegative-Flonum Geo-Surface-Create)
  (lambda [width height]
    (λ [self]
      (with-asserts ([self geo:sandglass?])
        (dc_sandglass create-abstract-surface
                      width height
                      (geo:sandglass-neck-width self) (geo:sandglass-neck-height self) (geo:sandglass-tube-height self)
                      (current-stroke-source) (current-fill-source)
                      (default-geometry-density))))))
