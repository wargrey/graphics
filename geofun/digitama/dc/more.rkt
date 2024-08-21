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
(define geo-stadium : (->* (Real Real) (#:id (Option Symbol) #:border Maybe-Stroke-Paint #:fill Maybe-Fill-Paint) Geo:Stadium)
  (lambda [length radius #:id [id #false] #:border [border (void)] #:fill [pattern (void)]]
    (define flength : Nonnegative-Flonum (~length length))
    (define flradius : Nonnegative-Flonum (~length radius flength))
    (define d : Nonnegative-Flonum (* 2.0 flradius))
    (define stadium-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox (+ d flength) d))
    
    (create-geometry-object geo:stadium
                            #:with [(geo-shape-surface-wrapper geo-stadium-surface border pattern) stadium-bbox] #:id id
                            flength flradius)))

(define geo-sandglass : (->* (Real)
                             (Real #:id (Option Symbol) #:neck-width Real #:neck-height Real #:tube-height Real
                                   #:border Maybe-Stroke-Paint #:fill Option-Fill-Paint)
                             Geo:Sandglass)
  (lambda [#:neck-width [neck-width -0.1618] #:neck-height [neck-height -0.0618] #:tube-height [tube-height 0]
           #:border [border (default-border-paint)] #:fill [pattern (default-fill-paint)] #:id [id #false]
           width [height -1.618]]
    (define-values (flwidth flheight) (~size width height))
    (define neck-flwidth (~length neck-width flwidth))
    (define neck-flheight (~length neck-height flheight))
    (define tube-flheight (~length tube-height flheight))
    (define sandglass-bbox : Geo-Calculate-BBox (geo-shape-plain-bbox flwidth flheight))
    
    (create-geometry-object geo:sandglass
                            #:with [(geo-shape-surface-wrapper geo-sandglass-surface border pattern) sandglass-bbox] #:id id
                            neck-flwidth neck-flheight tube-flheight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-stadium-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:stadium?])
      (dc_stadium create-abstract-surface
                  (geo:stadium-length self) (geo:stadium-radius self)
                  (current-border-source) (current-fill-source)
                  (default-geometry-density)))))

(define geo-sandglass-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:sandglass?])
      (define-values (width height) (geo-flsize self))
      (dc_sandglass create-abstract-surface
                    width height
                    (geo:sandglass-neck-width self) (geo:sandglass-neck-height self) (geo:sandglass-tube-height self)
                    (current-border-source) (current-fill-source)
                    (default-geometry-density)))))
