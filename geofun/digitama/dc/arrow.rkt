#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/arrow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:arrow geo
  ([head-radius : Nonnegative-Flonum]
   [shaft-length : Nonnegative-Flonum]
   [shaft-thickness : Nonnegative-Flonum]
   [start-angle : Flonum]
   [wing-angle : (Option Flonum)])
  #:type-name Geo:Arrow
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-arrow : (->* (Real Real)
                         (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint
                               #:shaft-thickness Real #:wing-angle (Option Real) #:radian? Boolean)
                         Geo:Arrow)
  (lambda [#:id [id #false] #:shaft-thickness [shaft-thickness -0.3] #:wing-angle [wing-angle #false]
           #:radian? [radian? #true] #:stroke [stroke (void)] #:fill [pattern (void)]
           head-radius shaft-length [start 0.0]]
    (define rhead : Nonnegative-Flonum (~length head-radius))
    (define shaft-flthickness : Nonnegative-Flonum (~length shaft-thickness rhead))
    (define shaft-flength : Nonnegative-Flonum (~length shaft-length rhead))
    (define wing-flangle (and wing-angle (~radian wing-angle radian?)))
    
    (create-geometry-object geo:arrow
                            #:surface geo-arrow-surface stroke pattern
                            #:extent (geo-stroke-extent-wrapper geo-arrow-extent stroke)
                            #:id id
                            rhead shaft-flength shaft-flthickness (~radian start radian?) wing-flangle)))

(define geo-arrowhead : (->* (Real)
                             (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint
                                   #:shaft-thickness Real #:wing-angle (Option Real) #:radian? Boolean)
                             Geo:Arrow)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)]
           #:shaft-thickness [shaft-thickness 0.0] #:wing-angle [wing-angle #false] #:radian? [radian? #true]
           radius [start 0.0]]
    (define flradius : Nonnegative-Flonum (~length radius))
    (define shaft-flthickness : Nonnegative-Flonum (~length shaft-thickness flradius))
    (define wing-flangle (and wing-angle (~radian wing-angle radian?)))
    
    (create-geometry-object geo:arrow
                            #:surface geo-arrow-surface stroke pattern
                            #:extent (geo-stroke-extent-wrapper geo-arrow-extent stroke)
                            #:id id
                            flradius 0.0 shaft-flthickness (~radian start radian?) wing-flangle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-arrow-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:arrow?])
      (define metrics
        (dc-arrow-metrics (geo:arrow-head-radius self) (geo:arrow-start-angle self)
                          (geo:arrow-shaft-thickness self) (geo:arrow-shaft-length self)
                          (geo:arrow-wing-angle self)))
      (values (vector-ref metrics 3) (vector-ref metrics 4) #false))))

(define geo-arrow-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:arrow?])
      (dc_arrow create-abstract-surface
                (dc-arrow-metrics (geo:arrow-head-radius self) (geo:arrow-start-angle self)
                                  (geo:arrow-shaft-thickness self) (geo:arrow-shaft-length self)
                                  (geo:arrow-wing-angle self))
                (current-stroke-source) (current-fill-source) (default-geometry-density)))))
