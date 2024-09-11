#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/path.rkt")
(require "../geometry/polygon/arrow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:arrow geo
  ([head-radius : Nonnegative-Flonum]
   [shaft-length : Nonnegative-Flonum]
   [shaft-thickness : Nonnegative-Flonum]
   [start.rad : Flonum]
   [wing.rad : (Option Flonum)])
  #:type-name Geo:Arrow
  #:transparent)

(struct geo:dart geo
  ([head-radius : Nonnegative-Flonum]
   [start.rad : Flonum]
   [wing.rad : (Option Flonum)])
  #:type-name Geo:Dart
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-dart : (->* (Real)
                             (Real #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint
                                   #:wing-angle (Option Real) #:radian? Boolean)
                             Geo:Dart)
  (lambda [#:id [id #false] #:stroke [stroke (void)] #:fill [pattern (void)] #:wing-angle [wing-angle #false] #:radian? [radian? #true]
           radius [start 0.0]]
    (define flradius : Nonnegative-Flonum (~length radius))
    (define wing-flangle (and wing-angle (~radian wing-angle radian?)))
    
    (create-geometry-object geo:dart
                            #:surface geo-dart-surface stroke pattern
                            #:extent (geo-stroke-extent-wrapper geo-dart-extent stroke)
                            #:id id
                            flradius (~radian start radian?) wing-flangle)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-dart-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:dart?])
      (define-values (prints tx ty width height)
        (geo-dart-metrics (geo:dart-head-radius self) (geo:dart-start.rad self)
                          (geo:dart-wing.rad self)))
      (values width height #false))))

(define geo-dart-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:dart?])
      (define-values (prints tx ty width height)
        (geo-dart-metrics (geo:dart-head-radius self) (geo:dart-start.rad self)
                          (geo:dart-wing.rad self)))
      
      (dc_polygon create-abstract-surface width height prints tx ty #true #true
                  (current-stroke-source) (current-fill-source) (default-fill-rule)
                  (default-geometry-density)))))

(define geo-arrow-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:arrow?])
      (define-values (prints tx ty width height)
        (geo-arrow-metrics (geo:arrow-head-radius self) (geo:arrow-start.rad self)
                           (geo:arrow-shaft-thickness self) (geo:arrow-shaft-length self)
                           (geo:arrow-wing.rad self)))
      (values width height #false))))

(define geo-arrow-surface : Geo-Surface-Create
  (lambda [self]
    (with-asserts ([self geo:arrow?])
      (define-values (prints tx ty width height)
        (geo-arrow-metrics (geo:arrow-head-radius self) (geo:arrow-start.rad self)
                           (geo:arrow-shaft-thickness self) (geo:arrow-shaft-length self)
                           (geo:arrow-wing.rad self)))
      
      (dc_polygon create-abstract-surface width height prints tx ty #true #true
                  (current-stroke-source) (current-fill-source) (default-fill-rule)
                  (default-geometry-density)))))
