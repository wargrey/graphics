#lang typed/racket/base

(provide (all-defined-out))

(require digimon/metrics)

(require "../paint.rkt")
(require "../../paint.rkt")

(require "../convert.rkt")
(require "../unsafe/dc/path.rkt")
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
(define geo-dart
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false] #:wing-angle [wing-angle : (Option Real) #false] #:radian? [radian? : Boolean #true]
           [radius : Real] [start : Real 0.0]] : Geo:Dart
    (define flradius : Nonnegative-Flonum (~length radius))
    (define wing-flangle (and wing-angle (~radian wing-angle radian?)))
    
    (create-geometry-object geo:dart
                            #:with [id (geo-draw-dart stroke pattern) geo-dart-extent (geo-shape-outline stroke)]
                            flradius (~radian start radian?) wing-flangle)))

(define geo-arrow
  (lambda [#:stroke [stroke : Maybe-Stroke-Paint (void)] #:fill [pattern : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false] #:radian? [radian? : Boolean #true]
           #:shaft-thickness [shaft-thickness : Real -0.3] #:wing-angle [wing-angle : (Option Real) #false]
           [head-radius : Real] [shaft-length : Real] [start : Real 0.0]] : Geo:Arrow
    (define rhead : Nonnegative-Flonum (~length head-radius))
    (define shaft-flthickness : Nonnegative-Flonum (~length shaft-thickness rhead))
    (define shaft-flength : Nonnegative-Flonum (~length shaft-length rhead))
    (define wing-flangle (and wing-angle (~radian wing-angle radian?)))
    
    (create-geometry-object geo:arrow
                            #:with [id (geo-draw-arrow stroke pattern) geo-arrow-extent (geo-shape-outline stroke)]
                            rhead shaft-flength shaft-flthickness (~radian start radian?) wing-flangle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-dart-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:dart?])
      (define-values (prints tx ty width height)
        (geo-dart-metrics (geo:dart-head-radius self) (geo:dart-start.rad self)
                          (geo:dart-wing.rad self)))
      (values width height #false))))

(define geo-draw-dart : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-stroke alt-fill]
    (λ [self cr x0 y0 width height]
      (when (geo:dart? self)
        (define-values (prints tx ty width height)
          (geo-dart-metrics (geo:dart-head-radius self) (geo:dart-start.rad self)
                            (geo:dart-wing.rad self)))
        
        (dc_polygon* cr (+ x0 tx) (+ y0 ty) width height prints
                     (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                     (default-fill-rule))))))

(define geo-arrow-extent : Geo-Calculate-Extent
  (lambda [self]
    (with-asserts ([self geo:arrow?])
      (define-values (prints tx ty width height)
        (geo-arrow-metrics (geo:arrow-head-radius self) (geo:arrow-start.rad self)
                           (geo:arrow-shaft-thickness self) (geo:arrow-shaft-length self)
                           (geo:arrow-wing.rad self)))
      (values width height #false))))

(define geo-draw-arrow : (-> Maybe-Stroke-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
 (lambda [alt-stroke alt-fill]
   (λ [self cr x0 y0 width height]
     (when (geo:arrow? self)
       (define-values (prints tx ty width height)
         (geo-arrow-metrics (geo:arrow-head-radius self) (geo:arrow-start.rad self)
                            (geo:arrow-shaft-thickness self) (geo:arrow-shaft-length self)
                            (geo:arrow-wing.rad self)))
       
       (dc_polygon* cr (+ x0 tx) (+ y0 ty) width height prints
                    (geo-select-stroke-paint alt-stroke) (geo-select-fill-source alt-fill)
                    (default-fill-rule))))))
