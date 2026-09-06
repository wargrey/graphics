#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Marker-Name (U 'arrow 'diamond 'triangle 'dot 'odot 'point 'pixel 'bullet 'circle))

(define-type Geo-Marker (U geo-marker Geo-Marker-Name))
(define-type Option-Geo-Marker (Option Geo-Marker))
(define-type Maybe-Geo-Marker (U Option-Geo-Marker Void))

(struct geo-marker
  ([cfg : Geo-Marker-Config])
  #:transparent)

(define-struct geo-marker-config : Geo-Marker-Config
  ([fill? : Boolean #true]
   [thickness : (Option Flonum) #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-filled-cfg : Geo-Marker-Config (make-geo-marker-config))
(define geo-unfilled-cfg : Geo-Marker-Config (make-geo-marker-config #:fill? #false))
(define geo-hollow-cfg : Geo-Marker-Config (make-geo-marker-config #:fill? #false #:thickness -0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Tip-Datum (Immutable-Vector Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Float-Complex))
(define-type Geo-Tip-Placement (U 'inside 'outside 'center))

(define geo-marker-values : (-> geo-marker Geo-Tip-Datum
                                (Values Geo-Path-Prints
                                        Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                        Float-Complex Geo-Marker-Config))
  (lambda [master vals]
    (values (vector-ref vals 0)
            (vector-ref vals 1) (vector-ref vals 2)
            (vector-ref vals 3) (vector-ref vals 4)
            (vector-ref vals 5) (geo-marker-cfg master))))

(define markerdb : (Weak-HashTable Any Geo-Tip-Datum) (make-weak-hash))
