#lang typed/racket/base

(provide (all-defined-out))

(require digimon/struct)

(require "../../geometry/footprint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Tip-Placement (U 'inside 'outside 'center))
(define-type Geo-Tip-Shape (U 'arrow 'diamond 'triangle 'dot 'point 'pixel 'bullet 'circle))
(define-type Geo-Tip (U geo-tip Geo-Tip-Shape))
(define-type Option-Geo-Tip (Option Geo-Tip))
(define-type Maybe-Geo-Tip (U Option-Geo-Tip Void))

(struct geo-tip
  ([cfg : Geo-Tip-Config])
  #:transparent)

(define-struct geo-tip-config : Geo-Tip-Config
  ([fill? : Boolean #true]
   [thickness : (Option Flonum) #false])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-filled-cfg : Geo-Tip-Config (make-geo-tip-config))
(define geo-unfilled-cfg : Geo-Tip-Config (make-geo-tip-config #:fill? #false))
(define geo-hollow-cfg : Geo-Tip-Config (make-geo-tip-config #:fill? #false #:thickness -0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Tip-Datum (Immutable-Vector Geo-Path-Prints Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Float-Complex))

(define geo-tip-values : (-> geo-tip Geo-Tip-Datum
                             (Values Geo-Path-Prints
                                     Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                     Float-Complex Geo-Tip-Config))
  (lambda [master vals]
    (values (vector-ref vals 0)
            (vector-ref vals 1) (vector-ref vals 2)
            (vector-ref vals 3) (vector-ref vals 4)
            (vector-ref vals 5) (geo-tip-cfg master))))

(define tip-db : (Weak-HashTable Any Geo-Tip-Datum) (make-weak-hash))
