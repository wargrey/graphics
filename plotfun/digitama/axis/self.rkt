#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/dc/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Ticks (U (Pairof Real Real) (Listof Real) False))
(define-type Plot-Axis-Position-Map (-> Flonum Float-Complex))

(define-type Plot-Cartesian-Position-Map
  (case-> [Flonum Flonum -> Float-Complex]
          [Float-Complex -> Float-Complex]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE: the `prop:property` forbids struct from being used in untyped code

(struct plot:axis geo:group
  ([origin : Float-Complex]
   [ticks : (Listof Real)]
   [map : Plot-Axis-Position-Map])
  #:type-name Plot:Axis
  #:transparent)

(struct plot:cartesian geo:group
  ([origin : Float-Complex]
   [xticks : Plot-Axis-Ticks]
   [yticks : Plot-Axis-Ticks]
   [map : Plot-Cartesian-Position-Map])
  #:type-name Plot:Cartesian
  #:transparent)
