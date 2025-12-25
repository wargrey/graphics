#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/dc/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Plot-Axis-Ticks (U (Pairof Real Real) (Listof Real) False))

(define-type Plot-Position-Transform
  (case-> [Flonum Flonum -> Float-Complex]
          [Float-Complex -> Float-Complex]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct plot:line geo:group
  ([origin : Float-Complex]
   [ticks : (Listof Real)]
   [map : Plot-Position-Transform])
  #:type-name Plot:Line
  #:transparent)

(struct plot:cartesian geo:group
  ([origin : Float-Complex]
   [xticks : Plot-Axis-Ticks]
   [yticks : Plot-Axis-Ticks]
   [map : Plot-Position-Transform])
  #:type-name Plot:Cartesian
  #:transparent)
