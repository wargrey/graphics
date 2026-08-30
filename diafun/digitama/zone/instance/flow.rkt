#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/dc/rect)

(require "../dc.rkt")
(require "../self.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) default-flow-zone-build : (Dia-Zone-Builder S)
  (lambda [id type title style width height options mask]
    (define-values (zone offset)
      (create-dia-zone #:zone dia:zone
                       #:id id type
                       #:options options
                       #:create-with title style width height mask
                       (geo-rounded-rectangle)))

    (cons zone offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flow-zone-factory : Dia-Zone-Factory
  (make-dia-zone-factory #:builder default-flow-zone-build))
