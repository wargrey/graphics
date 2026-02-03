#lang typed/racket/base

(provide (all-defined-out))

(require "../dc.rkt")
(require "../style.rkt")

(require geofun/digitama/layer/void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-ghost-block : (->* (Symbol (Dia-Block-Style-Spec S)) (Any) Dia:Block)
  (lambda [id style [tags #false]]
    (create-dia-block #:id id tags
                      #:intersect dia-id-intersect
                      #:with-group style the-void-group)))
