#lang typed/racket/base

(provide (all-defined-out))

(require "../dc.rkt")

(require geofun/digitama/layer/void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S) dia-ghost-block : (->* (Symbol Symbol) (Any) Dia:Block)
  (lambda [id type [etags #false]]
    (create-dia-block #:id id #:tag type etags
                      #:with-group the-void-group)))
