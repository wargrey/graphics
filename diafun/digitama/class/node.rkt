#lang typed/racket/base

(provide (all-defined-out))

(require "../node/style.rkt")
(require "../node/dc.rkt")
(require "../path/interface.rkt")
(require "../shared.rkt")

(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/text)
(require geofun/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diacls-block-interface : Dia-Path-Block-Create
  (lambda [node-key label style width height direction hint]
    (create-dia-node #:id node-key #:type 'Interface hint
                     #:fit-ratio 0.85 1.0
                     (geo-rectangle #:id (dia-node-shape-id node-key)
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height -0.125)
                     (and label
                          (geo-vc-append #:gapsize 2.0
                                         (geo-text #:color (dia-node-select-font-paint style)
                                                   "<<interface>>" default-label-tag-font)
                                         label)))))

(define diacls-block-class : Dia-Path-Block-Create
  (lambda [node-key label style width height direction hint]
    (create-dia-node #:id node-key #:type 'Class hint
                     #:fit-ratio 0.85 1.0
                     (geo-rectangle #:id (dia-node-shape-id node-key)
                                    #:stroke (dia-node-select-stroke-paint style)
                                    #:fill (dia-node-select-fill-paint style)
                                    width height -0.125)
                     label)))
