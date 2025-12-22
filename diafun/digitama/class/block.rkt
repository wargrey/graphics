#lang typed/racket/base

(provide (all-defined-out))

(require "../block/style.rkt")
(require "../block/dc.rkt")
(require "../track/interface.rkt")
(require "../shared.rkt")

(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/text)
(require geofun/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diacls-block-interface : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (create-dia-block #:id block-key #:type 'Interface subtype
                      #:fit-ratio 0.85 1.0
                      (geo-rectangle #:id (dia-block-shape-id block-key)
                                     #:stroke (dia-block-select-stroke-paint style)
                                     #:fill (dia-block-select-fill-paint style)
                                     width height '(12.5 %))
                      (and brief
                           (geo-vc-append #:gapsize 2.0
                                          (geo-text #:color (dia-block-select-font-paint style)
                                                    "<<interface>>" default-label-tag-font)
                                          brief)))))

(define diacls-block-class : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (create-dia-block #:id block-key #:type 'Class subtype
                      #:fit-ratio 0.85 1.0
                      (geo-rectangle #:id (dia-block-shape-id block-key)
                                     #:stroke (dia-block-select-stroke-paint style)
                                     #:fill (dia-block-select-fill-paint style)
                                     width height '(12.5 %))
                      brief)))
