#lang typed/racket/base

(provide (all-defined-out))

(require "../block/style.rkt")
(require "../block/dc.rkt")
(require "../interface.rkt")
(require "../shared.rkt")

(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/text)
(require geofun/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diacls-block-interface : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (create-dia-block #:id block-key #:type 'Interface subtype
                      #:fit-region 0.85 1.0
                      #:create-with style [geo-rectangle width height '(12.5 %)]
                      (and caption
                           (geo-vc-append #:gapsize 2.0
                                          (geo-text #:color (dia-block-resolve-font-paint style)
                                                    "<<interface>>" default-label-tag-font)
                                          caption)))))

(define diacls-block-class : Dia-Block-Create
  (lambda [block-key caption style width height direction subtype]
    (create-dia-block #:id block-key #:type 'Class subtype
                      #:fit-region 0.85 1.0
                      #:create-with style [geo-rectangle width height '(12.5 %)]
                      caption)))
