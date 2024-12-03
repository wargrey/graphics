#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/dc/composite)
(require geofun/digitama/unsafe/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(struct dia:memory geo:table
  ([segment : Symbol]
   [range : (Pairof Index Index)]
   [state : String])
  #:type-name Dia:Memory
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dia:memory : (-> (Pairof (List Geo Geo) (Listof (List Geo Geo))) (Option Symbol)
                              (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real)
                              Symbol String (Pairof Index Index)
                              Dia:Memory)
  (lambda [siblings id col-anchors row-anchors col-gaps segment state range]
    (define ncols : Positive-Index 2)
    (define nrows : Positive-Index (max (length siblings) 1))
    
    (create-geometry-table dia:memory id #false #false
                           (for/vector : (Vectorof (GLayerof Geo)) ([g (in-list (apply append siblings))])
                             (geo-own-layer g))
                           ncols nrows col-anchors row-anchors col-gaps 0.0
                           segment range state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-snapshot-address : (-> (Listof Geo) Index)
  (lambda [snapshots]
    (or (and (pair? snapshots)
             (let ([lucky-guy (car snapshots)])
               (and (dia:memory? lucky-guy)
                    (car (dia:memory-range lucky-guy)))))
        0)))
