#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/convert)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/combine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-c-source : (Parameterof (Option Path)) (make-parameter #false))

(struct dia:ram geo:table
  ([source : (Option Path)]
   [segment : Symbol]
   [range : (Pairof Index Index)]
   [state : String])
  #:type-name Dia:RAM
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dia:ram : (-> (Pairof (Pairof Geo (Listof Geo)) (Listof (Pairof Geo (Listof Geo)))) (Option Symbol)
                           (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real)
                           Symbol String (Pairof Index Index)
                           Dia:RAM)
  (lambda [siblings id col-anchors row-anchors col-gaps segment state range]
    (define ncols : Positive-Index (max (length (car siblings)) 1))
    (define nrows : Positive-Index (max (length siblings) 1))
    
    (create-geometry-table dia:ram id #false #false
                           (for/vector : (Vectorof (GLayerof Geo)) ([g (in-list (apply append siblings))])
                             (geo-own-layer g))
                           ncols nrows col-anchors row-anchors col-gaps 0.0
                           (default-c-source)
                           segment range state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-snapshot-address : (-> (Listof Geo) Index)
  (lambda [snapshots]
    (or (and (pair? snapshots)
             (let ([lucky-guy (car snapshots)])
               (and (dia:ram? lucky-guy)
                    (car (dia:ram-range lucky-guy)))))
        0)))
