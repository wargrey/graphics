#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/plain)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/void)
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
(define make-dia:ram : (-> (Pairof (Pairof (Option Geo) (Listof Geo))
                                   (Listof (Pairof Geo (Listof Geo))))
                           (Option Symbol)
                           (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real)
                           Symbol String (Pairof Index Index)
                           Dia:RAM)
  (lambda [siblings id col-anchors row-anchors col-gaps segment state range]
    (define ncols : Positive-Index (apply max 1 ((inst map Index (Listof (Option Geo))) length siblings)))
    (define nrows : Positive-Index (max (length siblings) 1))

    (create-geometry-table dia:ram id #false #false
                           (geo-siblings*->table siblings ncols the-void-layer)
                           ncols nrows col-anchors row-anchors col-gaps 0.0
                           (default-c-source) segment range state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-snapshot-address : (-> (Listof Geo) Index)
  (lambda [snapshots]
    (or (and (pair? snapshots)
             (let ([lucky-guy (car snapshots)])
               (and (dia:ram? lucky-guy)
                    (car (dia:ram-range lucky-guy)))))
        0)))
