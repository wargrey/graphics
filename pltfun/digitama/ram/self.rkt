#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-c-source : (Parameterof (Option Path)) (make-parameter #false))

(struct plt:ram geo:table
  ([source : (Option Path)]
   [segment : Symbol]
   [range : (Pairof Index Index)]
   [state : String])
  #:type-name Plt:RAM
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-plt:ram : (-> (Pairof (Pairof (Option Geo) (Listof Geo))
                                   (Listof (Pairof Geo (Listof Geo))))
                           (Option Symbol)
                           (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Real)
                           Symbol String (Pairof Index Index)
                           Plt:RAM)
  (lambda [siblings id col-anchors row-anchors col-gaps segment state range]
    (define ncols : Positive-Index (apply max 1 ((inst map Index (Listof (Option Geo))) length siblings)))
    (define nrows : Positive-Index (max (length siblings) 1))

    (create-geometry-table plt:ram id #false #false
                           (geo-siblings*->table siblings ncols the-void-layer)
                           ncols nrows col-anchors row-anchors col-gaps 0.0
                           (default-c-source) segment range state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-snapshot-address : (-> (Listof Geo) Index)
  (lambda [snapshots]
    (or (and (pair? snapshots)
             (let ([lucky-guy (car snapshots)])
               (and (plt:ram? lucky-guy)
                    (car (plt:ram-range lucky-guy)))))
        0)))
