#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/spacing)

(require geofun/digitama/dc/plain)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/void)
(require geofun/digitama/layer/combine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:matrix geo:table ()
  #:type-name Dia:Matrix
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-dia:matrix : (-> (Listof (Listof (Option Geo))) (Option Symbol) Index Index
                              Maybe-Stroke-Paint Maybe-Fill-Paint (Option Geo-Spacing) (Option Geo-Spacing)
                              (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor)
                              (Geo-Config-Argof Real) (Geo-Config-Argof Real)
                              Dia:Matrix)
  (lambda [siblings id ncols nrows border bgsource margin padding col-anchors row-anchors col-gaps row-gaps]
    (if (and (> ncols 0) (> nrows 0))
        (create-geometry-table dia:matrix id #false #false
                               #:margin margin #:padding padding #:border border #:background bgsource
                               (geo-siblings*->table siblings ncols the-void-layer)
                               ncols nrows col-anchors row-anchors col-gaps row-gaps)
        (make-dia:matrix (list (list #false)) id 1 1
                          border bgsource margin padding
                          col-anchors row-anchors col-gaps row-gaps))))
  