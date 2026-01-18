#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/self)
(require geofun/digitama/paint/self)
(require geofun/digitama/geometry/insets)

(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/void)

(require "../slot/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct expr:matrix expr:slot ()
  #:type-name Expr:Matrix
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-expr:matrix : (-> (Listof (Listof (Option Geo))) (Option Symbol)
                               Maybe-Stroke-Paint Maybe-Fill-Paint (Option Geo-Insets-Datum) (Option Geo-Insets-Datum)
                               (Geo-Config-Argof Geo-Pin-Anchor) (Geo-Config-Argof Geo-Pin-Anchor)
                               (Geo-Config-Argof Real) (Geo-Config-Argof Real)
                               Expr:Matrix)
  (lambda [siblings id border bgsource margin padding col-anchors row-anchors col-gaps row-gaps]
    (define nrows (length siblings))
    (define ncols (apply max 0 (map (inst length (Option Geo)) siblings)))
    
    (if (and (> ncols 0) (> nrows 0))
        (create-geometry-table expr:matrix id #false #false
                               #:margin margin #:padding padding #:border border #:background bgsource
                               (geo-siblings*->table siblings ncols the-void-layer)
                               ncols nrows col-anchors row-anchors col-gaps row-gaps)
        (make-expr:matrix (list (list #false)) id
                          border bgsource margin padding
                          col-anchors row-anchors col-gaps row-gaps))))
