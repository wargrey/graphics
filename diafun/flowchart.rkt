#lang typed/racket/base

(provide (all-defined-out) DiaFlow-Arrow->Edge)
(provide (all-from-out "digitama/style/flow.rkt"))
(provide (all-from-out geofun/path))
(provide (rename-out [dia-path-flow geo-path-flow]))

(provide default-diaflow-node-constructor)
(provide default-diaflow-arrow-constructor)
(provide geo-edge geo:edge? Geo:Edge)

(require geofun/path)

(require geofun/digitama/composite)
(require geofun/digitama/convert)

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)

(require geofun/digitama/dc/composite)
(require geofun/digitama/dc/edge)

(require "digitama/flowchart.rkt")
(require "digitama/style/flow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-flow : (->* (Geo:Path)
                             (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator) #:trusted-anchors (Option Geo-Trusted-Anchors)
                              #:λnode Geo-Anchor->Sticker #:λarrow DiaFlow-Arrow->Edge #:start-name (Option String))
                             (U Geo:Group Geo:Path))
  (lambda [#:id [id #false] #:operator [op #false] #:trusted-anchors [trusted-anchors #false] #:start-name [start #false]
           #:λnode [make-node default-diaflow-node-constructor] #:λarrow [make-arrow default-diaflow-arrow-constructor]
           self]
    (parameterize ([default-geo-edge-base-style make-diaflow-edge-style]
                   [default-diaflow-canonical-start-name (or start (default-diaflow-canonical-start-name))])
      (define stickers : (Listof (GLayerof Geo)) (diaflow-stick self make-node make-arrow trusted-anchors))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (make-geo:group (or id (gensym 'dia:flow:)) op
                            (cond [(or maybe-group) maybe-group]
                                  [else #;#:deadcode
                                        (let-values ([(Width Height) (geo-flsize self)])
                                          (vector-immutable Width Height stickers))])))
          self))))
