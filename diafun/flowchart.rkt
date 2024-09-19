#lang typed/racket/base

(provide (all-defined-out))
(provide DiaFlow-Anchor->Node DiaFlow-Arrow->Edge DiaFlow-Arrow-Label-Stickers)
(provide (all-from-out "digitama/style/flow.rkt"))
(provide (all-from-out geofun/path))
(provide (rename-out [dia-path-flow geo-path-flow]))

(provide default-diaflow-node-constructor)
(provide default-diaflow-arrow-constructor)

(require geofun/path)

(require geofun/digitama/composite)
(require geofun/digitama/convert)

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)
(require geofun/digitama/dc/composite)

(require "digitama/flowchart.rkt")
(require "digitama/style/flow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-flow : (->* (Geo:Path)
                             (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator) #:trusted-anchors (Option Geo-Trusted-Anchors)
                              #:λnode DiaFlow-Anchor->Node #:λarrow DiaFlow-Arrow->Edge #:λlabel (Option DiaFlow-Arrow-Label-Stickers)
                              #:start-name (Option String))
                             (U Geo:Group Geo:Path))
  (lambda [#:id [id #false] #:operator [op #false] #:trusted-anchors [trusted-anchors #false] #:start-name [start #false]
           #:λnode [make-node default-diaflow-node-constructor] #:λarrow [make-arrow default-diaflow-arrow-constructor]
           #:λlabel [make-label #false]
           self]
    (parameterize ([default-dia-node-base-style make-diaflow-node-base-style]
                   [default-dia-edge-base-style make-diaflow-edge-base-style]
                   [default-diaflow-canonical-start-name (or start (default-diaflow-canonical-start-name))])
      (define stickers : (Listof (GLayerof Geo)) (diaflow-stick self make-node make-arrow make-label trusted-anchors))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (make-geo:group (or id (gensym 'dia:flow:)) op
                            (cond [(or maybe-group) maybe-group]
                                  [else #;#:deadcode
                                        (let-values ([(Width Height) (geo-flsize self)])
                                          (vector-immutable Width Height stickers))])))
          self))))
