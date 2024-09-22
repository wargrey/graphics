#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/style/flow.rkt" "digitama/interface/flow.rkt"))
(provide (all-from-out geofun/path))
(provide (rename-out [dia-path-flow geo-path-flow]))

(provide default-diaflow-block-identify)
(provide default-diaflow-node-construct)
(provide default-diaflow-arrow-construct)
(provide default-diaflow-arrow-label-construct)

(require geofun/path)

(require geofun/digitama/dc/path)
(require geofun/digitama/composite)
(require geofun/digitama/convert)

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)
(require geofun/digitama/dc/composite)

(require "digitama/flowchart.rkt")
(require "digitama/style/flow.rkt")
(require "digitama/identifier/flow.rkt")
(require "digitama/interface/flow.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-flow : (->* (Geo:Path)
                             (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator) #:trusted-anchors (Option Geo-Trusted-Anchors)
                              #:λblock DiaFlow-Block-Identifier #:λnode DiaFlow-Anchor->Node
                              #:λarrow DiaFlow-Arrow->Edge #:λlabel DiaFlow-Arrow-Label-Sticker
                              #:start-name (Option String))
                             (U Geo:Group Geo:Path))
  (lambda [#:id [id #false] #:operator [op #false] #:trusted-anchors [trusted-anchors #false] #:start-name [start #false]
           #:λblock [block-detect default-diaflow-block-identify] #:λnode [make-node default-diaflow-node-construct]
           #:λarrow [make-arrow default-diaflow-arrow-construct] #:λlabel [make-label default-diaflow-arrow-label-construct]
           self]
    (parameterize ([default-dia-node-base-style make-diaflow-node-base-style]
                   [default-dia-edge-base-style make-diaflow-edge-base-style]
                   [default-diaflow-canonical-start-name (or start (default-diaflow-canonical-start-name))])
      (define node-sticker : Geo-Anchor->Sticker
        (λ [master anchor pos Width Height]
          (define style (block-detect anchor))
          (and style (make-node master anchor pos (car style) (cdr style)))))

      (define stickers : (Listof (GLayerof Geo))
        (diaflow-stick self node-sticker make-arrow
                       make-label (geo:path-foot-infos self)
                       trusted-anchors))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (make-geo:group (or id (gensym 'dia:flow:)) op
                            (cond [(or maybe-group) maybe-group]
                                  [else #;#:deadcode
                                        (let-values ([(Width Height) (geo-flsize self)])
                                          (vector-immutable Width Height stickers))])))
          self))))
