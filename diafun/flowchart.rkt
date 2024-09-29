#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/style/flow.rkt" "digitama/interface/flow.rkt"))
(provide (all-from-out geofun/path))
(provide (rename-out [dia-path-flow geo-path-flow]))

(provide default-diaflow-block-identify default-diaflow-node-construct)
(provide default-diaflow-arrow-construct default-diaflow-arrow-label-construct)
(provide default-diaflow-free-track-construct default-diaflow-free-track-label-construct)

(require digimon/metrics)
(require geofun/path)

(require geofun/digitama/dc/path)
(require geofun/digitama/composite)
(require geofun/digitama/convert)

(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)
(require geofun/digitama/dc/composite)

(require "digitama/flowchart.rkt")
(require "digitama/style/flow.rkt")
(require "digitama/identifier/flow.rkt")
(require "digitama/interface/flow.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-flowchart! stx)
  (syntax-parse stx #:literals []
    [(_ name
        (~alt (~optional (~seq #:grid-width  gw) #:defaults ([gw #'-0.618]))
              (~optional (~seq #:grid-height gh) #:defaults ([gh #'-1.618]))
              (~optional (~seq #:turn-scale  ts) #:defaults ([ts #'+0.030])))
        ...
        [args ...] #:- move-expr ...)
     (syntax/loc stx
       (define name : Dryland-Wani
         (with-dryland-wani!
             (let* ([grid-width  (~length gw (default-diaflow-block-width))]
                    [grid-height (~length gh (default-diaflow-block-height))]
                    [scale (make-rectangular ts (* ts (/ grid-width grid-height)))])
               (make-dryland-wani #:T-scale scale #:U-scale scale
                                  grid-width grid-height
                                  args ...))
           move-expr ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-flow : (->* (Geo:Path)
                             (#:id (Option Symbol) #:operator (Option Geo-Pin-Operator)
                              #:start-name (Option String) #:λblock DiaFlow-Block-Identifier
                              #:λnode DiaFlow-Anchor->Node-Shape #:λnode-label DiaFlow-Anchor->Node-Label
                              #:λarrow DiaFlow-Arrow->Edge #:λarrow-label DiaFlow-Arrow->Edge-Label
                              #:λfree-track DiaFlow-Free-Track->Edge #:λfree-track-label DiaFlow-Free-Track->Edge-Label)
                             (U Geo:Group Geo:Path))
  (lambda [#:id [id #false] #:operator [op #false] #:start-name [start #false] #:λblock [block-detect default-diaflow-block-identify]
           #:λnode [make-node default-diaflow-node-construct] #:λnode-label [make-node-label default-diaflow-node-label-construct]
           #:λarrow [make-arrow default-diaflow-arrow-construct] #:λarrow-label [make-arrow-label default-diaflow-arrow-label-construct]
           #:λfree-track [make-free-track default-diaflow-free-track-construct] #:λfree-track-label [make-free-label default-diaflow-free-track-label-construct]
           self]
    (parameterize ([default-dia-node-base-style make-diaflow-node-base-style]
                   [default-dia-edge-base-style make-diaflow-edge-base-style]
                   [default-diaflow-canonical-start-name (or start (default-diaflow-canonical-start-name))])
      (define node-sticker : Geo-Anchor->Sticker
        (λ [master anchor pos Width Height]
          (define blk-datum (block-detect anchor))
          (and blk-datum
               (let ([style (cadr blk-datum)])
                 (make-node master anchor
                            (make-node-label master anchor (car blk-datum) style pos)
                            style pos (caddr blk-datum))))))

      (define stickers : (Listof (GLayerof Geo))
        (diaflow-stick self node-sticker make-arrow make-arrow-label
                       make-free-track make-free-label (geo:path-foot-infos self)))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (make-geo:group (or id (gensym 'dia:flow:)) op
                            (cond [(or maybe-group) maybe-group]
                                  [else #;#:deadcode
                                        (let-values ([(Width Height) (geo-flsize self)])
                                          (vector-immutable Width Height stickers))])))
          self))))
