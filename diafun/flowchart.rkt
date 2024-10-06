#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out geofun/path))
(provide (all-from-out "digitama/flowchart/style.rkt"))
(provide (all-from-out "digitama/flowchart/interface.rkt"))
(provide (rename-out [dia-path-flow geo-path-flow]))

(provide default-diaflow-block-identify default-diaflow-arrow-identify)
(provide default-diaflow-node-construct default-diaflow-node-label-construct)
(provide default-diaflow-edge-construct default-diaflow-edge-label-construct)
(provide default-diaflow-free-edge-construct default-diaflow-free-edge-label-construct)

(require digimon/metrics)
(require geofun/path)

(require geofun/digitama/dc/path)
(require geofun/digitama/composite)
(require geofun/digitama/convert)

(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)
(require geofun/digitama/dc/composite)

(require "digitama/flowchart/node.rkt")
(require "digitama/flowchart/style.rkt")
(require "digitama/flowchart/identifier.rkt")
(require "digitama/flowchart/interface.rkt")
(require "digitama/flowchart/stick.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-flowchart! stx)
  (syntax-parse stx #:literals []
    [(_ name
        (~alt (~optional (~seq #:grid-width  gw) #:defaults ([gw #'-0.80]))
              (~optional (~seq #:grid-height gh) #:defaults ([gh #'-0.50]))
              (~optional (~seq #:turn-scale  ts) #:defaults ([ts #'+0.05])))
        ...
        [args ...] #:- move-expr ...)
     (syntax/loc stx
       (define name : Gomamon
         (with-gomamon!
             (let* ([grid-width  (~length gw (default-diaflow-block-width))]
                    [grid-height (~length gh grid-width)]
                    [scale (make-rectangular ts (* ts (/ grid-width grid-height)))])
               (make-gomamon #:T-scale scale #:U-scale scale
                             grid-width grid-height
                             args ...))
           move-expr ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-flow : (->* (Geo:Path)
                             (#:id (Option Symbol) #:draw-operator (Option Geo-Pin-Operator) #:start-name (Option String)
                              #:λblock DiaFlow-Block-Identifier #:λarrow DiaFlow-Arrow-Identifier
                              #:λnode DiaFlow-Anchor->Node-Shape #:λnode-label DiaFlow-Anchor->Node-Label
                              #:λedge DiaFlow-Arrow->Edge #:λedge-label DiaFlow-Arrow->Edge-Label
                              #:λfree-edge DiaFlow-Free-Track->Edge #:λfree-edge-label DiaFlow-Free-Track->Edge-Label)
                             (U Geo:Group Geo:Path))
  (lambda [#:id [id #false] #:draw-operator [op #false] #:start-name [start #false]
           #:λblock [block-detect default-diaflow-block-identify] #:λarrow [arrow-detect default-diaflow-arrow-identify]
           #:λnode [make-node default-diaflow-node-construct] #:λnode-label [make-node-label default-diaflow-node-label-construct]
           #:λedge [make-edge default-diaflow-edge-construct] #:λedge-label [make-edge-label default-diaflow-edge-label-construct]
           #:λfree-edge [make-free-track default-diaflow-free-edge-construct] #:λfree-edge-label [make-free-label default-diaflow-free-edge-label-construct]
           self]
    (parameterize ([default-dia-node-base-style make-diaflow-node-base-style]
                   [default-dia-edge-base-style make-diaflow-edge-base-style]
                   [default-diaflow-canonical-start-name (or start (default-diaflow-canonical-start-name))])
      (define stickers : (Listof (GLayerof Geo))
        (diaflow-stick self block-detect make-node make-node-label arrow-detect make-edge make-edge-label
                       make-free-track make-free-label (geo:path-foot-infos self)))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (make-geo:group (or id (gensym 'dia:flow:)) op
                            (cond [(or maybe-group) maybe-group]
                                  [else #;#:deadcode
                                        (let-values ([(Width Height) (geo-flsize self)])
                                          (glayer-group Width Height stickers))])))
          self))))
