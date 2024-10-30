#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out geofun/path))
(provide (all-from-out "digitama/flowchart/self.rkt"))
(provide (all-from-out "digitama/flowchart/style.rkt"))
(provide (all-from-out "digitama/flowchart/interface.rkt"))
(provide (rename-out [dia-path-flow geo-path-flow]))

(provide default-diaflow-block-identify default-diaflow-arrow-identify)
(provide default-diaflow-node-construct default-diaflow-node-label-construct)
(provide default-diaflow-edge-construct default-diaflow-edge-label-construct)
(provide default-diaflow-free-edge-construct default-diaflow-free-edge-label-construct)

(require digimon/metrics)
(require geofun/path)
(require geofun/paint)

(require geofun/digitama/composite)
(require geofun/digitama/convert)

(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)

(require "digitama/flowchart/self.rkt")
(require "digitama/flowchart/style.rkt")
(require "digitama/flowchart/identifier.rkt")
(require "digitama/flowchart/interface.rkt")
(require "digitama/flowchart/stick.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-flowchart! stx)
  (syntax-parse stx #:literals []
    [(_ name
        (~alt (~optional (~seq #:grid-width  gw) #:defaults ([gw #'-0.80]))
              (~optional (~seq #:grid-height gh) #:defaults ([gh #'-0.50]))
              (~optional (~seq #:turn-scale  ts) #:defaults ([ts #'+0.05]))
              (~optional (~seq #:at home) #:defaults ([home #'0])))
        ...
        [args ...] #:- move-expr ...)
     (syntax/loc stx
       (define name
         (dia-path-flow
          (with-gomamon!
              (let* ([grid-width  (~length gw (default-diaflow-block-width))]
                     [grid-height (~length gh grid-width)]
                     [scale (make-rectangular ts (* ts (/ grid-width grid-height)))])
                (make-gomamon #:T-scale scale #:U-scale scale #:at home
                              grid-width grid-height))
            move-expr ...)
          args ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-flow : (->* (Geo:Path)
                             (#:id (Option Symbol) #:draw-operator (Option Geo-Pin-Operator) #:start-name (Option String)
                              #:border Maybe-Stroke-Paint #:background Maybe-Fill-Paint
                              #:margin (Option Geo-Frame-Blank-Datum) #:padding (Option Geo-Frame-Blank-Datum)
                              #:λblock DiaFlow-Block-Identifier #:λarrow DiaFlow-Arrow-Identifier
                              #:λnode DiaFlow-Anchor->Node-Shape #:λnode-label DiaFlow-Anchor->Node-Label
                              #:λedge DiaFlow-Arrow->Edge #:λedge-label DiaFlow-Arrow->Edge-Label
                              #:λfree-edge DiaFlow-Free-Track->Edge #:λfree-edge-label DiaFlow-Free-Track->Edge-Label)
                             (U Dia:Flow Geo:Path))
  (lambda [#:id [id #false] #:draw-operator [op #false] #:start-name [start #false]
           #:border [bdr #false] #:background [bg #false] #:margin [margin #false] #:padding [padding #false]
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
            (create-geometry-group dia:flow id op
                                   #:border bdr #:background bg
                                   #:margin margin #:padding padding
                                   (cond [(or maybe-group) maybe-group]
                                         [else #;#:deadcode
                                               (let-values ([(Width Height) (geo-flsize self)])
                                                 (glayer-group Width Height stickers))])
                                   self))
          self))))
