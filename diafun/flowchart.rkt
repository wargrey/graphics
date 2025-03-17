#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out geofun/path))
(provide (all-from-out "digitama/shared.rkt"))
(provide (all-from-out "digitama/flowchart/self.rkt"))
(provide (all-from-out "digitama/flowchart/style.rkt"))
(provide (all-from-out "digitama/flowchart/interface.rkt"))
(provide (rename-out [dia-path-flow geo-path-flow]))

(provide default-diaflow-block-identify default-diaflow-arrow-identify)
(provide default-diaflow-node-label-construct default-diaflow-edge-construct default-diaflow-edge-label-construct)
(provide default-diaflow-free-edge-construct default-diaflow-free-edge-label-construct)
(provide default-dia-node-margin create-dia-node)

(require geofun/path)
(require geofun/paint)

(require geofun/digitama/convert)
(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)

(require "digitama/shared.rkt")
(require "digitama/node/dc.rkt")
(require "digitama/flowchart/self.rkt")
(require "digitama/flowchart/style.rkt")
(require "digitama/flowchart/identifier.rkt")
(require "digitama/flowchart/interface.rkt")
(require "digitama/flowchart/stick.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-flowchart! stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:grid-width  gw) #:defaults ([gw #'-0.80]))
              (~optional (~seq #:grid-height gh) #:defaults ([gh #'-0.50]))
              (~optional (~seq #:turn-scale  ts) #:defaults ([ts #'+0.05]))
              (~optional (~seq #:path-id pid) #:defaults ([pid #'#false]))
              (~optional (~seq #:start anchor) #:defaults ([anchor #''#:home]))
              (~optional (~seq #:at home) #:defaults ([home #'0.0+0.0i]))
              (~optional (~seq #:parameterize pexpr) #:defaults ([pexpr #'()])))
        ...
        [args ...] #:- move-expr ...)
     (syntax/loc stx
       (let* ([goma (dia-initial-path pid gw gh ts home anchor)]
              [chart (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-path-flow chart args ...))))]))

(define-syntax (define-flowchart! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-flowchart! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-flow
  (lambda [#:id [id : (Option Symbol) #false] #:start-name [start : (Option String) #false]
           #:path-operator [path-op : (Option Geo-Pin-Operator) #false] #:flow-operator [flow-op : (Option Geo-Pin-Operator) #false] 
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:λblock [block-detect : DiaFlow-Block-Identifier default-diaflow-block-identify]
           #:λarrow [arrow-detect : DiaFlow-Arrow-Identifier default-diaflow-arrow-identify]
           #:λnode [make-node : (Option DiaFlow-Id->Node-Shape) #false]
           #:λnode-label [make-node-label : DiaFlow-Id->Node-Label default-diaflow-node-label-construct]
           #:λedge [make-edge : DiaFlow-Arrow->Edge default-diaflow-edge-construct]
           #:λedge-label [make-edge-label : DiaFlow-Arrow->Edge-Label default-diaflow-edge-label-construct]
           #:λfree-edge [make-free-track : DiaFlow-Free-Track->Edge default-diaflow-free-edge-construct]
           #:λfree-edge-label [make-free-label : DiaFlow-Free-Track->Edge-Label default-diaflow-free-edge-label-construct]
           #:ignore [ignore : (Listof Symbol) null]
           [self : Geo:Path]] : (U Dia:Flow Geo:Path)
    (parameterize ([default-dia-node-base-style make-diaflow-node-fallback-style]
                   [default-dia-edge-base-style make-diaflow-edge-fallback-style]
                   [default-diaflow-canonical-start-name (or start (default-diaflow-canonical-start-name))]
                   [current-master-path self])
      (define stickers : (Listof (GLayerof Geo))
        (diaflow-stick self block-detect make-node make-node-label arrow-detect make-edge make-edge-label
                       make-free-track make-free-label (geo:path-foot-infos self) ignore))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (create-geometry-group dia:flow id path-op flow-op
                                   #:border bdr #:background bg
                                   #:margin margin #:padding padding
                                   (cond [(or maybe-group) maybe-group]
                                         [else #;#:deadcode
                                               (let-values ([(Width Height) (geo-flsize self)])
                                                 (glayer-group Width Height stickers))])
                                   self))
          self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flow-node
  (lambda [#:id [id : (Option Symbol) #false] #:scale [scale : Real 0.5]
           #:λblock [block-detect : DiaFlow-Block-Identifier default-diaflow-block-identify]
           #:λnode [make-node : (Option DiaFlow-Id->Node-Shape) #false]
           #:λnode-label [make-node-label : DiaFlow-Id->Node-Label default-diaflow-node-label-construct]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Node)
    (define ns : Nonnegative-Flonum (if (> scale 0) (real->double-flonum scale) 1.0))
    (parameterize ([default-diaflow-block-width  (* ((default-diaflow-block-width))  ns)]
                   [default-diaflow-block-height (* ((default-diaflow-block-height)) ns)]
                   [default-dia-node-margin (* (default-dia-node-margin) ns)]
                   [default-dia-node-base-style make-diaflow-node-fallback-style]
                   [current-master-path #false])
      (dia-make-node block-detect make-node make-node-label
                     (cond [(symbol? caption) caption]
                           [(keyword? caption) caption]
                           [(string? caption) (string->symbol caption)]
                           [else (string->symbol (format "~a" caption))])
                     direction
                     null))))
