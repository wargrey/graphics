#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out geofun/path))
(provide (all-from-out "digitama/shared.rkt"))
(provide (all-from-out "digitama/path/interface.rkt"))
(provide (all-from-out "digitama/usecase/interface.rkt"))
(provide (all-from-out "digitama/usecase/self.rkt"))
(provide (all-from-out "digitama/usecase/style.rkt"))
(provide (rename-out [dia-path-use-case geo-path-use-case]))

(provide default-diauc-block-identify default-diauc-arrow-identify)
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

(require "digitama/path/interface.rkt")
(require "digitama/path/stick.rkt")
(require "digitama/path/self.rkt")

(require "digitama/usecase/self.rkt")
(require "digitama/usecase/style.rkt")
(require "digitama/usecase/interface.rkt")
(require "digitama/usecase/identifier.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-use-case! stx)
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
       (let* ([goma (dia-initial-path pid gw gh ts home anchor ((default-diauc-block-width)))]
              [chart (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-path-use-case chart args ...))))]))

(define-syntax (define-use-case! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-use-case! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-use-case
  (lambda [#:id [id : (Option Symbol) #false]
           #:path-operator [path-op : (Option Geo-Pin-Operator) #false] #:flow-operator [flow-op : (Option Geo-Pin-Operator) #false] 
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:λblock [block-detect : Dia-Path-Block-Identifier default-diauc-block-identify]
           #:λarrow [arrow-detect : Dia-Path-Arrow-Identifier default-diauc-arrow-identify]
           #:λnode [make-node : (Option Dia-Path-Id->Node-Shape) #false]
           #:λnode-label [make-node-label : Dia-Path-Id->Node-Label default-dia-path-node-label-construct]
           #:node-desc [node-desc : (Option Dia-Path-Id->Label-String) #false]
           #:λedge [make-edge : Dia-Path-Arrow->Edge default-dia-path-edge-construct]
           #:λedge-label [make-edge-label : Dia-Path-Arrow->Edge-Label default-dia-path-edge-label-construct]
           #:λfree-edge [make-free-track : Dia-Path-Free-Track->Edge default-dia-path-free-edge-construct]
           #:λfree-edge-label [make-free-label : Dia-Path-Free-Track->Edge-Label default-dia-path-free-edge-label-construct]
           #:ignore [ignore : (Listof Symbol) null]
           [self : Geo:Path]] : (U Dia:Use-Case Geo:Path)
    (parameterize ([default-dia-node-base-style make-diauc-node-fallback-style]
                   [default-dia-edge-base-style make-diauc-edge-fallback-style]
                   [current-master-path self])
      (define stickers : (Listof (GLayerof Geo))
        (dia-path-stick self block-detect make-node make-node-label node-desc
                        arrow-detect make-edge make-edge-label
                        make-free-track make-free-label (default-diauc-free-track-style-make)
                        default-diauc-node-fallback-construct make-diauc-free-track-style
                        (geo:path-foot-infos self) ignore))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (create-geometry-group dia:use-case id path-op flow-op
                                   #:border bdr #:background bg
                                   #:margin margin #:padding padding
                                   (cond [(or maybe-group) maybe-group]
                                         [else #;#:deadcode
                                               (let-values ([(Width Height) (geo-flsize self)])
                                                 (glayer-group Width Height stickers))])
                                   self))
          self))))
