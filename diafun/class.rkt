#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/base.rkt"))
(provide (all-from-out "digitama/class/interface.rkt"))
(provide (all-from-out "digitama/class/self.rkt"))
(provide (all-from-out "digitama/class/style.rkt"))
(provide (rename-out [dia-path-simple-class geo-path-simple-class]))

(provide DiaCls-RelationShip-Type DiaCls-RelationShip-Identifier)
(provide default-diacls-block-identify default-diacls-arrow-identify)
(provide default-dia-node-margin create-dia-node)

(require geofun/paint)

(require geofun/digitama/convert)
(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/sticker)
(require geofun/digitama/layer/type)

(require "digitama/base.rkt")
(require "digitama/path/stick.rkt")
(require "digitama/path/self.rkt")

(require "digitama/class/self.rkt")
(require "digitama/class/style.rkt")
(require "digitama/class/interface.rkt")
(require "digitama/class/identifier.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-simple-class! stx)
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
       (let* ([goma (dia-initial-path pid gw gh ts home anchor ((default-diacls-block-width)))]
              [dia (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-path-simple-class dia args ...))))]))

(define-syntax (define-simple-class! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-simple-class! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-path-simple-class
  (lambda [#:id [id : (Option Symbol) #false]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:λblock [block-detect : Dia-Path-Block-Identifier default-diacls-block-identify]
           #:λarrow [arrow-detect : Dia-Path-Arrow-Identifier default-diacls-arrow-identify]
           #:λnode [make-node : (Option Dia-Path-Id->Node-Shape) #false]
           #:λnode-label [make-node-label : Dia-Path-Id->Node-Label default-dia-path-node-label-construct]
           #:relationship [class-type : (Option DiaCls-RelationShip-Identifier) (default-diacls-relationship-identifier)]
           #:λedge [make-edge : Dia-Path-Arrow->Edge default-dia-path-edge-construct]
           #:λedge-label [make-edge-label : Dia-Path-Arrow->Edge-Label default-dia-path-edge-label-construct]
           #:λfree-edge [make-free-track : Dia-Path-Free-Track->Edge default-dia-path-free-edge-construct]
           #:λfree-edge-label [make-free-label : Dia-Path-Free-Track->Edge-Label default-dia-path-free-edge-label-construct]
           #:ignore [ignore : (Listof Symbol) null]
           [self : Geo:Path]] : (U Dia:Class Geo:Path)
    (parameterize ([default-dia-node-base-style make-diacls-node-fallback-style]
                   [default-dia-edge-base-style make-diacls-edge-fallback-style]
                   [default-diacls-relationship-identifier class-type]
                   [current-master-path self])
      (define-values (nodes edges)
        (dia-path-stick self block-detect make-node make-node-label #false
                        arrow-detect make-edge make-edge-label
                        make-free-track make-free-label (default-diacls-free-track-style-make)
                        default-diacls-node-fallback-construct make-diacls-free-track-style
                        (geo:path-foot-infos self) ignore))
      (define stickers : (Listof (GLayerof Geo)) (append nodes edges))

      (if (pair? stickers)
          (let ([maybe-group (geo-path-try-extend/list stickers 0.0 0.0)])
            (create-geometry-group dia:class id #false #false
                                   #:border bdr #:background bg
                                   #:margin margin #:padding padding
                                   (cond [(or maybe-group) maybe-group]
                                         [else #;#:deadcode
                                               (let-values ([(Width Height) (geo-flsize self)])
                                                 (glayer-group Width Height stickers))])
                                   self))
          self))))
