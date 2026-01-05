#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/track/base.rkt"))
(provide (all-from-out "digitama/class/interface.rkt"))
(provide (all-from-out "digitama/class/self.rkt"))
(provide (all-from-out "digitama/class/style.rkt"))

(require geofun/digitama/dc/track)
(require geofun/digitama/dc/composite)

(require geofun/digitama/layer/type)
(require geofun/digitama/layer/merge)
(require geofun/digitama/layer/combine)

(require "digitama/track/dc.rkt")
(require "digitama/track/base.rkt")
(require "digitama/track/sticker.rkt")

(require "digitama/class/self.rkt")
(require "digitama/class/style.rkt")
(require "digitama/class/interface.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-simple-class! stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:grid-width  gw) #:defaults ([gw #''(80 %)]))
              (~optional (~seq #:grid-height gh) #:defaults ([gh #''(50 %)]))
              (~optional (~seq #:turn-scale  ts) #:defaults ([ts #'+0.05]))
              (~optional (~seq #:path-id pid) #:defaults ([pid #'#false]))
              (~optional (~seq #:start anchor) #:defaults ([anchor #''#:home]))
              (~optional (~seq #:at home) #:defaults ([home #'0.0+0.0i]))
              (~optional (~seq #:parameterize pexpr) #:defaults ([pexpr #'()])))
        ...
        [args ...] #:- move-expr ...)
     (syntax/loc stx
       (let* ([goma (dia-initial-track pid gw gh ts home anchor ((default-diacls-block-width)))]
              [dia (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-track-simple-class dia args ...))))]))

(define-syntax (define-simple-class! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-simple-class! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-simple-class
  (lambda [#:id [id : (Option Symbol) #false]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diacls-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier default-diacls-track-identify]
           #:block-backstop [block-backstop : Dia-Block-Backstop-Style (make-diacls-block-backstop-style)]
           #:track-backstop [track-backstop : Dia-Track-Backstop-Style (make-diacls-track-backstop-style)]
           #:λblock [make-block : (Option Dia-Anchor->Block) #false]
           #:λcaption [make-caption : Dia-Anchor->Caption default-dia-anchor->caption]
           #:relationship [class-type : (Option DiaCls-RelationShip-Identifier) (default-diacls-relationship-identifier)]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:λfree-path [make-free-track : Dia-Free-Track->Path default-dia-free-track->path]
           #:λfree-label [make-free-label : Dia-Free-Track->Label default-dia-free-track->label]
           #:ignore [ignore : (Listof Symbol) null]
           [self : Geo:Track]] : Dia:Class
    (parameterize ([default-diacls-relationship-identifier class-type]
                   [current-master-track self])
      (define-values (blocks paths)
        (dia-track-stick self block-detect make-block make-caption #false
                         track-detect make-path make-label
                         make-free-track make-free-label (default-diacls-free-track-style-make)
                         default-diacls-block-fallback-construct make-diacls-free-track-style
                         block-backstop track-backstop (geo:track-foot-infos self) ignore))
      (define stickers : (Listof (GLayerof Geo)) (append blocks paths))

      (create-geometry-group dia:class id #false #false
                             #:border bdr #:background bg
                             #:margin margin #:padding padding
                             (if (pair? stickers)
                                 (let ([maybe-group (geo-layers-try-extend stickers 0.0 0.0)])
                                   (cond [(or maybe-group) maybe-group]
                                         [else #;#:deadcode
                                               (let-values ([(Width Height) (geo-flsize self)])
                                                 (glayer-group Width Height stickers))]))
                                 #;'#:deadcode (geo-own-layers self))
                             self))))
