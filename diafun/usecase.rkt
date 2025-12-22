#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/base.rkt"))
(provide (all-from-out "digitama/usecase/interface.rkt"))
(provide (all-from-out "digitama/usecase/self.rkt"))
(provide (all-from-out "digitama/usecase/style.rkt"))

(provide default-diauc-block-identify default-diauc-track-identify)
(provide default-dia-block-margin create-dia-block)

(require geofun/paint)

(require geofun/digitama/convert)
(require geofun/digitama/dc/track)
(require geofun/digitama/dc/composite)
(require geofun/digitama/layer/merge)
(require geofun/digitama/layer/type)

(require "digitama/base.rkt")
(require "digitama/track/sticker.rkt")
(require "digitama/track/self.rkt")

(require "digitama/usecase/self.rkt")
(require "digitama/usecase/style.rkt")
(require "digitama/usecase/interface.rkt")
(require "digitama/usecase/identifier.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-use-case! stx)
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
       (let* ([goma (dia-initial-track pid gw gh ts home anchor ((default-diauc-block-width)))]
              [dia (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-track-use-case dia args ...))))]))

(define-syntax (define-use-case! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-use-case! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-use-case
  (lambda [#:id [id : (Option Symbol) #false]
           #:base-operator [base-op : (Option Geo-Pin-Operator) #false]
           #:operator [sibs-op : (Option Geo-Pin-Operator) #false] 
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diauc-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier default-diauc-track-identify]
           #:λblock [make-block : (Option Dia-Anchor->Block) #false]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:block-desc [block-desc : (Option Dia-Block-Describe) #false]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:λfree-path [make-free-track : Dia-Free-Track->Path default-dia-free-track->path]
           #:λfree-label [make-free-label : Dia-Free-Track->Label default-dia-free-track->label]
           #:ignore [ignore : (Listof Symbol) null]
           [self : Geo:Track]] : (U Dia:Use-Case Geo:Track)
    (parameterize ([default-dia-block-base-style make-diauc-block-fallback-style]
                   [default-dia-track-base-style make-diauc-track-fallback-style]
                   [current-master-track self])
      (define-values (blocks paths)
        (dia-track-stick self block-detect make-block make-brief block-desc
                         track-detect make-path make-label
                         make-free-track make-free-label (default-diauc-free-track-style-make)
                         default-diauc-block-fallback-construct make-diauc-free-track-style
                         (geo:track-foot-infos self) ignore))
      (define stickers : (Listof (GLayerof Geo)) (append paths blocks))

      (if (pair? stickers)
          (let ([maybe-group (geo-layers-try-extend stickers 0.0 0.0)])
            (create-geometry-group dia:use-case id base-op sibs-op
                                   #:border bdr #:background bg
                                   #:margin margin #:padding padding
                                   (cond [(or maybe-group) maybe-group]
                                         [else #;#:deadcode
                                               (let-values ([(Width Height) (geo-flsize self)])
                                                 (glayer-group Width Height stickers))])
                                   self))
          self))))
