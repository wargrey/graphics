#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/base.rkt"))
(provide (all-from-out "digitama/flowchart/interface.rkt"))
(provide (all-from-out "digitama/flowchart/self.rkt"))
(provide (all-from-out "digitama/flowchart/style.rkt"))

(require geofun/digitama/self)
(require geofun/digitama/dc/track)
(require geofun/digitama/dc/composite)
(require geofun/digitama/paint/self)
(require geofun/digitama/layer/merge)
(require geofun/digitama/layer/type)

(require "digitama/base.rkt")
(require "digitama/track/self.rkt")
(require "digitama/track/sticker.rkt")

(require "digitama/flowchart/self.rkt")
(require "digitama/flowchart/style.rkt")
(require "digitama/flowchart/interface.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-flowchart! stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:grid-width  gw) #:defaults ([gw #''(80 %)]))
              (~optional (~seq #:grid-height gh) #:defaults ([gh #''(50 %)]))
              (~optional (~seq #:turn-scale  ts) #:defaults ([ts #'0.05]))
              (~optional (~seq #:path-id pid) #:defaults ([pid #'#false]))
              (~optional (~seq #:start anchor) #:defaults ([anchor #''#:home]))
              (~optional (~seq #:at home) #:defaults ([home #'0.0+0.0i]))
              (~optional (~seq #:parameterize pexpr) #:defaults ([pexpr #'()])))
        ...
        [args ...] #:- move-expr ...)
     (syntax/loc stx
       (let* ([goma (dia-initial-track pid gw gh ts home anchor ((default-diaflow-block-width)))]
              [chart (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-track-flow chart args ...))))]))

(define-syntax (define-flowchart! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-flowchart! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-flow
  (lambda [#:id [id : (Option Symbol) #false] #:start-name [start : (Option String) #false]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Frame-Blank-Datum) #false] #:padding [padding : (Option Geo-Frame-Blank-Datum) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier default-diaflow-track-identify]
           #:block-backstop [block-backstop : Dia-Block-Backstop-Style (make-diaflow-block-backstop-style)]
           #:track-backstop [track-backstop : Dia-Track-Backstop-Style (make-diaflow-track-backstop-style)]
           #:λblock [make-block : (Option Dia-Anchor->Block) #false]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:block-desc [block-desc : (Option Dia-Block-Describe) #false]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:λfree-path [make-free-path : Dia-Free-Track->Path default-dia-free-track->path]
           #:λfree-label [make-free-label : Dia-Free-Track->Label default-dia-free-track->label]
           #:ignore [ignore : (Listof Symbol) null]
           [self : Geo:Track]] : (U Dia:Flow Geo:Track)
    (parameterize ([default-diaflow-canonical-start-name (or start (default-diaflow-canonical-start-name))]
                   [current-master-track self])
      (define-values (blocks tracks)
        (dia-track-stick self block-detect make-block make-brief block-desc
                         track-detect make-path make-label
                         make-free-path make-free-label (default-diaflow-free-track-style-make)
                         default-diaflow-block-fallback-construct make-diaflow-free-track-style
                         block-backstop track-backstop (geo:track-foot-infos self) ignore))
      (define stickers : (Listof (GLayerof Geo)) (append tracks blocks))

      (if (pair? stickers)
          (let ([maybe-group (geo-layers-try-extend stickers 0.0 0.0)])
            (create-geometry-group dia:flow id #false #false
                                   #:border bdr #:background bg
                                   #:margin margin #:padding padding
                                   (cond [(or maybe-group) maybe-group]
                                         [else #;#:deadcode
                                               (let-values ([(Width Height) (geo-flsize self)])
                                                 (glayer-group Width Height stickers))])
                                   self))
          self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flow-block
  (lambda [#:id [id : (Option Symbol) #false] #:scale [scale : Real 0.5]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:block-backstop [block-backstop : Dia-Block-Backstop-Style (make-diaflow-block-backstop-style)]
           #:λblock [make-block : (Option Dia-Anchor->Block) #false]
           #:λbrief [make-brief : Dia-Anchor->Brief default-dia-anchor->brief]
           #:block-desc [block-desc : (Option Dia-Block-Describe) #false]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (define ns : Nonnegative-Flonum (if (> scale 0) (real->double-flonum scale) 1.0))
    (parameterize ([default-diaflow-block-width  (* ((default-diaflow-block-width))  ns)]
                   [default-diaflow-block-height (* ((default-diaflow-block-height)) ns)]
                   [default-dia-block-margin (* (default-dia-block-margin) ns)]
                   [current-master-track #false])
      (dia-block-make block-detect make-block make-brief block-desc
                      default-diaflow-block-fallback-construct block-backstop
                      (cond [(symbol? caption) caption]
                            [(keyword? caption) caption]
                            [(string? caption) (string->symbol caption)]
                            [else (string->symbol (format "~a" caption))])
                      direction null))))
