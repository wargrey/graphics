#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/track/base.rkt"))
(provide (all-from-out "digitama/flowchart/interface.rkt"))
(provide (all-from-out "digitama/flowchart/self.rkt"))
(provide (all-from-out "digitama/flowchart/style.rkt"))

(require geofun/digitama/dc/track)
(require geofun/digitama/geometry/trail)
(require geofun/digitama/geometry/spacing)

(require "digitama/track/dc.rkt")
(require "digitama/track/base.rkt")
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
  (lambda [#:id [id : (Option Symbol) #false] #:start-name [home-name : (Option String) #false]
           #:border [bdr : Maybe-Stroke-Paint #false] #:background [bg : Maybe-Fill-Paint #false]
           #:margin [margin : (Option Geo-Spacing) #false] #:padding [padding : (Option Geo-Spacing) #false]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:track-detect [track-detect : Dia-Track-Identifier default-diaflow-track-identify]
           #:block-backstop [block-backstop : Dia-Block-Backstop-Style (make-diaflow-block-backstop-style)]
           #:track-backstop [track-backstop : Dia-Track-Backstop-Style (make-diaflow-track-backstop-style)]
           #:λblock [make-block : (Option Dia-Anchor->Block) #false]
           #:λcaption [make-caption : Dia-Anchor->Caption default-dia-anchor->caption]
           #:block-desc [block-desc : (Option Dia-Block-Describe) #false]
           #:λpath [make-path : Dia-Track->Path default-dia-track->path]
           #:λlabel [make-label : Dia-Track->Label default-dia-track->label]
           #:λfree-path [make-free-path : Dia-Free-Track->Path default-dia-free-track->path]
           #:λfree-label [make-free-label : Dia-Free-Track->Label default-dia-free-track->label]
           #:ignore [ignore : (Listof Symbol) null]
           [self : Geo:Track]] : Dia:Flow
    (parameterize ([current-master-track self])
      (define-values (blocks tracks)
        (dia-track-stick self block-detect make-block make-caption
                         (dia-register-home-name (geo-trail-home-anchor (geo:track-trail self)) home-name block-desc)
                         track-detect make-path make-label
                         make-free-path make-free-label (default-diaflow-free-track-style-make)
                         default-diaflow-block-fallback-construct make-diaflow-free-track-style
                         block-backstop track-backstop (geo:track-foot-infos self) ignore))

      (create-dia-track dia:flow id
                        #:border bdr #:background bg
                        #:margin margin #:padding padding
                        self tracks blocks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flow-block
  (lambda [#:id [id : (Option Symbol) #false] #:scale [scale : Real 0.5]
           #:block-detect [block-detect : Dia-Block-Identifier default-diaflow-block-identify]
           #:block-backstop [block-backstop : Dia-Block-Backstop-Style (make-diaflow-block-backstop-style)]
           #:λblock [make-block : (Option Dia-Anchor->Block) #false]
           #:λcaption [make-caption : Dia-Anchor->Caption default-dia-anchor->caption]
           #:block-desc [block-desc : (Option Dia-Block-Describe) #false]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (define ns : Nonnegative-Flonum (if (> scale 0) (real->double-flonum scale) 1.0))
    (parameterize ([default-diaflow-block-width  (* ((default-diaflow-block-width))  ns)]
                   [default-diaflow-block-height (* ((default-diaflow-block-height)) ns)]
                   [default-dia-block-margin (geo-spacing-scale (default-dia-block-margin) ns ns)]
                   [current-master-track #false])
      (dia-block-make block-detect make-block make-caption block-desc
                      default-diaflow-block-fallback-construct block-backstop
                      (cond [(symbol? caption) caption]
                            [(keyword? caption) caption]
                            [(string? caption) (string->symbol caption)]
                            [else (string->symbol (format "~a" caption))])
                      direction null))))
