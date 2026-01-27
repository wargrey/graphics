#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/track/base.rkt"))
(provide (all-from-out "digitama/track/freestyle.rkt"))
(provide (all-from-out "digitama/activity/self.rkt"))
(provide (all-from-out "digitama/activity/style.rkt"))
(provide (rename-out [dia-track-activate dia-track-act]
                     [dia-track-activate* dia-track-act*]))

(require geofun/digitama/dc/track)
(require geofun/digitama/track/trail)

(require "digitama/track/dc.rkt")
(require "digitama/track/base.rkt")
(require "digitama/track/realize.rkt")
(require "digitama/track/freestyle.rkt")
(require "digitama/track/interface.rkt")

(require "digitama/block/dc.rkt")
(require "digitama/block/realize.rkt")
(require "digitama/block/interface.rkt")

(require "digitama/activity/self.rkt")
(require "digitama/activity/style.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-activity-diagram! stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:grid-width   gw) #:defaults ([gw #'(&% 80)]))
              (~optional (~seq #:grid-height  gh) #:defaults ([gh #'(&% 50)]))
              (~optional (~seq #:turn-radius% ts) #:defaults ([ts #'0.05]))
              (~optional (~seq #:path-id pid) #:defaults ([pid #'#false]))
              (~optional (~seq #:start anchor) #:defaults ([anchor #''#:home]))
              (~optional (~seq #:at home) #:defaults ([home #'0.0+0.0i]))
              (~optional (~seq #:parameterize pexpr) #:defaults ([pexpr #'()])))
        ...
        [args ...] #:- move-expr ...)
     (syntax/loc stx
       (let* ([goma (dia-initial-track pid gw gh ts home anchor (default-act-block-width))]
              [chart (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-track-activate chart args ...))))]))

(define-syntax (define-activity-diagram! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-activity-diagram! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-activate
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:start-name [home-name : (Option String) #false]
           #:track-factory [track-factory : Act-Track-Factory (default-act-track-factory)]
           #:block-factory [block-factory : Act-Block-Factory (default-act-block-factory)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:block-desc [block-desc : (Option Act-Block-Describer) #false]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           [master : Dia-Track-Datum]] : Dia:Activity
    (define self (if (geo:track? master) master (dia:track-self master)))
    (parameterize ([current-master-track self])
      (create-dia-track dia:activity id self
                        #:frame frame
                        (dia-track-realize self track-factory free-factory block-factory
                                           (dia-register-home-name (geo-trail-home-anchor (geo:track-trail self)) home-name block-desc)
                                           (geo:track-foot-infos self)
                                           (real->double-flonum scale)
                                           (and opacity (real->double-flonum opacity)))))))

(define #:forall (TS BS BM) dia-track-activate*
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:start-name [home-name : (Option String) #false]
           #:block-desc [block-desc : (Option (Dia-Block-Describer BS BM)) #false]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:track-factory [track-factory : (Dia-Track-Factory TS)]
           #:block-factory [block-factory : (Dia-Block-Factory BS BM)]
           [master : Dia-Track-Datum]] : Dia:Activity
    (define self (if (geo:track? master) master (dia:track-self master)))
    (parameterize ([current-master-track self])
      (create-dia-track dia:activity id self
                        #:frame frame
                        (dia-track-realize self track-factory free-factory block-factory
                                           (dia-register-home-name (geo-trail-home-anchor (geo:track-trail self)) home-name block-desc)
                                           (geo:track-foot-infos self)
                                           (real->double-flonum scale)
                                           (and opacity (real->double-flonum opacity)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-activity-block
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:factory [block-factory : Act-Block-Factory (default-act-block-factory)]
           #:desc [desc : (Option Act-Block-Describer) #false]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (define ns : Nonnegative-Flonum (if (> scale 0) (real->double-flonum scale) 1.0))
    (parameterize ([current-master-track #false])
      (dia-block-realize block-factory desc
                         (cond [(symbol? caption) caption]
                               [(keyword? caption) caption]
                               [(string? caption) (string->symbol caption)]
                               [else (string->symbol (format "~a" caption))])
                         direction
                         (real->double-flonum scale)
                         (and opacity (real->double-flonum opacity))))))

(define #:forall (S M) dia-activity-block*
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:desc [desc : (Option (Dia-Block-Describer S M)) #false]
           #:factory [block-factory : (Dia-Block-Factory S M)]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (define ns : Nonnegative-Flonum (if (> scale 0) (real->double-flonum scale) 1.0))
    (parameterize ([current-master-track #false])
      (dia-block-realize block-factory desc
                         (cond [(symbol? caption) caption]
                               [(keyword? caption) caption]
                               [(string? caption) (string->symbol caption)]
                               [else (string->symbol (format "~a" caption))])
                         direction
                         (real->double-flonum scale)
                         (and opacity (real->double-flonum opacity))))))
