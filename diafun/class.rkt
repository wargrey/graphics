#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/track/base.rkt"))
(provide (all-from-out "digitama/track/freestyle.rkt"))
(provide (all-from-out "digitama/class/self.rkt"))
(provide (all-from-out "digitama/class/style.rkt"))
(provide (all-from-out "digitama/class/relationship.rkt"))

(require geofun/digitama/dc/track)

(require "digitama/track/dc.rkt")
(require "digitama/track/base.rkt")
(require "digitama/track/realize.rkt")
(require "digitama/track/freestyle.rkt")
(require "digitama/track/interface.rkt")

(require "digitama/block/dc.rkt")
(require "digitama/block/realize.rkt")
(require "digitama/block/interface.rkt")

(require "digitama/class/self.rkt")
(require "digitama/class/style.rkt")
(require "digitama/class/identifier.rkt")
(require "digitama/class/relationship.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-class-diagram! stx)
  (syntax-parse stx #:literals []
    [(_ (~alt (~optional (~seq #:grid-width   gw) #:defaults ([gw #'(&% 80)]))
              (~optional (~seq #:grid-height  gh) #:defaults ([gh #'(&% 50)]))
              (~optional (~seq #:turn-radius% ts) #:defaults ([ts #'+0.05]))
              (~optional (~seq #:path-id pid) #:defaults ([pid #'#false]))
              (~optional (~seq #:start anchor) #:defaults ([anchor #''#:home]))
              (~optional (~seq #:at home) #:defaults ([home #'0.0+0.0i]))
              (~optional (~seq #:parameterize pexpr) #:defaults ([pexpr #'()])))
        ...
        [args ...] #:- move-expr ...)
     (syntax/loc stx
       (let* ([goma (dia-initial-track pid gw gh ts home anchor (default-cls-block-width))]
              [dia (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-track-class dia args ...))))]))

(define-syntax (define-class-diagram! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-class-diagram! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-class
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:block-factory [block-factory : Cls-Block-Factory (default-cls-block-factory)]
           #:track-factory [track-factory : Cls-Track-Factory (default-cls-track-factory)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:relationship [class-type : (Option Cls-RelationShip-Identifier) (default-cls-relationship-identifier)]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           [master : Dia-Track-Datum]] : Dia:Class
    (define self (if (geo:track? master) master (dia:track-self master)))
    (parameterize ([default-cls-relationship-identifier class-type]
                   [current-master-track self])
      (create-dia-track dia:class id self
                        #:frame frame
                        (dia-track-realize self track-factory free-factory block-factory #false
                                           (geo:track-foot-infos self)
                                           (real->double-flonum scale)
                                           (and opacity (real->double-flonum opacity)))))))

(define #:forall (TS BS BM) dia-track-class*
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:block-factory [block-factory : (Dia-Block-Factory BS BM)]
           #:track-factory [track-factory : (Dia-Track-Factory TS)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:relationship [class-type : (Option Cls-RelationShip-Identifier) (default-cls-relationship-identifier)]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           [master : Dia-Track-Datum]] : Dia:Class
    (define self (if (geo:track? master) master (dia:track-self master)))
    (parameterize ([default-cls-relationship-identifier class-type]
                   [current-master-track self])
      (create-dia-track dia:class id self
                        #:frame frame
                        (dia-track-realize self track-factory free-factory block-factory #false
                                           (geo:track-foot-infos self)
                                           (real->double-flonum scale)
                                           (and opacity (real->double-flonum opacity)))))))           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-class-block
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:factory [block-factory : Cls-Block-Factory (default-cls-block-factory)]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (parameterize ([current-master-track #false])
      (dia-block-realize block-factory #false
                         (cond [(symbol? caption) caption]
                               [(keyword? caption) caption]
                               [(string? caption) (string->symbol caption)]
                               [else (string->symbol (format "~a" caption))])
                         direction
                         (real->double-flonum scale)
                         (and opacity (real->double-flonum opacity))))))

(define #:forall (S M) dia-class-block*
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:factory [block-factory : (Dia-Block-Factory S M)]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (parameterize ([current-master-track #false])
      (dia-block-realize block-factory #false
                         (cond [(symbol? caption) caption]
                               [(keyword? caption) caption]
                               [(string? caption) (string->symbol caption)]
                               [else (string->symbol (format "~a" caption))])
                         direction
                         (real->double-flonum scale)
                         (and opacity (real->double-flonum opacity))))))
