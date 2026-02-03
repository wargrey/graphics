#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/track/base.rkt"))
(provide (all-from-out "digitama/decoration/base.rkt"))
(provide (all-from-out "digitama/activity/self.rkt"))
(provide (all-from-out "digitama/activity/style.rkt"))
(provide (all-from-out "digitama/activity/parameter.rkt"))
(provide (all-from-out geofun/constructor))
(provide (rename-out [dia-track-activate dia-track-act]
                     [dia-track-activate* dia-track-act*]))

(require "digitama/track/dc.rkt")
(require "digitama/track/base.rkt")
(require "digitama/track/realize.rkt")
(require "digitama/track/interface.rkt")

(require "digitama/block/dc.rkt")
(require "digitama/block/realize.rkt")
(require "digitama/block/interface.rkt")

(require "digitama/decoration/base.rkt")
(require "digitama/decoration/note/uml.rkt")

(require "digitama/activity/self.rkt")
(require "digitama/activity/style.rkt")
(require "digitama/activity/parameter.rkt")

(require geofun/constructor)

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
           #:track-factory [track-factory : Act-Track-Factory (default-act-track-factory)]
           #:block-factory [block-factory : Act-Block-Factory (default-act-block-factory)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) uml-note-factory]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           #:block-desc [block-desc : (Option Act-Block-Describer) #false]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           [master : Dia-Track-Datum]] : Dia:Activity
    (define self (if (geo:track? master) master (dia:track-self master)))

    (create-dia-track dia:activity id self
                      #:frame frame
                      (dia-track-realize self track-factory free-factory block-factory block-desc
                                         note-factory note-desc scale opacity #false))))

(define #:forall (TS BS BM) dia-track-activate*
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:block-desc [block-desc : (Option (Dia-Block-Describer BS BM)) #false]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:track-factory [track-factory : (Dia-Track-Factory TS)]
           #:block-factory [block-factory : (Dia-Block-Factory BS BM)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) uml-note-factory]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           [master : Dia-Track-Datum]] : Dia:Activity
    (define self (if (geo:track? master) master (dia:track-self master)))

    (create-dia-track dia:activity id self
                      #:frame frame
                      (dia-track-realize self track-factory free-factory block-factory block-desc
                                         note-factory note-desc scale opacity #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-activity-block
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:block-factory [block-factory : Act-Block-Factory (default-act-block-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) uml-note-factory]
           #:block-desc [block-desc : (Option Act-Block-Describer) #false]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (dia-block-realize block-factory block-desc note-factory note-desc caption direction scale opacity)))

(define #:forall (S M) dia-activity-block*
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:block-factory [block-factory : (Dia-Block-Factory S M)]
           #:block-desc [block-desc : (Option (Dia-Block-Describer S M)) #false]
           #:note-factory [note-factory : (Option Dia-Note-Factory) uml-note-factory]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (dia-block-realize block-factory block-desc note-factory note-desc caption direction scale opacity)))
