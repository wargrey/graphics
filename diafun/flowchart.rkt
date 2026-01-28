#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/track/base.rkt"))
(provide (all-from-out "digitama/decoration/base.rkt"))
(provide (all-from-out "digitama/flowchart/self.rkt"))
(provide (all-from-out "digitama/flowchart/style.rkt"))

(require "digitama/track/dc.rkt")
(require "digitama/track/base.rkt")
(require "digitama/track/realize.rkt")
(require "digitama/track/interface.rkt")

(require "digitama/block/dc.rkt")
(require "digitama/block/realize.rkt")
(require "digitama/block/interface.rkt")

(require "digitama/decoration/base.rkt")

(require "digitama/flowchart/self.rkt")
(require "digitama/flowchart/style.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-flowchart! stx)
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
       (let* ([goma (dia-initial-track pid gw gh ts home anchor (default-flow-block-width))]
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
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:track-factory [track-factory : Flow-Track-Factory (default-flow-track-factory)]
           #:block-factory [block-factory : Flow-Block-Factory (default-flow-block-factory)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) (default-dia-note-factory)]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           #:start-name [home-name : (Option Geo-Rich-Text) #false]
           #:block-desc [block-desc : (Option Flow-Block-Describer) #false]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           [master : Dia-Track-Datum]] : Dia:FlowChart
    (define self (if (geo:track? master) master (dia:track-self master)))

    (create-dia-track dia:flowchart id self
                      #:frame frame
                      (dia-track-realize self track-factory free-factory block-factory block-desc
                                         note-factory note-desc
                                         scale opacity home-name))))

(define #:forall (TS BS BM) dia-track-flow*
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:start-name [home-name : (Option Geo-Rich-Text) #false]
           #:block-desc [block-desc : (Option (Dia-Block-Describer BS BM)) #false]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:track-factory [track-factory : (Dia-Track-Factory TS)]
           #:block-factory [block-factory : (Dia-Block-Factory BS BM)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) (default-dia-note-factory)]
           [master : Dia-Track-Datum]] : Dia:FlowChart
    (define self (if (geo:track? master) master (dia:track-self master)))

    (create-dia-track dia:flowchart id self
                      #:frame frame
                      (dia-track-realize self track-factory free-factory block-factory block-desc
                                         note-factory note-desc
                                         scale opacity home-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-flow-block
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:block-factory [block-factory : Flow-Block-Factory (default-flow-block-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) (default-dia-note-factory)]
           #:block-desc [block-desc : (Option Flow-Block-Describer) #false]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (dia-block-realize block-factory block-desc note-factory note-desc caption direction scale opacity)))

(define #:forall (S M) dia-flow-block*
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:block-desc [block-desc : (Option (Dia-Block-Describer S M)) #false]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           #:block-factory [block-factory : (Dia-Block-Factory S M)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) (default-dia-note-factory)]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (dia-block-realize block-factory block-desc note-factory note-desc caption direction scale opacity)))
