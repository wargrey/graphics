#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/track/base.rkt"))
(provide (all-from-out "digitama/decoration/base.rkt"))
(provide (all-from-out "digitama/usecase/self.rkt"))
(provide (all-from-out "digitama/usecase/style.rkt"))

(require "digitama/track/dc.rkt")
(require "digitama/track/base.rkt")
(require "digitama/track/realize.rkt")
(require "digitama/track/interface.rkt")

(require "digitama/block/dc.rkt")
(require "digitama/block/realize.rkt")
(require "digitama/block/interface.rkt")

(require "digitama/decoration/base.rkt")

(require "digitama/usecase/self.rkt")
(require "digitama/usecase/style.rkt")

(require (for-syntax racket/base))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (make-use-case-diagram! stx)
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
       (let* ([goma (dia-initial-track pid gw gh ts home anchor (default-uc-block-width))]
              [dia (with-gomamon! goma move-expr ...)])
         (parameterize pexpr
           (dia-track-use-case dia args ...))))]))

(define-syntax (define-use-case-diagram! stx)
  (syntax-parse stx #:literals []
    [(_ name argv ...)
     (syntax/loc stx
       (define name (make-use-case-diagram! argv ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-track-use-case
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:block-factory [block-factory : UC-Block-Factory (default-uc-block-factory)]
           #:track-factory [track-factory : UC-Track-Factory (default-uc-track-factory)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) uml-note-factory]
           #:start-name [home-name : (Option Geo-Rich-Text) #false]
           #:block-desc [block-desc : (Option UC-Block-Describer) #false]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           [master : Dia-Track-Datum]] : Dia:Use-Case
    (define self (if (geo:track? master) master (dia:track-self master)))
    (parameterize ([current-master-track self])
      (create-dia-track dia:use-case id self
                        #:frame frame
                        (dia-track-realize self track-factory free-factory block-factory block-desc
                                           note-factory note-desc scale opacity home-name)))))

(define #:forall (TS BS BM) dia-track-use-case*
  (lambda [#:id [id : (Option Symbol) #false]
           #:frame [frame : Geo-Frame-Datum #false]
           #:block-factory [block-factory : (Dia-Block-Factory BS BM)]
           #:track-factory [track-factory : (Dia-Track-Factory TS)]
           #:free-track-factory [free-factory : (Option Dia-Free-Track-Factory) (default-dia-free-track-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) uml-note-factory]
           #:start-name [home-name : (Option Geo-Rich-Text) #false]
           #:block-desc [block-desc : (Option (Dia-Block-Describer BS BM)) #false]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           #:block-scale [scale : Nonnegative-Real 1.0]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           [master : Dia-Track-Datum]] : Dia:Use-Case
    (define self (if (geo:track? master) master (dia:track-self master)))
    (parameterize ([current-master-track self])
      (create-dia-track dia:use-case id self
                        #:frame frame
                        (dia-track-realize self track-factory free-factory block-factory block-desc
                                           note-factory note-desc scale opacity home-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-use-case-block
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:block-factory [block-factory : UC-Block-Factory (default-uc-block-factory)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) uml-note-factory]
           #:block-desc [block-desc : (Option UC-Block-Describer) #false]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (parameterize ([current-master-track #false])
      (dia-block-realize block-factory block-desc note-factory note-desc
                         caption direction scale opacity))))

(define #:forall (S M) dia-use-case-block*
  (lambda [#:id [id : (Option Symbol) #false]
           #:scale [scale : Nonnegative-Real 0.5]
           #:opacity [opacity : (Option Nonnegative-Real) #false]
           #:block-factory [block-factory : (Dia-Block-Factory S M)]
           #:note-factory [note-factory : (Option Dia-Note-Factory) uml-note-factory]
           #:block-desc [block-desc : (Option (Dia-Block-Describer S M)) #false]
           #:note-desc [note-desc : (Option Dia-Note-Describer) #false]
           [caption : Any] [direction : (Option Float) #false]] : (Option Dia:Block)
    (parameterize ([current-master-track #false])
      (dia-block-realize block-factory block-desc note-factory note-desc
                         caption direction scale opacity))))
