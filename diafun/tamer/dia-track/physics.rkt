#lang typed/racket/base

(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scale : Nonnegative-Flonum 1.0)

(define Qual. : Symbol '定性解释)
(define Quant : Symbol '定量计算)
(define Expt. : Symbol '实验探究)
(define Theor. : Symbol '理论分析)
(define phenomenon : Keyword (string->keyword "现象/经验/规律"))
(define model : Keyword (string->keyword "模型/定律/定理"))

(define track-style : Act-Track-Theme-Adjuster
  (lambda [style src tgt labels]
    (if (and (dia:block? src) (dia:block? tgt)
             (dia:block-diff-type? src tgt)
             (or (dia:block*-typeof? src act-object-style?)
                 (dia:block*-typeof? tgt act-object-style?)))
        (remake-dia-track-style style #:source-tip 'arrow)
        (void))))

(define zone-colorize : Dia-Zone-Theme-Adjuster
  (lambda [style id type metadata]
    (remake-dia-zone-style #:stroke-width 1.0
                           #:fill-paint (if (eq? id 'SPhy) 'Lavender 'AliceBlue)
                           style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-activity-diagram! phy.dia
  #:parameterize ([default-act-track-theme-adjuster track-style]
                  [default-dia-rubber-zone-theme-adjuster zone-colorize])
  [#:block-scale scale] #:-
  [#:zone 'SPhy #:desc "高中物理" #:options (list (make-dz:dock #:side 't #:position 0.0))

   [#:zone 'JPhy #:desc "初中物理" #:options (list (make-dz:dock #:side 't #:position 0.0))
    (stay Qual.)
    (move-down 1.0 phenomenon)
    (move-down 1.0 Expt.)
    (move-right 1.0 '-+)]
   
   [=> (focus Qual.)
       (move-right 2.5 Quant)
       (move-down 1.0 model)
       (move-down Expt.)]
   [=> (focus '-+)
       [=> (move-right 1.5 Theor. "[精确、系统]")]
       [=> (move-up-left Qual. 0.75 #false "[启蒙、体验]")
           (move-left Qual.)]]]

  (focus model)
  (move-right 1.5 '#:物理直觉 (cons '#:内化 #false))

  (jump-left-down '-+ 2.618 '#::测量仪器#device)
  (move-to '-+ (cons '#:测量 #false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  phy.dia)
