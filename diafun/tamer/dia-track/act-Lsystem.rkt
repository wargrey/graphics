#lang typed/racket/base

(require diafun/activity)

(define-activity-diagram! tr.act
  #:parameterize ([default-act-block-width 80.0]
                  [default-act-block-height 45.0])
  #:let ([apex-pin (geo-inset (geo-square 8 #:stroke (desc-stroke #:color 'ForestGreen #:width 2.0)) 4.0)])
  #:grid-width (&: 2)
  [#:frame 'White] #:-

  [#:zone 'TR-rule region #:desc "L-system: TR\nparameter: Order" #:options (list (make-dz:dock #:side 't #:position 1.0))
   (move-down 1.5 '#:Apex)
   (move-down 1.0 '-+)
   [=> (move-right 0.85 'F (list "[No]" apex-pin))]
   [=> (move-down 1.50 'FF#a (list #false "[Yes]" #false #false apex-pin))
       (move-down 0.75 '-==== apex-pin)
       [=> (step-left-down 0.8 0.6 '#:LBud)
           (move-down 0.8 '\\+)
           (move-down 1.0 '@R#L apex-pin)]
       [=> (move-down 0.6 '#:RBud)
           (move-down 0.8 '\\-)
           (move-down 1.0 '@R#R apex-pin)]
       [=> (step-right-down 0.8 1.4 'FF#b #false apex-pin)
           (move-down 1.0 '@R apex-pin)]]

   (note #:stereotype 'decisionInput
         '-+ -0.8 pi/6
         "Order > 0")
  
   (note #:stereotype 'update
         0+3.5i 0.618 pi "Order --")]

  (jump-to 'F)
  (move-right 0.75 '$)
  
  (jump-to '#:Apex)
  (jump-left 1.5 '@R.)
  (move-right '#:Apex #false "all apices")

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  tr.act)
