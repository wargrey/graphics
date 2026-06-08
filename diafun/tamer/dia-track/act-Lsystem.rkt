#lang typed/racket/base

(require diafun/activity)

(define-activity-diagram! tr.act
  #:parameterize ([default-act-block-width 80.0]
                  [default-act-block-height 45.0])
  #:let ([apex-pin (geo-inset (geo-square 8 #:stroke (desc-stroke #:color 'ForestGreen #:width 2.0)) 4.0)])
  #:grid-width (&: 2)
  [#:frame 'White] #:-
  (move-down 1.0 '#:Apex)
  [=> (jump-left 1.5 '@R.)
      (move-right '#:Apex #false "all apices")]
  [=> (move-down 1.0 '-+)
      [=> (move-right 0.85 'F (list "[No]" apex-pin))
          (move-right 0.75 '$)]
      [=> (move-down 1.50 'FF#a (list #false "[Yes]" #false #false apex-pin))
          (move-down 0.75 '-==== apex-pin)
          [=> (step-left-down 0.8 0.6 '#:Shoot1)
              (move-down 0.8 '\\+)
              (move-down 1.0 '@R#L apex-pin)]
          [=> (move-down 0.6 '#:Shoot2)
              (move-down 0.8 '\\-)
              (move-down 1.0 '@R#R apex-pin)]
          [=> (step-right-down 0.8 1.4 'FF#b #false apex-pin)
              (move-down 1.0 '@R apex-pin)]]]

  (jump-to -1.25+0.5i '#:.lt)
  (move-right 2.0)
  (T-step 0.5+6.5i "L-system")
  (T-step '#:.lt)

  (note #:stereotype 'decisionInput
        '-+ -0.8 pi/6
        "Order > 0")
  
  (note 0+2.5i 0.618 pi "Order --"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  tr.act)
