#lang typed/racket/base

(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-activity-diagram! goma.dia [#:opacity 0.32 #:frame 'Snow] #:-
  [#:tree (move-down 1 '--=)
   [=> (move-left 0.8 #false "should be dropped")
       (move-down 1.5 'sort "fork 1")
       (L-step +0.6+0.8i #false "sorted")
       (move-down 0.5)
       (move-right 0.2 '>-)]

   [=> (move-right 0.8)
       (move-down 1.5 'collate "fork 2")
       (L-step -0.6+0.8i #false "collated")
       (L-step -0.2+0.5i #false "should be dropped")]]

  (jump-to '>-)
  (move-down 1 'step1)
  (move-down 1 'step2)
  (move-down 1 '|<   >|)
  (move-left 0.5 'End$)

  (jump-to -1.5+0.5i '.inside)
  (T-step 3+3.5i " label inside")
  (T-step '.inside)
  
  (jump-to -1+7.25i '.outside)
  (T-step 2-3i)
  (T-step '.outside #false "label outside")

  (jump-to 'collate '#:/doc/whatever)
  [#:tree (jump-right 1.5 '#:/doc/pptx)
   [=> (move-left 0.25)
       (L-step 'step1 #false '#:horizon)]
   [=> (move-left 0.25)
       (L-step 'step2 #false '#:horizon)]
   [=> (move-down '|<   >|)
       (move-leftwards '|<   >| #false '#:U-turn)
       (turn-up-left-down)
       (turn-down-right-up)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  goma.dia)
