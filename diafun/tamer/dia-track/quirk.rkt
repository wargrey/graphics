#lang typed/racket/base

(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-activity-diagram! goma.dia [#:opacity 0.32 #:frame 'Snow] #:-
  [#:tree (move-down 1 '--=)
   [=> (move-left 1 #false "should be dropped")
       (move-down 1.5 'sort "fork 1")
       (move-down 1.5 #false "sorted")
       (move-right '#:home '=--)]

   [=> (move-right 1)
       (move-down 1.5 'collate "fork 2")
       (L-step '=-- "collated" "should be dropped")]]

  (jump-to '=--)
  (move-down 1 'step1)
  (move-down 1 'step2)
  (move-down 1 '|<   >|)
  (move-left 0.5 'End$)

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
