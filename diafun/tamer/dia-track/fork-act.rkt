#lang typed/racket/base

(require diafun/flowchart)
(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-activity-diagram! goma.dia [#:opacity 0.5] #:-
  [#:tree (move-down 1 '-=)
   [=> (move-left 1)
       (move-down 1.5 '<sort> "by priority")
       (move-down 1)
       (move-right '#:home '=-)]

   [=> (move-right 1)
       (move-down 1.5 'λcollate~ "by identity")
       (L-step '=-)]]

  (jump-to '=-)
  (move-down 1 'End$))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  goma.dia)
