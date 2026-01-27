#lang typed/racket/base

(require diafun/flowchart)
(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scale : Nonnegative-Flonum 1.0)

(define-flowchart! goma [#:block-scale scale] #:-
  [#:tree (move-down 1 '-=)
   [=> (move-left 1)
       (move-down 1.5 '<sort> "by priority")
       (move-down 1)
       (move-right '#:home '=-)]

   [=> (move-right 1)
       (move-down 1.5 'λcollate~ "by identity")
       (L-step '=-)]]

  (jump-to '=-)
  (move-down 1 'wait...)
  
  [#:tree (move-down 1 '-+)
   [=> (move-left 1 #false "[guard 1]")
       (move-down 1 '/db/store)
       (move-down 1)
       (move-right '#:home '+-)]

   [=> (move-right 1 #false "[guard 2]")
       (move-down 1 '|λremote call|)
       (L-step '+-)]]

  (jump-to '+-)
  (move-down 1 'End$))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-ht-append #:gapsize 32.0
   goma
   (dia-track-activate goma #:block-scale scale)))
