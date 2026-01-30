#lang typed/racket/base

(require diafun/flowchart)
(require diafun/activity)

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scale : Nonnegative-Flonum 1.0)

(define-flowchart! goma.dia [#:block-scale scale] #:-
  [#:tree (move-down 1 '-=)
   [=> (move-left 1)
       (move-down 2 '<sort> "by priority")
       (move-down 1)
       (move-right '#:home '=-)]

   [=> (move-right 1)
       (move-down 2 'λcollate~ "by identity")
       (L-step '=-)]]

  (jump-to '=-)
  (move-down 1 'wait...)
  (move-down 1 '|:manually operate|)
  
  [#:tree (move-down 1 '-+)
   [=> (move-left 1 #false "[guard 1]")
       (move-down 1 '#:/db/store)
       (move-down 1)
       (move-right '#:home '+-)]

   [=> (move-right 1 #false "[guard 2]")
       (move-down 1 '#:/proc/memory)
       (L-step '+-)]]

  (jump-to '+-)
  (move-down 1 '@A)
  (jump-to '-=)
  (jump-down 0.75 '@A.)
  (move-down 1 'End$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-ht-append #:gapsize 32.0
   goma.dia
   (dia-track-activate goma.dia #:block-scale scale)))
