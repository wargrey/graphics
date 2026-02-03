#lang typed/racket/base

(require diafun/flowchart)
(require diafun/activity)

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scale : Nonnegative-Flonum 1.0)

(define note-desc
  #hasheq([//#decisionInput . "block comment: decision block\n\n流程图随意;\n活动图,此处用法有误！"]))

(define-flowchart! goma.dia [#:block-scale scale #:frame 'Snow #:note-desc note-desc] #:-
  [#:tree (move-down 1 '--=)
   [=> (move-left 1)
       (move-down 2 '<sort> "by priority")
       (move-down 1)
       (move-right '#:home '=--)]

   [=> (move-right 1)
       (move-down 2 'λcollate~ "by identity")
       (L-step '=--)]]

  (jump-to '=--)
  (move-down 1 'wait...)
  
  [#:tree (move-down 1 '-+)
   [=> (move-left 1 #false "[guard 1]")
       (move-down 1 '#:/db/Store)
       (move-down 1)
       (move-right '#:home '+-)]

   [=> (move-right 1 #false "[guard 2]")
       (move-down 1 '#:/proc/Memory)
       (L-step '+-)]]

  (jump-to '+-)
  (move-down 0.75 '@A)
  (jump-to '--=)
  (jump-down 0.75 '@A.)
  (move-down 1 'End$)

  (jump-to '@A)
  (radial-back 1 0 '//|block comment: on-page connector|)

  (jump-to '#:home)
  (jump-down 0.5)
  (radial-back 1 pi '//|track comment: control flow|)

  (jump-to '-+)
  (radial-back 1.75 -pi/5 '//#decisionInput)

  (jump-left-down '+- '#:/db/Store '//#error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-ht-append #:gapsize 32.0
                 goma.dia
                 (dia-track-activate goma.dia #:frame 'Azure #:note-desc note-desc #:block-scale scale)))
