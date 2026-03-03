#lang typed/racket/base

(require diafun/flowchart)
(require diafun/activity)

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define scale : Nonnegative-Flonum 1.0)

(define-flowchart! goma.dia [#:block-scale scale #:frame 'LavenderBlush] #:-
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
   [=> (move-left 2 #false "[database]")
       (move-down 1 '#:/db/Store)
       (move-down 1)
       (move-right '#:home '+-)]

   [=> (move-left-down 0.75 0.35 #false (cons #false "[dir]"))
       (move-down 0.7 '#:/doc/folder/)
       (move-downward '+-)
       (turn-down-right)
       (move-to '+-)]
   
   [=> (move-right-down 0.75 0.35 #false (cons #false "[file]"))
       (move-down 0.7 '#:/doc/document.zip)
       (move-downward '+-)
       (turn-down-left)
       (move-to '+-)]

   [=> (move-right 2 #false "[in-memory]")
       (move-down 1 '#:/proc/Memory)
       (L-step '+-)]]

  (jump-to '+-)
  (move-down 0.75 '@A)
  (jump-to '--=)
  (jump-down 0.75 '@A.)
  (move-down 1 'End$)

  (note '@A 1 0 "block comment: on-page connector")
  (note 0+0.5i 1 pi "track comment: control flow")
  (note #:stereotype 'decisionInput
        '-+ 1.75 -pi/5
        "block comment: decision block"
        ""
        "流程图随意;"
        "活动图,此处用法有误！")

  (jump-left-down 1 'wait... '//#empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-ht-append #:gapsize 32.0
                 goma.dia
                 (dia-track-activate goma.dia #:frame 'Azure #:block-scale scale)))
