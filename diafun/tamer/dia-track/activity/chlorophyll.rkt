#lang typed/racket/base

(provide (all-defined-out))

(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-activity-diagram! chlorophyll.dia
  [#:block-desc #hasheq((Observe . "留心观察日常生活和自然现象")
                        (Curiosity  . "提出问题\n树叶为什么是绿色的？又为什么会变黄？")
                        (Hypothesis . "联系已做实验猜测\n树叶里存在跟颜色有关的色素")
                        (Watch . "带着问题观看科普视频")
                        (Expression . "分享对视频的理解\n表述光合色素与树叶颜色的关系")
                        (Experiment01 . "实验环节第一步\n[叶绿素提取实验]")
                        (Experiment02 . "实验环节第二步\n[叶绿素层析实验]")
                        (#:Filtrate . "叶绿素滤液")
                        (#:Chromatogram . "层析滤纸")
                        (Experience . "理解并感受科学家的日常\n当实验结果不符合预期时")
                        (Sample . "采集滤液样品\n(带回家留作纪念、延伸实验)")
                        (Report . "项目总结"))] #:-
  (move-down 1 'Observe)

  [#:tree (move-down 1 '?)
   [=> (move-left 1 'cancel~$ "无想法")]
   [=> (move-down 1 'Curiosity)
       (move-down 1 'Hypothesis)
       (move-down 1 'Watch)
       (move-down 1 'Expression)
       
       (move-down 1 'Experiment01)
       (move-down 1 '#:Filtrate)

       [#:tree (move-down 0.75 '-=)
        [=> (move-left)
            (move-down 0.75 'Experiment02)
            (move-down 1 '#:Chromatogram)

            [=> [#:tree (move-down 1 'okay?)
                 [=> (move-down 2 '+report- "[层析效果明显]")
                     (move-down 1 'Report)
                     (move-down 1 'Sample)]
                 [=> (move-left 1 #false  "[层析效果不明显]")
                     (move-down 1 'Experience)
                     (L-step '+report-)]]]]
        [=> (L-step '+report-)]]]]

  (jump-to 'Sample)
  (move-down 1 'pbl$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  chlorophyll.dia)
