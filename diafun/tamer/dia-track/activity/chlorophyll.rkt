#lang typed/racket/base

(provide (all-defined-out))

(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define captions
  #hasheq((Observe . "留心观察日常生活和自然现象")
          (Curiosity  . "提出问题\n树叶为什么是绿色的？又为什么会变黄？")
          (Hypothesis . "联系已做实验猜测\n树叶里存在跟颜色有关的色素")
          (Watch . "带着问题观看科普视频")
          (Expression . "分享对视频的理解\n表述光合色素与树叶颜色的关系")
          (Step1 . "实验验证第一步\n[叶绿素提取实验]")
          (Step2 . "实验验证第二步\n[叶绿素层析实验]")
          (#:Filtrate#material . "叶绿素滤液")
          (#:Chromatogram#material . "层析滤纸")
          (Analyze . "分析讨论可能的原因\n菠菜是大变数、操作是否规范")
          (Experience . "理解并感受科学家的日常\n当实验结果不符合预期时")
          (Assembly . "科学背包归档\n(带回家留作纪念、延伸实验)")
          (Report . "项目总结\n重新理解生活现象")
          (Light . "有趣的光学实验\n(了解胶体和丁达尔效应)")))

(define pin-stroke (desc-stroke #:color 'ForestGreen #:width 2.0))
(define material-pin (geo-inset (geo-square 8 #:stroke pin-stroke) 2.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-activity-diagram! chlorophyll.dia
  [#:block-desc captions #:frame 'White] #:-
  (move-down 1 'Observe)

  [#:tree (move-down 1 '?)
   [=> (move-left 1 'cancel~$ '无想法)]
   [=> (move-down 1.5 'Curiosity "[好奇]")

       [#:tree (move-down 1 'freshmen?)
        [=> (move-left 1 #false '研磨过花青素)
            (move-down 1 'Hypothesis)
            (move-down)
            (move-right 1 '+watch-)]
        [=> (move-down '+watch- #false '无基础)]]

       (jump-to '+watch-)
       (move-down 1 'Watch)
       (move-down 1 'Expression)
       
       [#:tree (move-down 1 'Step1)
        [=> (move-left 3 #false '#:write)
            (move-down 3 '#:/doc/实验报告)]
        [=> (move-down 1 '#:Filtrate#material)
            
            [#:tree (move-down 0.75 '-=)
             [=> (move-left 0.5)
                 [#:tree (move-down '#:/doc/实验报告 'Step2 material-pin)
                  [=> (move-to '#:/doc/实验报告 '#:write)]
                  [=> [#:tree (move-left-down 0.5 1 'obvious?)
                       [=> (move-down 3 '+report- '|色带明显{P < 0.9}|)
                           (move-down 1 'Report)
                           (move-down 1 'Assembly)]
                       [=> (move-left 1 #false '|色带不明显{P > 0.1}|)
                           (move-down 1 'Analyze)
                           (move-down 1 'Experience)
                           (L-step '+report-)]]]
                  [=> [#:tree (move-right-down 0.5 1 '#:Chromatogram#material)
                       [=> (move-to 'obvious? '#:read)]
                       [=> (move-downwards 'Assembly)
                           (turn-down-left)
                           (move-to 'Assembly)]]]]]
             [=> (move-right 0.5)
                 (L-step 'Assembly material-pin)]
             [=> (move-right)
                 (move-down)
                 (move-right 1 'Light material-pin)]]]]]]

  (jump-to '#:/doc/实验报告)
  (L-step 'Assembly)
  (move-down 1 '$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  chlorophyll.dia)
