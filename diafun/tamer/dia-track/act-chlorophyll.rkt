#lang typed/racket/base

(provide (all-defined-out))

(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define captions
  #hasheq((Observe . "留心观察日常生活和自然现象")
          (Curiosity  . "提出问题\n树叶为什么是绿色的？又为什么会变黄？")
          (Hypothesis . "联系已做实验猜测\n树叶里存在跟外显颜色有关的色素")
          (Watch . "带着问题观看科普视频")
          (Expression . "分享对视频的理解\n表述光合色素与树叶颜色的关系")
          (Prepare . "准备实验验证\n明确实验目的和操作细则")
          (Step1 . "实验验证第一步\n[叶绿素提取实验]")
          (Step2 . "实验验证第二步\n[叶绿素层析实验]")
          (#::Filtrate . "叶绿素滤液")
          (#::Chromatogram . "层析滤纸")
          (Analyze . "分析讨论可能的原因\n菠菜是大变数、操作是否规范")
          (Experience . "理解并感受科学家的日常\n当实验结果不符合预期时")
          (Assembly . "科学背包归档\n(带回家与父母/同学交流、延伸实验)")
          (Sumup . "项目总结\n重新理解最初的好奇")
          (Colloid . "对比实验：叶绿素滤液 vs 绿色水溶液\n直观丁达尔效应、初识胶体")

          (Change . "苏教版六年级科学教学\n第一单元：《物质的变化》")
          (Extract . "收尾小节教学\n第三节：《化学家的研究》")
          
          (Prepare6 . "简单提及树叶的颜色\n明确实验目的和操作细则")
          (Grinding+Filter . "研磨、过滤\n提取叶绿素")
          (#::Filtrate6 . "叶绿素滤液")
          (#::Chromatogram6 . "层析滤纸")
          (Chromatography . "层析\n观察光合色素的分离过程")
          (Experience6 . "理解和感受化学家的研究")
          (Cleanup . "实验善后处理")
          (Light . "留给五年级做光学实验")
          
          (Spread . "小实验：纸吸水观察墨汁扩展\n模拟物质组成分析")
          (Rote . "在困惑中死记硬背\n化学家的工作包括提取和合成新物质")))

(define pin-stroke (desc-stroke #:color 'Teal #:width 2.0))
(define material-pin (geo-inset (geo-square 8 #:stroke pin-stroke) 2.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-activity-diagram! chlorophyll.dia
  [#:block-desc captions #:frame 'White] #:-
  (move-down 1 'Observe)

  [#:tree (move-down 1 '?)
   [=> (move-left 1 'cancel~$ "[无想法]")]
   [=> (move-down 1.5 'Curiosity "[好奇]")

       [#:tree (move-down 1 'freshmen?)
        [=> (move-left 1 #false "[研磨过花青素]")
            (move-down 1 'Hypothesis)
            (move-down)
            (move-right 1 '+watch-)]
        [=> (move-down '+watch- #false "[无基础]")]]

       (jump-to '+watch-)
       (move-down 1 'Watch)
       (move-down 1 'Expression)
       (move-down 1 'Prepare)
       
       [#:tree (move-down 1 'Step1)
        [=> (move-left 3.5 #false '#:write)
            (move-down 3 '#:/doc/实验报告)]
        [=> (move-down 1 '#::Filtrate)
            
            [#:tree (move-down 0.75 '--=)
             [=> (move-left)
                 [#:tree (move-down '#:/doc/实验报告 'Step2 material-pin)
                  [=> (move-to '#:/doc/实验报告 '#:write)]
                  [=> [#:tree (move-left-down 0.5 1 'obvious?)
                       [=> (move-down 3 '+sumup- "[色带明显{P < 0.1}]")
                           (move-down 1 'Sumup)
                           (move-down 1 'Assembly)]
                       [=> (move-left 1 #false "[色带不明显{P > 0.9}]")
                           (move-down 1 'Analyze)
                           (move-down 1 'Experience)
                           (L-step '+sumup-)]]]
                  [=> [#:tree (move-right-down 0.5 1 '#::Chromatogram)
                       [=> (move-to 'obvious? '#:read)]
                       [=> (move-downwards 'Assembly)
                           (turn-down-left)
                           (move-to 'Assembly)]]]]]
             [=> (L-step 'Assembly material-pin)]
             [=> (move-right)
                 (move-down 'Step2 'Colloid material-pin)
                 (move-down 2)
                 (move-right 1 '#:/试剂架)]]]]]]

  [#:tree (jump-to '#:/doc/实验报告)
   [=> (move-right 0.2)
       (L-step 'Sumup #false '#:read)]
   [=> (move-left 0.2)
       (L-step 'Assembly)
       (move-down 1 '$)]]

  (jump-to 5 '^6)
  (move-down 1 'Change)
  (move-down 1 'Extract)

  [#:tree (move-down 1 'std?)
   [=> (move-right 1 #false "[标准流程]")
       (move-down 1 'Spread)
       (move-down 1 'Rote)
       (move-down 1 '6~$)]
   [=> (move-left 1 #false "[有趣且有用]")
       (move-down 1 'Prepare6)
       (move-down 1 'Grinding+Filter)
       (move-down 1 '#::Filtrate6)

       [#:tree (move-down 1 '-<)
        [=> (move-right 0.2)
            (L-step 0.5+0.5i #false material-pin)
            [#:tree (move-down 1 'Chromatography material-pin)
             [=> (move-left-down 0.25 1 'Experience6)
                 (move-down 1 'Cleanup)
                 (move-down 1 '6$)]
             [=> (move-right-down 1 1 '#::Chromatogram6)
                 (L-step 'Cleanup)]]]
        [=> (move-left 0.2)
            (L-step -0.8+0.5i #false material-pin)
            [#:tree (move-down 'Cleanup 'keep? material-pin)
             [=> (move-to 'Cleanup (cons "[不再需要]" material-pin))]
             [=> (L-step '#:/试剂架 "[留给五年级做光学实验]")]]]]]]
  
  (jump-to 2)
  [#:tree (jump-down 'Prepare6 '#:/doc/课件)
   [=> (move-left 0.25)
       (L-step 'Watch #false '#:read)]
   [=> (move-left 0.25)
       (L-step 'Prepare #false '#:read)]
   [=> (move-to 'Prepare6 '#:read)]]
  
  (jump-up '#:home)
  (move-down '$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require geofun/vector)

  ; 75ms
  (time (geo-freeze chlorophyll.dia)))
