#lang typed/racket/base

(provide (all-defined-out))

(require diafun/usecase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Skeleton : Symbol 'Skeleton#像工程师一样实践)
(define title : String "剑龙骨架项目")

(define pbl-colorize : UC-Block-Theme-Adjuster
  (lambda [style id stereotype]
    (if (keyword? id)
        (case id
          [(#:Teacher) (remake-dia-block-style style #:fill-paint 'DeepSkyBlue)]
          [(#:Assistant) (remake-dia-block-style style #:fill-paint 'SkyBlue)]
          [(#:StuHead #:StuVertebra #:StuOrgans #:StuLimbs) (remake-dia-block-style style #:height 50 #:fill-paint 'Gold)])
        (case stereotype
          [(#:edu) (remake-dia-block-style style #:fill-paint 'LightSkyBlue #:stroke-color 'transparent)]
          [(#:grp) (remake-dia-block-style style #:fill-paint 'LemonChiffon #:stroke-color 'transparent)]
          [(#:stu) (remake-dia-block-style style #:fill-paint 'LightGreen #:stroke-color 'transparent)]))))

(define sys-colorize : UML-Zone-Theme-Adjuster
  (lambda [style id type property]
    (remake-dia-zone-style style #:fill-paint 'Lavender #:stroke-color 'transparent)))

(define pbl-desc
  #hasheq((#:Teacher . "主教老师")
          (#:Assistant . "助教老师")
          (#:Student . "全体学生")
          (#:StuHead . "头部模型组")
          (#:StuVertebra . "脊椎模型组")
          (#:StuOrgans . "内脏模型组")
          (#:StuLimbs . "四肢模型组")
          (set#edu . "启动学期项目")
          (survey . "学情摸底")
          (search . "搜索科普文")
          (module#edu . "设计 PBL 教学流程")
          (assign#edu . "布置小论文作业")
          (assist . "示范、协助学生")
          (do-homework#stu . "完成小论文\n恐龙何以如此大")
          (study#stu . "听讲\n梳理理论知识")
          (experiment#edu . "准备项目物料")
          (experiment#stu . "实验验证\n平方-立方定律")
          (create#stu  . "完成剑龙骨架模型")
          (optimize#stu . "评估与优化模型")
          (report#stu . "项目总结与报告")
          
          (divide#grp . "小组分工")
          (head#grp . "制作头骨和细牙")
          (vertebra#grp . "制作脊椎和背骨")
          (organs#grp . "制作气囊呼吸系统\n和消化系统")
          (limbs#grp . "制作四肢和脚趾")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-use-case-diagram! skeleton.dia #:start '#:Teacher
  #:parameterize ([default-uc-block-theme-adjuster pbl-colorize]
                  [default-dia-rubber-zone-theme-adjuster sys-colorize])
  [#:frame 'White #:block-desc pbl-desc] #:-
  [#:zone Skeleton 'system #:desc title
   (actor-use 2.5 -pi/3 'set#edu)
   (actor-use 2.5 +pi/3 'assist)
   
   (jump-to 'set#edu)
   (case-include 2.25 -pi/12 'survey)
   (case-include 2.25 +pi/12 'assign#edu)
   (case-include 2 +pi/4 'module#edu)
   (case-include 2 +pi/2 'experiment#edu)]
   
  (jump-left-down '#:Teacher 'assist '#:Assistant)
  [#:with-zone Skeleton
   (actor-use 'assist)]

  (jump-to 6.5+0.5i '#:Student)
  [#:with-zone Skeleton
   (actor-use 2.0 -5pi/6 'do-homework#stu)
   (actor-use 2.0 pi 'study#stu)
   (actor-use 2.0 +5pi/6 'create#stu)

   (jump-to 'do-homework#stu)
   (case-extend 2.0 -pi/2 'search)
   
   (jump-to 'study#stu)
   (case-extend -2.0 0 'experiment#stu)

   (jump-to 'create#stu)
   (case-extend 2.0 pi 'optimize#stu)
   (case-include 2.0 4pi/5 'report#stu)]

  (jump-to 6+3.5i '#:StuHead)
  [=> [#:with-zone Skeleton
       (actor-use 1.5 +pi 'head#grp)
       (jump-to 'head#grp)
       (case-include 3.0 +3pi/4 'divide#grp)]]
  [=> (move-rightward '#:Student)
      (turn-right-up)
      (actor-generalize '#:Student)]
  
  (jump-to 6+5.0i '#:StuVertebra)
  [=> [#:with-zone Skeleton
       (actor-use 1.5 +pi 'vertebra#grp)
       (include 'vertebra#grp 'divide#grp)]]
  [=> (move-rightward '#:Student)
      (turn-right-up)
      (actor-generalize '#:Student)]

  (jump-to 6+6.5i '#:StuOrgans)
  [=> [#:with-zone Skeleton
       (actor-use 1.5 +pi 'organs#grp)
       (include 'organs#grp 'divide#grp)]]
  [=> (move-rightward '#:Student)
      (turn-right-up)
      (actor-generalize '#:Student)]
  
  (jump-to 6+8.0i '#:StuLimbs)
  [=> [#:with-zone Skeleton
       (actor-use 1.5 +pi 'limbs#grp)
       (include 'limbs#grp 'divide#grp)]]
  [=> (move-rightward '#:Student)
      (turn-right-up)
      (actor-generalize '#:Student)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  skeleton.dia)
