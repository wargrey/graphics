#lang typed/racket/base

(provide (all-defined-out))

(require diafun/usecase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define inc : String "include")
(define ext : String "extend")
(define title : String "JrPLT and PBL Practice")

(define pbl-colorize : UC-Block-Theme-Adjuster
  (lambda [self id hint]
    (case id
      [(#:Researcher #:Teacher) (remake-dia-block-style self #:fill-paint 'Yellow)]
      [(#:Engineer) (remake-dia-block-style self #:fill-paint 'DeepSkyBlue)]
      [(arch dev api bdd) (remake-dia-block-style self #:fill-paint 'DeepSkyBlue #:stroke-color 'transparent)]
      [(fit dup example slide) (remake-dia-block-style self #:fill-paint 'LightGreen #:stroke-color 'transparent)]
      [(ct study trade-off) (remake-dia-block-style self #:fill-paint 'LemonChiffon #:stroke-color 'transparent)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-use-case-diagram! role.dia #:start '#:Engineer
  #:parameterize ([default-uc-block-theme-adjuster pbl-colorize])
  [#:frame 'White
   #:start-name "Software\nEngineer"
   #:block-desc #hasheq((#:Researcher . "Curriculum Designer")
                        (#:Teacher . "Instructor")
                        (#:Student . "Student")
                        (arch . "设计教学引擎")
                        (dev . "实现教学引擎")
                        (asset . "预制素材资源")
                        (api . "规范命名 API")
                        (train . "培训系统用法")
                        (doc . "编写用户文档")
                        (example . "编写范例项目")
                        (bdd . "行为驱动开发")
                        (deploy . "部署系统\n同步课程源码")
                        (study . "研发课程")
                        (slide . "编写演示程序")
                        (fit . "裁剪课程项目")
                        (dup . "完成课程项目")
                        (experiment . "设计实验")
                        (ct . "分解、识别\n抽象、建模")
                        (trade-off . "权衡新旧知识点")
                        (report . "项目总结与报告"))] #:-
  (radial-move 2 -45 'arch)
  (radial-move 2 0 'dev)
  (radial-move 2 +45 'train)
  
  (jump-to 'dev)
  (radial-move 2.5 -25 'bdd inc)
  (radial-back 2.5 +0 'asset ext)

  (jump-to 'arch)
  (radial-move 2.5 -15 'api inc)
  (move-to 'bdd #false inc)

  (jump-to 'train)
  (radial-move 2 -15 'doc inc)
  (radial-move 2 +15 'example inc)
  (radial-back 2.0 +75 'study ext)
  
  (jump-to -0.5+8i '#:Teacher)
  (radial-move 2 -30 'fit)
  (radial-move 3 0 'deploy)
  (move-to 3+9i 'report)
  
  [#:tree (jump-to -0.5+4i '#:Researcher)
   [=> (radial-move 2.0 25 'slide)
       (move-to 'study)
       (move-to 'example #false inc)]
   [=> (move-to '#:Teacher)]]

  (jump-to 'study)
  (radial-move 2.5 -10 'trade-off inc)
  (radial-back 2.0 +25 'experiment ext)
  (move-to 3.0+5.5i 'ct inc)

  [#:tree (jump-to 6+8i '#:Student)
   [=> (radial-move 2 -150.0 'dup)
       (move-to 'report)]
   [=> (move-to 'deploy)]]
  
  (jump-to 'ct)
  (move-to 'fit #false ext)
  (jump-to 'ct)
  (move-to 'dup #false ext)
  
  (jump-to 0.5-3.5i '.sys)
  (move-right 5 #false title)
  (move-down 13.0)
  (move-left '.sys)
  (move-up '.sys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  role.dia)
