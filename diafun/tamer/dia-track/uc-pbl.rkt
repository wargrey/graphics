#lang typed/racket/base

(provide (all-defined-out))

(require diafun/usecase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define title : String "JrPLT and PBL Practice")

(define pbl-colorize : UC-Block-Theme-Adjuster
  (lambda [style id stereotype]
    (if (keyword? id)
        (case id
          [(#:Researcher #:Teacher) (remake-dia-block-style style #:fill-paint 'Yellow)]
          [(#:Engineer) (remake-dia-block-style style #:fill-paint 'DeepSkyBlue)]
          [(#:Parent) (remake-dia-block-style style #:fill-paint 'MediumOrchid)])
        (case stereotype
          [(#:dev) (remake-dia-block-style style #:fill-paint 'DeepSkyBlue #:stroke-color 'transparent)]
          [(#:edu) (remake-dia-block-style style #:fill-paint 'LemonChiffon #:stroke-color 'transparent)]
          [(#:stu) (remake-dia-block-style style #:fill-paint 'LightGreen #:stroke-color 'transparent)]))))

(define pbl-desc
  #hasheq((#:Researcher . "Curriculum Designer")
          (#:Teacher . "Instructor")
          (#:Student . "Student")
          (#:Parent . "Parent")
          (arch#dev . "设计教学引擎")
          (code#dev . "实现教学引擎")
          (asset . "预制素材资源")
          (api#dev . "规范命名 API")
          (train . "培训系统用法")
          (doc . "编写用户文档")
          (example#edu . "编写范例项目")
          (bdd#dev . "行为驱动开发")
          (deploy . "部署系统\n同步课程源码")
          (develop#edu . "研发课程")
          (demo#edu . "编写演示程序")
          (fit#edu . "裁剪课程项目")
          (dup#stu . "完成课程项目")
          (experiment#edu . "设计实验\n准备实验环境")
          (experiment#stu . "做实验")
          (cthinking#stu . "分解、识别\n抽象、建模")
          (trade-off#edu  . "权衡新旧知识点")
          (report#stu . "项目总结与报告")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-use-case-diagram! role.dia #:start '#:Engineer
  #:parameterize ([default-uc-block-theme-adjuster pbl-colorize])
  [#:frame 'White #:start-name "Software\nEngineer" #:block-desc pbl-desc] #:-
  [#:zone 'JrLab 'system #:desc title
   [(actor-use 2 -pi/4 'arch#dev)
    (actor-use 2 0 'code#dev)
    (actor-use 2 +pi/4 'train)
    
    (jump-to 'code#dev)
    (case-include 2.5 -pi/8 'bdd#dev)
    (case-extend 2.5 +0 'asset)
    
    (jump-to 'arch#dev)
    (case-include 2.5 -pi/12 'api#dev)
    (case-include 'bdd#dev)
    
    (jump-to 'train)
    (case-include 2 -pi/12 'doc)
    (case-include 2 +pi/12 'example#edu)
    (case-extend 2 +5pi/12 'develop#edu)]]
   
  (jump-to -0.5+8i '#:Teacher)
  [#:with-zone 'JrLab
   [(actor-use 2 -pi/6 'fit#edu)
    (actor-use 3 0 'deploy)
    (actor-use 3+9i 'report#stu)]]
  
  [#:tree (jump-to -0.5+4i '#:Researcher)
   [=> (actor-use 2.0 +pi/8 'demo#edu)
       (actor-use 'develop#edu)
       (case-include 'example#edu)]
   [=> (actor-generalize '#:Teacher)]]

  (jump-to 'develop#edu)
  [#:with-zone 'JrLab
   [(case-include 2.5 -pi/12 'trade-off#edu)
    (case-extend 2.0 +pi/8 'experiment#edu)
    (case-include 3.0+5.5i 'cthinking#stu)]]

  [#:tree (jump-to 6+8i '#:Student)
   [=> (actor-use 2 -7pi/8 'dup#stu)
       (actor-use 3.2 -5pi/8 'experiment#stu)
       (actor-use 'report#stu)]
   [=> (actor-use 'deploy)]]
  
  [#:with-zone 'JrLab
   [(extend 'fit#edu 'cthinking#stu)
    (extend 'dup#stu 'cthinking#stu)
    (extend 'dup#stu 'experiment#stu)]]

  (jump-to 6+6i '#:Parent)
  (note '#:Parent 1 pi/4 "首任老师" "表现出重视" "支持孩子学习"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  role.dia)
