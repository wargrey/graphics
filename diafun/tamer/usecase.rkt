#lang typed/racket/base

(require diafun/usecase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Document : Symbol (string->symbol "Write Documents"))
(define Development : Keyword (string->keyword "Write Code"))
(define Design : Keyword (string->keyword "Design Software System"))
(define Test : Keyword (string->keyword "Test"))
(define Debug : Keyword (string->keyword "Debug"))
(define Readability : Keyword (string->keyword "Make API Regular"))
(define Management : Keyword (string->keyword "Manage Projects"))
(define Engineer : Keyword (string->keyword ":Software\nEngineer"))

(define-use-case! ucase.dia #:start ':Coder [] #:-
  (radial-move 2 -45 Development)
  (radial-move 2 0 Document)
  (radial-move 2 +45 Management)

  (jump-to Development)
  (radial-move 2.5 -15 Test "<<include>>")
  (radial-move 2.5 +15 Debug "<<include>>")

  (jump-to +3i Engineer)
  (radial-move 1.5 0 Design)
  (move-to ':Coder)

  (jump-to Design)
  (radial-move 2.5 0 Readability "<<extend>>")

  (jump-to 0.5-3.5i '.ucase)
  (move-right 4.5 #false "用例图")
  (move-down 8.0)
  (move-left 4.5)
  (move-up '.ucase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  #;(default-diauc-fill-paint #false)
  #;(geo-save ucase.dia (build-path (find-system-path 'desk-dir) "repl.png") #:format 'png)

  ucase.dia)
