#lang typed/racket/base

(require diafun/usecase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Document : Symbol (string->symbol "Write Documents"))
(define Development : Symbol (string->symbol "Write Code"))
(define Design : Symbol (string->symbol "Design Software System"))
(define Test : Symbol (string->symbol "Test"))
(define Debug : Symbol (string->symbol "Debug"))
(define Readability : Symbol (string->symbol "Make API Regular"))
(define Management : Symbol (string->symbol "Manage Projects"))
(define Engineer : Keyword (string->keyword "Software\nEngineer"))

(define-use-case! ucase.dia #:start '#:Coder [] #:-
  (radial-move 2 -45 Development)
  (radial-move 2 +00 Document)
  (radial-move 2 +45 Management)

  (jump-to Development)
  (radial-move 2.5 -15 Test " <<include>> ")
  (radial-move 2.5 +15 Debug " <<include>> ")

  (jump-to +3i Engineer)
  (radial-move 1.5 +00 Design)
  (move-to '#:Coder)

  (jump-to Design)
  (radial-move 2.5 +00 Readability " <<extend>> ")

  (jump-to 0.5-3.5i '.ucase)
  (move-right 4.5 #false "Use Case Diagram")
  (move-down 8.0)
  (move-left 4.5)
  (move-up '.ucase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  #;(default-diauc-fill-paint #false)
  #;(geo-save ucase.dia (build-path (find-system-path 'desk-dir) "repl.png") #:format 'png)

  ucase.dia)
