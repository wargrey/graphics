#lang typed/racket/base

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Wait : Symbol (string->symbol "Wait User..."))
(define Read : Keyword (string->keyword ">>:Read\nExpression"))
(define Print : Symbol (string->symbol "<<:Print\nResult"))
(define Print-Error : Symbol (string->symbol "<<:Show\nError Message"))

(define-flowchart! repl [] #:-
  ; Portion on Page 1
  (move-down 1 'Initialization!)
  (move-down 1 Wait)
  (move-down 1 Read)
  (move-down 1 'λEvaluate)
  (move-down 1 '#:-+)
  (move-down 1 '#:Exit?)
  (move-down 1 '#:Void? "False")
  (move-down 1 Print "No")

  (move-down 0.5)
  (move-left 1.0 '.loop)
  (step-up-right Wait "LOOP")

  (jump-back)
  (move-left '.loop #false "Yes")

  (jump-back)
  (move-right 1.0 #false "True")
  (move-down Print)
  (move-down 0.425)
  (turn-left-down)
  (turn-down-right)
  (move-down 0.5 '@E)

  (jump-back)
  (move-right 2 #false "Error/Failure")
  (move-down Print Print-Error)
  (step-down-left '.loop)
  (move-left '.loop)
  
  (jump-back)
  (move-right 2 '/db=/|Append to .history|)

  ; Portion on the same page, located at Grid (5, 0)
  (jump-to 5 '@E.)
  (move-down 1 'λ|Call Exit Handler|)
  (move-down 1 '#:Byte? "Check Exit Status")
  (move-down 1 '#:Zero? "Y")
  (move-down-left 1 1 'Exit$ "True")

  (jump-back)
  (move-down-right 1 1 '|Exit with Error Status| "False")
  (move-down 1 '&Page2)
  
  (jump-back)
  (step-left-down 'Exit$ "N")

  ; Portion on Page 2, located at Grid (9, 0)
  ; Referenced by Page 1
  (jump-to 8 '&Page1.)
  (move-down 1 '|Report Error|)
  (move-down 1 '#:-.+)
  (move-down 2 '=continue- "Ignored")
  
  (jump-back)
  (move-right 1.5)
  (move-down 0.5 '--|Awkward and Check|--)
  (move-down 1 '--Confirm--)
  (step-down-left '=continue-)
  (move-down 0.5 '#:.hide)

  ; Free Edges are designed for drawing decorative things,
  ;   such as separators, swimlanes
  (jump-to 7)
  (move-down '@E #false "= PAGE SEPARATOR ="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  ;(default-diaflow-fill-paint #false)
  
  (geo-frame (dia-path-flow repl #:start-name "REPL\n(Shell)") #:background 'White))
