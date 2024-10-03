#lang typed/racket/base

(require geofun/vector)
(require diafun/flowchart)
(require diafun/digitama/style/shared)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Read : Symbol (string->symbol ">>:Read\nExpression"))
(define Print : Symbol (string->symbol "Print\nResult<<"))

(define-flowchart! repl [] #:-
  ; Portion on Page 1
  (move-down 1 'Initialization!)
  (move-down 1 Read)
  (move-down 1 'λEvaluate)
  (move-down 1 '#:Exit?)
  (move-down 1 '#:Void? "No")
  (move-down 1 Print "No")

  (move-down 0.5)
  (move-left 1.0)
  (step-up-right Read "LOOP")

  (jump-back)
  (move-left 1.0 #false "Yes")

  (jump-back)
  (move-right 1.0 #false "Yes")
  (move-down Print)
  (move-down 0.75)
  (move-left Print)
  (move-down 0.5 '@E)

  ; Portion on the same page, located at Grid (4, 0)
  (jump-to 4 '@E.)
  (move-down 1 'λ|Call Exit Handler|)
  (move-down 1 '#:Byte? "Check Exit Status")
  (move-down 1 '#:Zero? "True")
  (move-down-left 1 1 'Exit$ "True")

  (jump-back)
  (move-down-right 1 1 '--|Exit with Error Status|-- "False")
  (move-down 1 '&Page2)
  
  (jump-back)
  (step-left-down 'Exit$ "False")

  ; Portion on Page 2, located at Grid (2, 8)
  ; Referenced by Page 1
  (jump-to 2+8i '&Page1.)
  (move-down 1 '|Report Error|)
  (move-down 1 ':Confirm)
  (move-down 0.5 '#:.hide)

  ; Free Edges are designed for drawing decorative things,
  ;   such as separators, swimlanes
  (jump-to -1.0+7.5i)
  (move-right 6 #false "= PAGE SEPARATOR ="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  ;(default-diaflow-fill-paint #false)
  ;(default-diaflow-edge-label-rotate? #true)
  ;(default-diaflow-edge-label-inline? #false)
  
  (geo-frame (dia-path-flow repl #:start-name "REPL\n(Shell)") #:background 'White))
