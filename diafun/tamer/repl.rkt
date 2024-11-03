#lang typed/racket/base

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Wait : Symbol (string->symbol "Wait for You to Type..."))
(define Read : Symbol (string->symbol ">>:Read\nExpression"))
(define Select : Symbol (string->symbol ">>Select Expression\nFrom History List"))
(define Create : Keyword (string->keyword "/proc/Create\nHistory List"))
(define Update : Symbol (string->symbol "/proc/Update\nHistory List"))
(define Sync : Keyword (string->keyword "/proc/Sync\nHistory List"))
(define Print : Symbol (string->symbol "<<:Print\nResult"))
(define Print-Error : Symbol (string->symbol "<<:Show\nError Message"))

(define-flowchart! repl.dia [#:start-name "REPL\n(Shell)" #:border (default-border-paint)] #:-
  ; Portion on Page 1
  (move-down 1 'Initialization!)
  (move-down 1 Create)
  (move-down 1 Wait)
  (move-down 1 '#:-type+)
  (move-down 0.5)
  (move-left 1.0 #false "Input Expression")
  (move-down 1 Read)
  (L-step 1)
  (move-down 0.5 '=expr-)
  (move-down 1 Update)
  
  (jump-back)
  (move-down 0.5)
  (move-right 1 #false "Pressed Up/Down")
  (move-down 1 Select)
  (L-step -1+i)
  
  (jump-to Update)
  (move-down 1 'λEvaluate)
  (move-down 1 '#:-+)
  (move-down 1.2 '#:Void? "datum")
  (move-left 1.5 Print "No")
  (move-left 1.0 '.loop)
  (L-step Wait "LOOP")

  (jump-back)
  (move-down 0.5 #false "Yes")
  (T-step '.loop)

  (jump-back)
  (move-right 1.5 #false "Exit")
  (move-down '#:Void?)
  (move-down 0.75)
  (move-left '#:Void?)
  (move-down 0.50 '@E)

  (jump-back '#:-+)
  (move-left 1.5 Print-Error "Fatal")
  (move-left '.loop)
  
  (jump-back)
  (jump-right 2 '/doc/.history)
  (move-to Create #false "load")

  ; Portion on the same page, located at Grid (4, 0)
  (jump-to 4 '@E.)
  (move-down 1 Sync)
  (move-down 1 'λ|Call Exit Handler|)
  (move-down 1 '#:Byte? "Check Exit Status")
  (move-down 1 '#:Zero? "Y")
  (move-down-left 1 1 'Exit$ "True")

  (jump-back)
  (move-down-right 1 1 '|Exit with Error Status| "False")
  (move-down 1 '&Page2)
  
  (jump-back)
  (T-step 'Exit$ "N")

  (jump-back)
  (T-step '/doc/.history "save")

  ; Portion on Page 2, located at Grid (7, 0)
  ; Referenced by Page 1
  (jump-to 7 '&Page1.)
  (move-down 1 '|Report Error|)
  (move-down 1 '#:-.+)
  (move-down 3 '=continue- "Ignored")
  
  (jump-back)
  (move-right 1)
  (move-down 1 '--|Awkward and Check|--)
  (move-down 1 '--Confirm--)
  (L-step '=continue-)
  (move-down 0.5)

  ; Free Edges are designed for drawing decorative things,
  ;   such as separators, swimlanes
  (jump-to 6)
  (move-down '@E #false "= PAGE SEPARATOR ="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  ;(default-diaflow-fill-paint #false)

  #;(geo-save repl.dia (build-path (find-system-path 'desk-dir) "repl.png") #:format 'png)

  ; 36ms
  (time (geo-freeze repl.dia)))
