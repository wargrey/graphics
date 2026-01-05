#lang typed/racket/base

(require geofun/vector)
(require geofun/markup)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Wait : Symbol (string->symbol "Wait for You to Type..."))
(define Read : Symbol (string->symbol ">>:Read\nExpression"))
(define Select : Symbol (string->symbol ">>Select Expression\nFrom The History List"))
(define Create : Keyword (string->keyword "/proc/Create\nHistory List"))
(define Update : Symbol (string->symbol "/proc/Update\nHistory List"))
(define Sync : Keyword (string->keyword "/proc/Sync\nHistory List"))
(define Print : Symbol (string->symbol "<<:Print\nResult"))
(define Print-Error : Symbol (string->symbol "<<:Show\nError Message"))

(define Page1 : Symbol (string->symbol "&Page 1."))
(define Page2 : Symbol (string->symbol "&Page 2"))

(define-flowchart! repl.dia [#:start-name "REPL\n(Shell)" #:border (default-border-paint) #:background 'Azure] #:-
  ; Portion on Page 1
  (move-down 1 'Initialization!)
  (move-down 1 Create)
  (move-down 1 Wait)
  
  [#:tree (move-down 1 '-type+)
   [=> (move-down 0.5)
       (move-right 1 #false "Pressed Up/Down")
       (move-down 1 Select)
       (L-step -1+i)]
   
   [=> (move-down 0.5)
       (move-left 1.0 #false "Input Expression")
       (move-down 1 Read)
       (L-step 1)
       (move-down 0.5 '=expr-)
       (move-down 1 Update)
       (move-down 1 'λEvaluate)

       [#:tree (move-down 1 '#:-+)
        [=> [#:tree (move-down 1.2 '#:Void? "datum")
             [=> (move-left 1.5 Print "No")
                 (move-left 1.0 '.loop)
                 (L-step Wait "LOOP")]
             
             [=> (move-down 0.5 #false "Yes")
                 (T-step '.loop)]]]
        
        [=> (move-right 1.5 #false "Exit")
            (move-down '#:Void?)
            (move-down 0.75)
            (move-left '#:Void?)
            (move-down 0.50 '@E)]
        
        [=> (move-left 1.5 Print-Error "Fatal")
            (move-left '.loop)]]]]
  
  (jump-to Create)
  (jump-right 2 '/doc/.history)
  (move-to Create #false "load")

  ; Portion on the same page, located at Grid (4, 0)
  (jump-to 4 '@E.)
  (move-down 1 Sync)
  (move-down 1 'λ|Call Exit Handler|)

  [#:seq
   [(move-down 1 'Byte? "Check Exit Status") [=> (move-left 1 #false "N")
                                                 (move-down 2 'Exit$)]]
   [(move-down 1 'Zero? "Y") =>
                             (move-down-right 1 1 '|Exit with Error Status| "False")
                             (move-down 1 Page2)]
   [(move-to 'Exit$ #false "True")]]

  (jump-to Sync)
  (T-step '/doc/.history "save")

  ; Portion on Page 2, located at Grid (7, 0)
  ; Referenced by Page 1
  (jump-to 7 Page1)
  (move-down 1 '|Report Error|)

  [#:tree (move-down 1 '#:-.+)
   [=> (move-down 3 '=continue- (<span> '([strikethrough . true]) "Ignored"))]
   [=> (move-right 1)
       (move-down 1 '--|Awkward and Check|--)
       (move-down 1 '--Confirm--)
       (L-step '=continue-)
       (move-down 0.5)]]

  ; Free Edges are designed for drawing decorative things,
  ;   such as separators, swimlanes
  (jump-to 6)
  (move-down '@E #false "= PAGE SEPARATOR ="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  ;(default-diaflow-fill-paint #false)

  ; 36 -> 72ms
  (time (geo-freeze repl.dia)))
