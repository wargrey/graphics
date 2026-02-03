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
(define Print : Symbol (string->symbol "<<:Print\nOutput"))
(define Print-Error : Symbol (string->symbol "<<:Show\nError Message"))

(define Page1 : Symbol (string->symbol "&Page 1."))
(define Page2 : Symbol (string->symbol "&Page 2"))

(define frame (make-geo-box #:border (default-border-paint) #:background 'Snow))

(define-flowchart! repl.dia [#:start-name "REPL\n(Shell)" #:frame frame] #:-
  ; Portion on Page 1
  (move-down 1 'Initialize!)
  (move-down 1.2 Create)
  (move-down 1.2 Wait)
  
  [#:tree (move-down 1 '-type+)
   [=> (move-right 1 #false "Pressed Up/Down")
       (move-down 0.75 Select)
       (move-down 0.75)
       (move-left 1 '+expr-)]
   
   [=> (move-left 1.0 #false "Input Expression")
       (move-down 0.75 Read)
       (L-step '+expr-)
       (move-down 1.0 Update)
       (move-down 1.2 'λEvaluate)

       [#:tree (move-down 1 '#:-+)
        [=> (move-left 1.5 Print-Error "Fatal")
            (move-left 1 '+loop-)
            (L-step Wait "Loop")]

        [=> [#:tree (move-down 1.2 '#:Void? "datum")
             [=> (move-left 1.5 Print "No")
                 (move-left '+loop- '+stdout-)
                 (move-up '+loop-)]

             [=> (move-down 0.75 '.bot "Yes")
                 (T-step '+stdout-)]]]
        
        [=> (move-right 1.5 '@E "Exit")]]]]
  
  (jump-to Create)
  (jump-right 2 '/doc/.history)
  (move-to Create '#:load)

  ; Portion on the same page, located at Grid (4, 0)
  (jump-to 4 '@E.)
  (move-down 1 Sync)
  (move-down 1.2 'λ|Call Exit Handler|)

  [#:seq
   [(move-down 1.2 'Byte? "Check Exit Status")
    [=> (move-left 1 #false "N")
        (move-down 2 'Exit$)]]
   [(move-down 1 'Zero? "Y")
    =>
    (move-down-right 1 'Exit$ '|Exit with Error Status| "False")
    (move-down 1 Page2)]
   [(move-to 'Exit$ "True")]]

  (jump-to Sync)
  (T-step '/doc/.history '#:save)

  ; Portion on Page 2, located at Grid (7, 0)
  ; Referenced by Page 1
  (jump-to 7 Page1)
  (move-down 1 '|Report Error|)

  [#:tree (move-down 1 '#:-.=)
   [=> (move-down 3 '=continue- (<span> '([strikethrough . true]) "Ignored"))]
   [=> (move-right 1)
       (move-down 1 '--|Awkward and Check|--)
       (move-down 1 '--Confirm--)
       (L-step '=continue-)
       (move-down 1 'Close$)]]

  ; Free Edges are designed for drawing decorative things,
  ;   such as separators, swimlanes
  (jump-to 6)
  (move-down '.bot #false "= PAGE SEPARATOR ="))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  ; 36 - 72ms
  (time (geo-freeze repl.dia)))
