#lang typed/racket/base

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Read : Symbol (string->symbol ">>Read\nExpression"))
(define Print : Symbol (string->symbol "Print\nResult<<"))

(define-flowchart! repl [] #:-
  ; Portion on Page 1
  (move-down 1 'Initialization!)
  (move-down 1 Read)
  (move-down 1 '->Evaluate)
  (move-down 1 '#:Exit?)
  (move-down 1 '#:Void? "False")
  (move-down 1 Print "False")

  (move-down 0.5)
  (move-left 1.0)
  (step-up-right Read "LOOP")

  (jump-back)
  (move-left 1.0 #false "True")

  (jump-back)
  (move-right 1.0 #false "True")
  (move-down Print)
  (move-down 0.75)
  (move-left Print)
  (move-down 0.5 '@E)

  ; Portion on the same page, located at Grid (4, 0)
  (jump-to 4 '@E.)
  (move-down 1 '->|(apply atexit)|)
  (move-down 0.5 '-|Check Exit Status|-)
  (move-down 0.5 '#:Byte?)
  (move-down 1 '#:Zero? "Yes")
  (move-down-left 1 1 'Exit$ "Yes")

  (jump-back)
  (move-down-right 1 1 '|Exit with Error Status| "No")
  (move-down 1 '&P2)
  
  (jump-back)
  (step-left-down 'Exit$ "No")

  ; Portion on Page 2, referenced by Page 1
  ; Located at Grid (2, 8)
  (jump-to 2+8i '&P1.)
  (move-down 1 '|Report Error|)
  (move-down 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define colorful-edge : Dia-Edge-Style-Make
    (lambda [source target]
      (cond [(memq source (list Read Print)) (make-diaflow-arrow-style #:line-paint 'ForestGreen)]
            [else (case source
                    [(#:home Exit$) (make-diaflow-arrow-style #:line-paint 'DimGray)]
                    [(Initialization!) (make-diaflow-arrow-style #:line-paint 'Red)]
                    [(#:Exit? -YES-) (make-diaflow-arrow-style #:line-paint (if (not target) 'SeaGreen 'Blue))]
                    [(#:Void?) (make-diaflow-arrow-style #:line-paint (if (not target) (desc-stroke #:color 'SeaGreen #:width 2.0 #:dash 'long-dash) 'Blue))]
                    [(#:Byte?) (make-diaflow-arrow-style #:line-paint (if (eq? target 'Exit$) (desc-stroke #:color 'SeaGreen #:width 2.0 #:dash 'long-dash) 'Blue))]
                    [(->Evaluate) (make-diaflow-arrow-style #:line-paint 'Orange)])])))

  (default-diaflow-arrow-style-make colorful-edge)
  (default-diaflow-start-stroke-paint 'Chocolate)
  (default-diaflow-stop-stroke-paint 'Chocolate)
  (default-diaflow-fill-paint #false)
  
  (geo-frame (dia-path-flow repl #:start-name "REPL\n(Shell)")))
