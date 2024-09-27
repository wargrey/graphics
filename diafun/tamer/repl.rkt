#lang typed/racket/base

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Read : Symbol (string->symbol ">>Read\nExpression"))
(define Print : Symbol (string->symbol "Print\nResult<<"))

(define-flowchart! repl [] #:-
  (move-down 1 'Initialization!)
  (move-down 1 Read)
  (move-down 1 '->Evaluate)
  (move-down 1 '#:Exit?)
  (move-down 1 '#:Void? "No")
  (move-down 1 Print "No")

  (move-down 0.5)
  (move-left 1.0)
  (move-up Read)
  (move-right Read)

  (jump-back)
  (move-left 1.0)

  (jump-back)
  (move-right 1.0 '-YES-)
  (move-right 0.2)
  (move-down Print)
  (move-down 0.75)
  (move-left Print)
  (move-down 0.5 'Exit$)

  (move-down 0.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define colorful-edge : Dia-Edge-Style-Make
    (lambda [source target]
      (cond [(memq source (list Read Print)) (make-diaflow-arrow-style #:line-paint 'ForestGreen)]
            [else (case source
                    [(#:home Exit$) (make-diaflow-arrow-style #:line-paint 'Gray)]
                    [(Initialization!) (make-diaflow-arrow-style #:line-paint 'Red)]
                    [(#:Exit? -YES-) (make-diaflow-arrow-style #:line-paint (if (not target) 'SeaGreen 'Blue))]
                    [(#:Void?) (make-diaflow-arrow-style #:line-paint (if (not target) (desc-stroke #:color 'SeaGreen #:width 2.0 #:dash 'long-dash) 'Blue))]
                    [(->Evaluate) (make-diaflow-arrow-style #:line-paint 'Orange)])])))

  (default-diaflow-arrow-style-make colorful-edge)
  (default-diaflow-start-stroke-paint 'Chocolate)
  (default-diaflow-stop-stroke-paint 'Chocolate)
  (default-diaflow-fill-paint #false)
  
  (geo-frame (dia-path-flow repl #:start-name "REPL")))
