#lang typed/racket/base

(require geofun/vector)
(require diafun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! repl [128 108] #:-
  (move-down 1 'Initialization!)
  (move-down 1 '>>Read)
  (move-down 1 'Evaluate)
  (move-down 1 'exit?)
  (move-down 1 'Print<<)

  (move-down 0.5)
  (move-left 1.5)
  (move-up 3.5)
  (move-to '>>Read)

  (jump-back 'exit?)
  (move-right 1.5)
  (move-down 1.75)
  (move-left 1.5)
  (move-down 0.5 'Exit$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define colorful-edge : Geo-Edge-Style-Make
    (lambda [source target]
      (case source
        [(#:home Exit$) (make-diaflow-arrow-style #:line-paint 'Gray)]
        [(initialization!) (make-diaflow-arrow-style #:line-paint 'Red)]
        [(>>Read Print<<) (make-diaflow-arrow-style #:line-paint 'ForestGreen)]
        [(exit?) (make-diaflow-arrow-style #:line-paint (if (eq? target 'Evaluate) 'SeaGreen 'Blue))]
        [(Evaluate) (make-diaflow-arrow-style #:line-paint 'Orange)]
        [else #false])))

  (default-diaflow-arrow-style-make colorful-edge)
  (default-diaflow-fill-paint #false)
  (default-diaflow-start-stroke-paint 'Chocolate)
  (default-diaflow-stop-stroke-paint 'Chocolate)
  
  (geo-frame (dia-path-flow repl #:start-name "REPL")))
