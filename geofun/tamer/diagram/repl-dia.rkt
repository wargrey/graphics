#lang typed/racket/base

(require geofun/vector)
(require geofun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! repl [96 64] #:-
  (move-down 1 'initialization!)
  (move-down 1 '>>Read)
  (move-down 1 'Evaluate)
  (move-down 1 'exit?)
  (move-down 1 'Print<<)

  (move-down 0.5)
  (move-left 1)
  (move-up 4)
  (move-right 1)

  (jump-back 'exit?)
  (move-right 1)
  (move-down 1.75)
  (move-left 1)
  (move-down 0.5 'Exit$)

  (move-down))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define colorful-edge : Geo-Edge-Style-Make
    (lambda [source target]
      (case source
        [(#:home Exit$) (make-geo-flow-arrow-style #:line-paint 'Gray)]
        [(initialization!) (make-geo-flow-arrow-style #:line-paint 'Red)]
        [(>>Read Print<<) (make-geo-flow-arrow-style #:line-paint 'Lime)]
        [(exit?) (make-geo-flow-arrow-style #:line-paint (if (eq? target 'Evaluate) 'SeaGreen 'Blue))]
        [(Evaluate) (make-geo-flow-arrow-style #:line-paint 'Orange)]
        [else #false])))

  (default-flow-arrow-style-make colorful-edge)
  (default-flow-start-stroke-paint 'Chocolate)
  (default-flow-stop-stroke-paint 'Chocolate)
  
  (geo-frame (geo-path-flow repl #:start-name "REPL")))
