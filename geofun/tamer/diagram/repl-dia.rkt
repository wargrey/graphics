#lang typed/racket/base

(require geofun/vector)
(require geofun/flowchart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-dryland-wani! repl [64 64 #:anchor '^REPL] #:-
  (move-down 1 'initialization!)
  (move-down 1 '>>Read)
  (move-down 1 'exit?)
  (move-down 1 'Evaluate)
  (move-down 1 'Print<<)

  (move-down 0.5)
  (move-left 2)
  (move-up 4)
  (move-right 2)

  (jump-back 'exit?)
  (move-right 2)
  (move-down 2.75)
  (move-left 2)
  (move-down 0.5 'Exit$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-path-flow repl))
