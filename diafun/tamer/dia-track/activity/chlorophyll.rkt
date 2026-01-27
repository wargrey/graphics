#lang typed/racket/base

(provide (all-defined-out))

(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-activity-diagram! chlorophyll.dia [] #:-
  (move-right 0.5)
  (turn-right-down)
  (move-down 1 'Create)

  [#:tree (move-down 1 '?)]

  (move-down 1 'end$))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  chlorophyll.dia)
