#lang typed/racket/base

(require diafun/class)
(require diafun/usecase)
(require diafun/flowchart)
(require diafun/activity)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-gomamon! goma [108 108] #:-)

(module+ main
  (dia-track-flow goma)
  (dia-track-use-case goma)
  (dia-track-class goma)
  (dia-track-activate goma))
