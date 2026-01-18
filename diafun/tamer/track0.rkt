#lang typed/racket/base

(require diafun/class)
(require diafun/usecase)
(require diafun/flowchart)

(define-gomamon! goma [108 108] #:-)

(dia-track-flow goma)
(dia-track-use-case goma)
(dia-track-class goma)
