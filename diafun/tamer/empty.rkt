#lang typed/racket/base

(require diafun)

(define-gomamon! goma [108 108] #:-)

(dia-track-flow goma)
(dia-track-use-case goma)
(dia-track-simple-class goma)
