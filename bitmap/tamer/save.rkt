#lang typed/racket/base

(require racket/port)
(require bitmap/base)

(require "zero.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tamer-save : (-> Bitmap Symbol Void)
  (lambda [bmp ext]
    (bitmap-save bmp (build-path (find-system-path 'desk-dir) (format "tamer.~a" ext)) #:format ext)))

;(tamer-save frame 'png)
;(tamer-save frame 'pdf)
;(tamer-save frame 'svg)
