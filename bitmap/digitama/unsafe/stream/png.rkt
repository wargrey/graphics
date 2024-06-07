#lang racket/base

(provide (all-defined-out))

(require "write.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-png-stream-pool-size 8192)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-png-stream-write
  (lambda [/dev/pngout λsurface]
    (cairo-image-stream-write /dev/pngout default-png-stream-pool-size λsurface)))

(define make-cairo-png-stream-bytes
  (lambda [λsurface]
    (make-cairo-image-stream-bytes default-png-stream-pool-size λsurface)))

(define open-input-cairo-png-stream
  (lambda [λsurface]
    (open-cairo-input-image-stream default-png-stream-pool-size λsurface)))
