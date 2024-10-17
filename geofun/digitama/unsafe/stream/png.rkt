#lang typed/racket/base

(provide (all-defined-out))

(require "image.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-png-stream-pool-size : Positive-Index 8192)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-png-stream-write : (-> (U Path-String Output-Port) Cairo-Image-Stream-Source-Make Void)
  (lambda [/dev/pngout λsurface]
    (cairo-image-stream-write /dev/pngout default-png-stream-pool-size λsurface)))

(define make-cairo-png-stream-bytes : (-> Cairo-Image-Stream-Source-Make Bytes)
  (lambda [λsurface]
    (make-cairo-image-stream-bytes default-png-stream-pool-size λsurface)))

(define open-cairo-input-png-stream : (-> Cairo-Image-Stream-Source-Make Input-Port)
  (lambda [λsurface]
    (open-cairo-input-image-stream default-png-stream-pool-size λsurface)))
