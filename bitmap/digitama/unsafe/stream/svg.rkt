#lang racket/base

(provide (all-defined-out))

(require "write.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-svg-stream-pool-size 4096)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-create-svg-stream-surface
  (lambda [/dev/svgout flwidth flheight pool-size]
    (define svg-write (make-cairo-vector-surface-writer /dev/svgout pool-size))
    (define surface (cairo_svg_surface_create_for_stream svg-write flwidth flheight))
    
    (let ([status (cairo_surface_status surface)])
      (unless (unsafe-fx= status CAIRO_STATUS_SUCCESS)
        (raise-arguments-error 'cairo-create-svg-stream-surface (cairo_status_to_string status)
                               "width" flwidth "height" flheight "pool size" pool-size)))
    
    surface))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-svg-stream-write
  (lambda [/dev/svgout flwidth flheight λmake]
    (cairo-vector-stream-write /dev/svgout cairo-create-svg-stream-surface
                               flwidth flheight default-svg-stream-pool-size λmake)))

(define make-cairo-svg-stream-bytes
  (lambda [flwidth flheight λmake]
    (make-cairo-vector-stream-bytes cairo-create-svg-stream-surface
                                    flwidth flheight default-svg-stream-pool-size λmake)))

(define open-input-cairo-svg-stream
  (lambda [flwidth flheight λmake]
    (open-cairo-input-vector-stream cairo-create-svg-stream-surface
                                    flwidth flheight default-svg-stream-pool-size λmake)))
