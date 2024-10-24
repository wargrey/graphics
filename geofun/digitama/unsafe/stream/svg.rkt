#lang typed/racket/base

(provide (all-defined-out))

(require "vector.rkt")
(require "writer.rkt")

(require "../typed/cairo.rkt")
(require "../typed/more.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-svg-stream-pool-size : Positive-Index 4096)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-create-svg-stream-surface : Cairo-Vector-Stream-Surface-Make
  (lambda [/dev/svgout flwidth flheight pool-size]
    (define svg-write (make-cairo-vector-surface-writer /dev/svgout pool-size))
    (define surface (cairo_svg_surface_create_for_stream svg-write flwidth flheight))
    
    (let ([status (cairo_surface_status surface)])
      (unless (= status CAIRO_STATUS_SUCCESS)
        (raise-arguments-error 'cairo-create-svg-stream-surface (cairo_status_to_string status)
                               "width" flwidth "height" flheight "pool size" pool-size)))

    surface))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Master) cairo-svg-stream-write : (Cairo-Vector-Stream-Write Master)
  (lambda [/dev/svgout Width Height λdc master x0 y0 flwidth flheight]
    (cairo-vector-stream-write /dev/svgout cairo-create-svg-stream-surface
                               Width Height default-svg-stream-pool-size
                               λdc master x0 y0 flwidth flheight)))

(define #:forall (Master) make-cairo-svg-stream-bytes : (Cairo-Vector-Stream->Bytes Master)
  (lambda [Width Height density scale? λdc master x0 y0 flwidth flheight]
    (make-cairo-vector-stream-bytes cairo-create-svg-stream-surface
                                    Width Height default-svg-stream-pool-size
                                    λdc master x0 y0 flwidth flheight)))

(define #:forall (Master) open-cairo-input-svg-stream : (Open-Cairo-Input-Vector-Port Master)
  (lambda [Width Height density scale? λdc master x0 y0 flwidth flheight]
    (open-cairo-input-vector-stream cairo-create-svg-stream-surface
                                    Width Height default-svg-stream-pool-size
                                    λdc master x0 y0 flwidth flheight)))
