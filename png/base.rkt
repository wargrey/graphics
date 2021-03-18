#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)

(require "digitama/png.rkt")
(require "digitama/stdin.rkt")
(require "digitama/enum.rkt")
(require "digitama/zlib.rkt")
(require "digitama/filtering.rkt")
(require "digitama/interlace.rkt")

(struct PNG PNG-Header () #:transparent)

(define-read-bitmap #:-> PNG
  (lambda [/dev/stdin density]
    (define-values (width height bitdepth color-type compression-method filter-method interlace-method) (read-png-header /dev/stdin))
    (define chunks (read-png-nonIDATs /dev/stdin color-type))
    (define-values (zlib extra-chunks) (read-png-IDATs /dev/stdin))
    (define data-size (bytes-length zlib))
    (when (< data-size 6) (throw-check-error /dev/stdin 'png "IDAT chunks do not contain a valid zlib stream"))
    (define-values (flevel adler32 ?dictid) (parse-png-zlib /dev/stdin zlib 0 data-size))
    (when (and ?dictid) (throw-check-error /dev/stdin 'png "unknown preset dictionary ~a is present" ?dictid))
    (define zlib-block-start (if (not ?dictid) 2 6))
    (define compression-level (integer->compression-level flevel throw-range-error*))
    (define fxcount (png-color-type->sample-count color-type))

    (create-bitmap PNG (bitmap-port-source /dev/stdin)
                   density width height fxcount bitdepth color-type
                   compression-method compression-level filter-method interlace-method
                   (λ [pixels fxwidth fxheight]
                     (define blocks : (Listof Bytes)
                       (png-zlib-inflate /dev/stdin zlib zlib-block-start (assert (- data-size 4) index?) adler32))
                     (define senilnacs : (Listof Bytes)
                       (filter-scanline-reconstruct /dev/stdin blocks fxwidth fxcount filter-method))
                     (scanline-recombine senilnacs interlace-method pixels fxwidth fxheight fxcount)))))
