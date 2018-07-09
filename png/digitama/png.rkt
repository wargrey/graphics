#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)
(require "enum.rkt")

(struct PNG-Header Bitmap
  ([color-type : PNG-Color-Type]
   [compression-method : PNG-Compression-Method]
   [compression-level : PNG-Compression-Level]
   [filter-method : PNG-Filter-Method]
   [interlace-method : PNG-Interlace-Method])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define png-color-type->sample-count : (-> PNG-Color-Type Positive-Byte)
  (lambda [color]
    (case color
      [(Truecolor+Alpha) 4]
      [(Grayscale+Alpha) 2]
      [(Truecolor Indexed-color) 3]
      [(Grayscale) 1]
      [else 4])))
