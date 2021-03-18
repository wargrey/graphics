#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)

(require "enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-binary-struct png-ihdr-chunk : PNG-Header
  ([width : (#:subint MUInt32 positive-index? #:-> Positive-Index)]
   [height : (#:subint MUInt32 positive-index? #:-> Positive-Index)]
   [bit-depth : Byte]
   [color-type : (#:enum Byte color-type->integer integer->color-type #:-> PNG-Color-Type)]
   [compression-method : (#:enum Byte compression-method->integer integer->compression-method #:-> PNG-Compression-Method)]
   [filter-method : (#:enum Byte filter-method->integer integer->filter-method #:-> PNG-Filter-Method)]
   [interlace-method : (#:enum Byte interlace-method->integer integer->interlace-method #:-> PNG-Interlace-Method)]))
