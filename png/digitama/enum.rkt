#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/stdio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* png-color-type #:+> PNG-Color-Type
  color-type->integer integer->color-type
  [Grayscale 0] [Truecolor 2] [Indexed-color 3]
  [Grayscale+Alpha 4] [Truecolor+Alpha 6])

(define-enumeration* png-compression-method #:+> PNG-Compression-Method ; order matters
  compression-method->integer integer->compression-method
  [0 zlib])

(define-enumeration* png-compression-level #:+> PNG-Compression-Level ; order matters
  compression-level->integer integer->compression-level
  [0 fastest fast default slowest])

(define-enumeration* png-filter-method #:+> PNG-Filter-Method ; order matters
  filter-method->integer integer->filter-method
  [0 Method0])

(define-enumeration* png-interlace-method #:+> PNG-Interlace-Method ; order matters
  interlace-method->integer integer->interlace-method
  [0 None Adam7])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-unnamed-enumeration png-bit-depth : Byte #:with png-bit-depth-identity #:-> Positive-Byte [1 2 4 8 16])

(define png-bit-depth-acceptable? : (-> Byte PNG-Color-Type Boolean : #:+ Positive-Byte)
  (lambda [bit-depth color-type]
    (cond [(= bit-depth 8) #true]
          [(= bit-depth 1) (or (eq? color-type 'Index-color) (eq? color-type 'Grayscale))]
          [(= bit-depth 2) (or (eq? color-type 'Index-color) (eq? color-type 'Grayscale))]
          [(= bit-depth 4) (or (eq? color-type 'Index-color) (eq? color-type 'Grayscale))]
          [(= bit-depth 16) (not (eq? color-type 'Indexed-color))]
          [else #false])))
