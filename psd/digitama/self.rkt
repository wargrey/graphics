#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/digitama/self)

(require "image/enum.rkt")
(require "resource/self.rkt")

(require "layer/self.rkt")
(require "layer/block.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct psd-header bitmap
  ([channels : Positive-Byte]
   [depth : Positive-Byte]
   [color-mode : PSD-Color-Mode]
   [compression-method : PSD-Compression-Method]
   [special-size : Positive-Byte])
  #:type-name PSD-Header
  #:transparent)

(struct psd-body psd-header
  ([color-data : Bytes]
   [resources : PSD-Image-Resources]
   [layers : (Listof PSD-Layer)]
   [global-mask : (Option PSD-Global-Mask-Info)]
   [tagged-blocks : PSD-Layer-Tagged-Blocks])
  #:type-name PSD-Body
  #:mutable)
