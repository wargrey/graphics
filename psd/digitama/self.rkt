#lang typed/racket/base

(provide (all-defined-out))
(provide PSD-Layer-Infobase)

(require bitmap/digitama/self)

(require "resource.rkt")
(require "image.rkt")

(require "layer.rkt")
(require "layer/format.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct psd-header bitmap
  ([channels : Positive-Byte]
   [depth : Positive-Byte]
   [color-mode : PSD-Color-Mode])
  #:type-name PSD-Header
  #:transparent)

(struct psd-section psd-header
  ([color-data : Bytes]
   [resources : (U PSD-Image-Resources Bytes)]
   [layers : (U (Listof PSD-Layer-Object) Bytes)]
   [global-mask : (U PSD-Global-Layer-Mask Bytes False)]
   [tagged-blocks : (U PSD-Layer-Infobase Bytes)]
   [compression-method : PSD-Compression-Method]
   [special-size : Positive-Byte])
  #:type-name PSD-Section
  #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (psd-ref! stx)
  (syntax-case stx [λ lambda]
    [(_ self field (λ [maybe-val] make-val ...))
     (with-syntax ([psd-field (datum->syntax #'field (string->symbol (format "psd-~a" (syntax-e #'field))))]
                   [set-psd-field! (datum->syntax #'field (string->symbol (format "set-psd-~a!" (syntax-e #'field))))])
       #'(let ([maybe-val (psd-field self)])
           (cond [(not (bytes? maybe-val)) maybe-val]
                 [else (let ([tmp (let () make-val ...)]) (set-psd-field! self tmp) tmp)])))]
    [(_ self field (lambda [maybe-val] make-val ...))
     #'(psd-ref! self field (λ [maybe-val] make-val ...))]))
