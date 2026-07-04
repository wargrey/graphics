#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/layer/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layer-location : (-> PSD-Layer (Values Fixnum Fixnum))
  (lambda [self]
    (define record : PSD-Layer-Record (psd-layer-object-record self))
    (values (psd-layer-header-x record) (psd-layer-header-y record))))

(define psd-layer-size : (-> PSD-Layer-Object (Values Index Index))
  (lambda [self]
    (define record : PSD-Layer-Record (psd-layer-object-record self))
    (values (psd-layer-header-width record) (psd-layer-header-width record))))
