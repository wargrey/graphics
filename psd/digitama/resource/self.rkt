#lang typed/racket/base

(provide (all-defined-out))

(define-type PSD-Image-Resources (Immutable-HashTable Fixnum PSD-Resource))
(define-type PSD-Resource-Parser (-> Integer String Bytes Index Index (Listof Any) PSD-Resource))

(struct psd-resource ([id : Integer] [name : String]) #:type-name PSD-Resource)
