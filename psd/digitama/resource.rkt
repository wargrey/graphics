#lang typed/racket/base

(provide (all-defined-out))

(define-type PSD-Image-Resource-Segment (Vector String Bytes Fixnum Index))
(define-type PSD-Image-Resources (HashTable Integer (U PSD-Image-Resource-Segment PSD-Resource)))
(define-type PSD-Resource-Parser (-> Integer String Bytes Fixnum Index (Listof Any) PSD-Resource))

(struct PSD-Resource ([id : Integer] [name : String]))
