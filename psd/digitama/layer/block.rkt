#lang typed/racket/base

(provide (all-defined-out) unsafe-provide)

(require typed/racket/unsafe)

(require "../image/enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type PSD-Layer-Tagged-Blocks (Immutable-HashTable Symbol PSD-Layer-Tagged-Block))
(define-type PSD-Layer-Tagged-Block-Parser (-> Bytes Index Index (Listof Any) PSD-Layer-Tagged-Block))

(struct psd-layer-tagged-block () #:type-name PSD-Layer-Tagged-Block #:transparent)

(struct psd:ltb:id psd-layer-tagged-block ([data : Index]) #:transparent)
(struct psd:ltb:unicode:name psd-layer-tagged-block ([data : String]) #:transparent)
(struct psd:ltb:text:engine psd-layer-tagged-block ([data : Bytes]) #;#:transparent)

(struct psd:ltb:section:divider psd-layer-tagged-block
  ([type : Integer]
   [blend-mode : (Option PSD-Blend-Mode)]
   [subtype : (Option Integer)])
  #:type-name PSD:LTB:Section:Divider
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define /psd/layer/blocks : Path (collection-file-path "block" "psd" "digitama" "layer"))
(define psd-empty-tagged-blocks : PSD-Layer-Tagged-Blocks (hasheq))
