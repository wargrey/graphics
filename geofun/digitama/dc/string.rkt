#lang typed/racket/base

(provide (all-defined-out))

(require "../self.rkt")

(require "../unsafe/dc/text-layout.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:string geo
  ([body : String]
   [lines : (Listof Geo-Text-Line)]
   [alignment : Geo-Text-Alignment])
  #:type-name Geo:String
  #:transparent)
