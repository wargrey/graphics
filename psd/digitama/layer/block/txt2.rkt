#lang typed/racket/base

(require "../../parser.rkt")
(require "../block.rkt")

(unsafe-provide Txt2)

;;; TODO: https://github.com/layervault/psd-enginedata

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Txt2 : (-> Bytes Index Index Null PSD-Layer-Tagged-Block)
  (lambda [layer-info start size argl]
    (psd:ltb:text:engine (parse-nbytes layer-info start size))))
