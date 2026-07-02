#lang typed/racket/base

(require "../../image.rkt")
(require "../../parser.rkt")
(require "../format.rkt")

(unsafe-provide lsct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lsct : (-> Bytes Fixnum Index Null PSD-Layer-Section-Divider)
  (lambda [layer-info start size argl]
    (PSD-Layer-Section-Divider (parse-uint32 layer-info start)
                               (and (>= size 12) (parse-keyword layer-info (unsafe-fx+ start 8) psd-blend-mode?))
                               (and (>= size 16) (parse-uint32 layer-info (unsafe-fx+ start 12))))))
