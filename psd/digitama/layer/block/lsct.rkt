#lang typed/racket/base

(require "../../image/enum.rkt")
(require "../../parser.rkt")
(require "../block.rkt")

(unsafe-provide lsct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lsct : (-> Bytes Index Index Null PSD-Layer-Tagged-Block)
  (lambda [layer-info start size argl]
    (psd:ltb:section:divider (parse-uint32 layer-info start)
                             (and (>= size 12) (parse-keyword layer-info (unsafe-idx+ start 8) psd-blend-mode?))
                             (and (>= size 16) (parse-uint32 layer-info (unsafe-fx+ start 12))))))
