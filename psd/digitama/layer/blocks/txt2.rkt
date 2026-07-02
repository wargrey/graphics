#lang typed/racket/base

(require "../../parser.rkt")
(require "../format.rkt")

(unsafe-provide Txt2)

;;; TODO: https://github.com/layervault/psd-enginedata

(define Txt2 : (-> Bytes Fixnum Index Null PSD-Layer-Text-Engine)
  (lambda [layer-info start size argl]
    (PSD-Layer-Text-Engine (make-special-comment (parse-nbytes layer-info start size)))))
