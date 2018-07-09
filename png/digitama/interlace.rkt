#lang typed/racket/base

;;; https://www.w3.org/TR/PNG/#7Scanline
;;; https://www.w3.org/TR/PNG/#8Interlace

(provide scanline-recombine)

(require bitmap/stdio)

(require "enum.rkt")

(define scanline-recombine : (-> (Listof Bytes) PNG-Interlace-Method Bytes Positive-Fixnum Positive-Fixnum Positive-Byte Void)
  (lambda [senilnacs interlace pixels fxwidth fxheight fxcount]
    (void)))
