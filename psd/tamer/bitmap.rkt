#lang racket/base

(require psd/bitmap)

(require racket/runtime-path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-runtime-path vanishing.psd "samples/Vanishing.Point.psd")
(define-runtime-path megaphyll.psd "samples/megaphyll.psd")

(define vanishing.bmp (read-psd-bitmap vanishing.psd))
(define megaphyll.bmp (read-psd-bitmap megaphyll.psd))

vanishing.bmp
megaphyll.bmp
