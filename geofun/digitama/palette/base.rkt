#lang typed/racket/base

(provide (all-defined-out))

(require "../base.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Palette-Index->Colors (-> Natural (Option Color) (Pairof FlRGBA FlRGBA)))
(define-type Palette-Color-Adapter (-> Color (Option Color) FlRGBA))
