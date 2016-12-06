#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Color space conversion")

(define deps '("base" "math-lib" "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '(wargrey))

(define scribblings '(["tamer/colorspace.scrbl" (main-doc) ("Math and Science")]))
