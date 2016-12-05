#lang info

(define collection 'use-pkg-name)
(define pkg-desc "A CSS Engine written in pure Typed Racket")

(define deps '("base" "digimon" "images-lib"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '(wargrey))

(define scribblings '(["tamer/css.scrbl" (main-doc) (parsing-library)]))
