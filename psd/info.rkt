#lang info

(define collection 'use-pkg-name)
(define pkg-desc "PSD: Read and Write Photoshop Documents")

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define deps '("base" "digimon" "graphics" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc" "digimon" "graphics"))

;(define scribblings '(["tamer/psd.scrbl" (main-doc) (parsing-library)]))
