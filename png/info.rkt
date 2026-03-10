#lang info

(define collection 'use-pkg-name)
(define pkg-desc "PNG: Read and Write Portable Network Graphics")

(define version "0.0.0.1")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define deps '("base" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc"))

;(define scribblings '(["tamer/png.scrbl" (main-doc) (parsing-library)]))
