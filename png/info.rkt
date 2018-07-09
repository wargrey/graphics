#lang info

(define collection 'use-pkg-name)
(define pkg-desc "PNG: Read and Write Portable Network Graphics")

(define deps '("base" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "1.0")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define scribblings '(["tamer/png.scrbl" (main-doc) (parsing-library)]))
