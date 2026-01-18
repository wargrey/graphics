#lang info

(define collection 'use-pkg-name)
(define pkg-desc "PNG: Read and Write Portable Network Graphics")

(define deps '("base" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc"))

(define version "0.0.1")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define msvc-sdk-root "C:\\Program Files (x86)\\Windows Kits\\10")
(define msvc-sdk-library "10.0.17763.0")

(define scribblings '(["tamer/png.scrbl" (main-doc) (parsing-library)]))
