#lang info

(define collection "Wisemon")
(define version "Baby")

(define pkg-desc "The Development Environment of the Digital World")
(define pkg-authors '("WarGrey Ju"))

(define compile-omit-paths (list "stone"))
(define test-omit-paths 'all)

(define build-deps '("base" "math-lib" "typed-racket-lib" "scribble-lib" "rackunit-lib" "dynext-lib"
                            "pict-lib"))
