#lang info

(define collection "Digimon-Gnome")

(define version "Baby")

(define build-deps '{"base" "math-lib" "typed-racket-lib" "scribble-lib" "dynext-lib"})

(define compile-omit-paths (list "stone"))
(define test-omit-paths 'all)

(define pkg-desc "The Meta-Project built for developers to make life simple")

