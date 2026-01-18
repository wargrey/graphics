#lang info

(define collection 'multi)
(define pkg-desc "A typed technical graphics family for Racket: essentials, diagrams, plots, and more...")
(define pkg-authors '(wargrey))
(define test-omit-paths 'all)

(define version "1.2.0")
(define deps '("base" "digimon" "math-lib" "draw-lib" "typed-racket-lib" "typed-racket-more"))
(define build-deps '("scribble-lib" "racket-doc" "typed-racket-doc" "digimon"))

