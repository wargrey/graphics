#lang info

(define collection "Digimon-Gnome")

(define version "Baby")

(define build-deps '{"base" "at-exp-lib" "scribble-lib"})

(define compile-omit-paths (list "makefile.rkt" "submake.rkt" "info.rkt" "stone" "tamer"))
(define test-omit-paths 'all)

(define pkg-desc "DigiGnome is built for developers to make life simple.")
