#lang scribble/text

@(require "../digitama/digicore.rkt")

#lang info

(define collection "@(getenv "info.collection")")

(define version "Baby")

(define pkg-desc "@(getenv "info.pkg-desc")")

(define compile-omit-paths (list "stone"))
(define test-omit-paths 'all)

(define racket-launcher-names (list "@(current-digimon)"))
(define racket-launcher-libraries (list "digivice/@(current-digimon).rkt"))
