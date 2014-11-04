#lang at-exp racket

(require pict)
(require images/flomap)

(require "composition.rkt")

(define nanomon (recv-digimon "/images/7/7f/Nanomon.jpg"))
(define rallies (list (cons 241 147)
                      (cons 200 156)
                      (cons 57 135)
                      (cons 86 64)))

(digimon-ark! nanomon #:lightness 0.64 #:show? #true #:rallies rallies)

(flomap->bitmap nanomon)

