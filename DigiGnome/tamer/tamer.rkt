#lang racket/base

(require "../digitama/tamer.rkt")

(provide (all-defined-out))
(provide (all-from-out "../digitama/tamer.rkt"))

(current-digimon (digimon-gnome))

{module+ makefile
  (provide (all-defined-out))
  
  (current-tamer-story (tamer-story->libpath "makefile.rkt"))
  (define tamer-partner (tamer-partner->filepath "../makefile.rkt"))}
