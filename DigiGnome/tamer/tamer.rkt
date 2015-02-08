#lang racket/base

(require "../digitama/tamer.rkt")

(provide (all-defined-out))
(provide (all-from-out "../digitama/tamer.rkt"))

(digimon-setenv digimon-gnome)

(define-values {rootdir zonedir dgvcdir stonedir tamerdir}
  (apply values (map (compose1 getenv (curry format "digimon-~a"))
                     (list "world" "zone" "digivice" "stone" "tamer"))))

{module+ makefile
  (provide (all-defined-out))
  
  (current-tamer-story (tamer-story->libpath "makefile.rkt"))
  (define tamer-partner (tamer-partner->filepath "../makefile.rkt"))}
