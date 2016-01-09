#lang scribble/text

@require{../digitama/digicore.rkt}

#lang at-exp racket/base

(provide (all-defined-out))
(provide (except-out (all-from-out "../digitama/digicore.rkt" "../../@(digimon-gnome)/digitama/tamer.rkt")
                     exn:break:hang-up? exn:break:terminate?))

;;; To force makefile.rkt counting the required file
@|#\@|require{../digitama/digicore.rkt}
@|#\@|require{../../@(digimon-gnome)/digitama/tamer.rkt}

(current-tongue 'English)

(module typed typed/racket
  (provide (all-defined-out))
  (provide (all-from-out (submod "../../@(digimon-gnome)/digitama/tamer.rkt" typed)))
  
  (require (submod "../../@(digimon-gnome)/digitama/tamer.rkt" typed)))

