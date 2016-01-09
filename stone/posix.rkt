#lang scribble/text

@require{../digitama/digicore.rkt}

#lang at-exp racket/base

(provide (all-defined-out))
(provide (all-from-out "../../@(digimon-gnome)/digitama/posix.rkt"))

;;; To force makefile.rkt counting the required file
@|#\@|require{../../@(digimon-gnome)/digitama/posix.rkt}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* typed/ffi typed/racket
  (provide (all-from-out (submod "../../@(digimon-gnome)/digitama/posix.rkt" typed/ffi)))

  (require (submod "../../@(digimon-gnome)/digitama/posix.rkt" typed/ffi)))
