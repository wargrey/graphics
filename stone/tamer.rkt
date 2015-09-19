#lang scribble/text

#lang at-exp racket/base

(provide (all-defined-out))
(provide (all-from-out "../digitama/digicore.rkt"))
(provide (all-from-out "../../DigiGnome/digitama/tamer.rkt"))

;;; To force makefile.rkt counting the required file
@|#\@|require{../digitama/digicore.rkt}
@|#\@|require{../../DigiGnome/digitama/tamer.rkt}
