#lang scribble/text

#lang at-exp racket/base

(provide (all-defined-out))
(provide (all-from-out "../../DigiGnome/digitama/posix.rkt"))

;;; To force makefile.rkt counting the required file
@|#\@|require{digicore.rkt}
@|#\@|require{../../DigiGnome/digitama/posix.rkt}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module* typed/ffi typed/racket
  (provide (all-from-out (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi)))

  (require (submod "../../DigiGnome/digitama/posix.rkt" typed/ffi)))

