#lang scribble/text

#lang at-exp racket/base

(provide (all-defined-out))
(provide (except-out (all-from-out "../digitama/digicore.rkt" "../../DigiGnome/digitama/tamer.rkt")
                     exn:break:hang-up? exn:break:terminate?))

;;; To force makefile.rkt counting the required file
@|#\@|require{../digitama/digicore.rkt}
@|#\@|require{../../DigiGnome/digitama/tamer.rkt}

