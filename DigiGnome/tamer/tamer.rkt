#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../digitama/tamer.rkt}

(provide (all-defined-out))
(provide (all-from-out "../digitama/tamer.rkt"))

(current-digimon (digimon-gnome))
