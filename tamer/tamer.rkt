#lang at-exp racket/base

;;; To force makefile.rkt counting the required file
@require{../digitama/tamer.rkt}
@require{../digitama/ssh.rkt}

(provide (all-defined-out))
(provide (all-from-out "../digitama/tamer.rkt"))
