#lang at-exp racket/base

(provide (all-defined-out))
(provide (all-from-out "../digitama/tamer.rkt"))

;;; To force makefile.rkt counting the required file
@require{../digitama/tamer.rkt}

(current-tongue 'English)

(module typed typed/racket
  (provide (all-defined-out))
  (provide (all-from-out (submod "../digitama/tamer.rkt" typed)))
  
  (require (submod "../digitama/tamer.rkt" typed)))

