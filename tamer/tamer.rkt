#lang at-exp racket/base

(provide (all-defined-out))
(provide (except-out (all-from-out "../digitama/tamer.rkt")
                     exn:break:hang-up? exn:break:terminate?))

;;; To force makefile.rkt counting the required file
@require{../digitama/tamer.rkt}
@require{../digitama/i18n.rkt}

(current-tongue 'English)

