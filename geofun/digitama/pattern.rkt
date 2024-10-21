#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* geo-pattern-filter #:+> Geo-Pattern-Filter ; order matters
  geo-pattern-filter->integer integer->geo-pattern-filter
  [0 fast good best nearest bilinear gaussian])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-select-filter : (-> (Option Symbol) (-> Geo-Pattern-Filter) (Option Byte))
  (lambda [fltr fallback-fltr]
    (define seq (and fltr (geo-pattern-filter->integer fltr)))

    (or seq (geo-pattern-filter->integer (fallback-fltr)))))
