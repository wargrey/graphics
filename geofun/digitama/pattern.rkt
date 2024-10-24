#lang typed/racket/base

(provide (all-defined-out))

(require digimon/enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-enumeration* geo-pattern-filter #:+> Geo-Pattern-Filter ; order matters
  geo-pattern-filter->integer integer->geo-pattern-filter
  [0 fast good best nearest bilinear gaussian])

(define-enumeration* geo-pattern-extend #:+> Geo-Pattern-Extend ; order matters
  geo-pattern-extend->integer integer->geo-pattern-extend
  [0 none repeat reflect pad])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-select-filter : (-> (Option Symbol) (-> Geo-Pattern-Filter) Byte)
  (lambda [fltr fallback-fltr]
    (define seq (and fltr (geo-pattern-filter->integer fltr)))

    (or seq (geo-pattern-filter->integer (fallback-fltr)))))

(define geo-select-extend : (-> (Option Symbol) (-> Geo-Pattern-Extend) Byte)
  (lambda [ext fallback-ext]
    (define seq (and ext (geo-pattern-extend->integer ext)))

    (or seq (geo-pattern-extend->integer (fallback-ext)))))
