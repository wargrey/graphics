#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Geo-Anchor-Name (U Symbol Keyword))
(define-type Geo-Trusted-Anchors (U (Listof Geo-Anchor-Name) (-> Geo-Anchor-Name Boolean)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-anchor-trusted? : (-> Geo-Anchor-Name (Option Geo-Trusted-Anchors) Boolean)
  (lambda [anchor trusted-anchors]
    (or (not trusted-anchors)
        (if (list? trusted-anchors)
            (and (memq anchor trusted-anchors) #true)
            (trusted-anchors anchor)))))
