#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/keyword)

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

(define geo-anchor->symbol : (-> Geo-Anchor-Name Symbol)
  (lambda [anchor]
    (cond [(symbol? anchor) anchor]
          [else (string->symbol (keyword->immutable-string anchor))])))

(define geo-anchor->string : (-> Geo-Anchor-Name String)
  (lambda [anchor]
    (if (symbol? anchor)
        (symbol->immutable-string anchor)
        (keyword->immutable-string anchor))))
