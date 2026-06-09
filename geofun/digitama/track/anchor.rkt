#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)
(require racket/keyword)

(require racket/list)
(require racket/string)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-anchor-extract : (->* (Geo-Anchor-Name) (String) (Values String (Option Keyword)))
  (lambda [anchor [sep "#"]]
    (define text (geo-anchor->string anchor))
    (define has-hash? (string-contains? text sep))

    (cond [(not has-hash?) (values text #false)]
          [else (let ([tokens (string-split text sep)])
                  (define-values (cname stype)
                    (cond [(not (pair? tokens)) (values text #false)]
                          [(null? (cdr tokens)) (values "" (string-trim (car tokens)))]
                          [(null? (cddr tokens)) (values (car tokens) (string-trim (cadr tokens)))]
                          [else (let-values ([(cnames tag) (split-at-right tokens 1)])
                                  (values (string-join cnames sep) (string-trim (car tag))))]))
                  (values cname (and stype (string->keyword stype))))])))
