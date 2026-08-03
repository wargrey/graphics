#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)
(require racket/string)

(require geofun/digitama/track/anchor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-identity-extract : (-> Geo-Anchor-Name (Values String (Option Keyword)))
  (lambda [id]
    (define text : String (geo-anchor->string id))
    (define has-hash? (regexp-match? #px"#" text))

    (cond [(not has-hash?) (values text #false)]
          [else (let ([tokens (string-split text #px"#")])
                  (define-values (cname stype)
                    (cond [(not (pair? tokens)) (values text #false)]
                          [(null? (cdr tokens)) (values "" (string-trim (car tokens)))]
                          [(null? (cddr tokens)) (values (car tokens) (string-trim (cadr tokens)))]
                          [else (let-values ([(cnames tag) (split-at-right tokens 1)])
                                  (values (string-join cnames "#") (string-trim (car tag))))]))
                  (values cname
                          (and stype
                               (and (non-empty-string? stype)
                                    (cond [(eq? (string-ref stype 0) #\.) #false]
                                          [else (string->keyword stype)])))))])))
