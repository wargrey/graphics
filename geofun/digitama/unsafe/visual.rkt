#lang typed/racket/base

(provide (all-defined-out))

(require file/convertible)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Visual-Object-Convert (-> Visual-Object<%> Symbol Any Any))

(struct rich-datum<%> () #:type-name Rich-Datum<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define vobject-convert : Visual-Object-Convert
  (let ([sentry (gensym 'convert)])
    (λ [[self : Visual-Object<%>] [mime : Symbol] [fallback : Any]]
      (define self-convert (visual-object<%>-convert self))

      (cond [(not self-convert) fallback]
            [else (let ([maybe-datum (self-convert self mime sentry)])
                    (cond [(eq? maybe-datum sentry) fallback]
                          [(visual-object<%>? maybe-datum) (vobject-convert maybe-datum mime fallback)]
                          [else maybe-datum]))]))))

(struct visual-object<%> rich-datum<%>
  ([convert : (Option Visual-Object-Convert)])
  #:type-name Visual-Object<%>
  #:property prop:convertible vobject-convert)
