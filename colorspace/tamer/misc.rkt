#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define color->name : (-> Natural Keyword)
  (lambda [color]
    (string->keyword (string-upcase (~r color #:base 16 #:min-width 6 #:pad-string "0")))))

(define color->flbyte : (-> Flonum Flonum)
  (lambda [color]
    (define flbyte : Flonum (* color 255.0))
    (/ (round (* flbyte 1000.0)) 1000.0)))
  
(define datum=? : (-> Flonum Flonum * Boolean)
  (lambda [src . res]
    (cond [(ormap nan? (cons src res)) (andmap nan? (cons src res))]
          [else (for/and ([r (in-list res)])
                  (ormap (λ [[m : Flonum]] (eq? (exact-round (* src m)) (exact-round (* r m))))
                         (list #;1000.0 100.0 10.0)))])))
