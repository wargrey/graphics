#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define require-image : (-> String Symbol Positive-Real Any)
  (lambda [src.rkt id density]
    (define fallback (λ [] (call-with-values (λ [] (eval id (module->namespace src.rkt))) (λ _ (car _)))))
    (module-declared? src.rkt #true)
    (define value (dynamic-require src.rkt id fallback))
    (cond [(not (hash? value)) value]
          [else (hash-ref value (exact->inexact density)
                          (λ [] (let ([all (sort (assert (hash-keys value) (lambda ([l : (Listof Any)]) (andmap real? l))) >)])
                                  (if (pair? all) (hash-ref value (car all)) value))))])))
