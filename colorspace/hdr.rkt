#lang typed/racket/base

(provide (all-defined-out))

(require racket/flonum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-hdr-exposure : (->* (Flonum) ((-> Flonum Flonum)) (-> Flonum Flonum))
  (lambda [e [ct-map hdr-tone-map/reinhard]]
    (define expfactor : Flonum (flexpt 2.0 e))
    
    (λ [[linear-value : Flonum]]
      (ct-map (* linear-value expfactor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define hdr-tone-map/reinhard : (-> Flonum Flonum)
  (lambda [c]
    (/ c (+ 1.0 c))))
