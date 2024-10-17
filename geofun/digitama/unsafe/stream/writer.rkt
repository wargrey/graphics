#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(module unsafe racket/base
  (provide (all-defined-out))

  (require ffi/unsafe)
  (require racket/unsafe/ops)
  (require racket/draw/unsafe/cairo)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define make-cairo-vector-surface-writer
    (lambda [/dev/vecout pool-size]
      (define pool (make-bytes pool-size))
      
      (λ [bstr-ptr len]
        (let cairo-write ([src-rst len]
                          [ptr-off 0])
          (define size (unsafe-fxmin src-rst pool-size))
          (define rest-- (unsafe-fx- src-rst size))
          
          (memcpy pool 0 bstr-ptr ptr-off size)
          (write-bytes pool /dev/vecout 0 size)
          
          (if (unsafe-fx> rest-- 0)
              (cairo-write rest-- (unsafe-fx+ ptr-off size))
              CAIRO_STATUS_SUCCESS))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cairo-Vector-Surface-Writer (-> Bytes Index Index))

(unsafe-require/typed/provide
 (submod "." unsafe)
 [make-cairo-vector-surface-writer (-> Output-Port Positive-Index Cairo-Vector-Surface-Writer)])
