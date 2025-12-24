#lang typed/racket

(require diafun/matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx : (Vectorof (Vectorof Nonnegative-Fixnum))
  (build-vector 8 (λ [[r : Index]] ((inst make-vector Nonnegative-Fixnum) 8 0))))

(for ([idx (in-range 4)])
  (define row (vector-ref mtx (random (vector-length mtx))))
  (vector-set! row (random (vector-length row))
               (random #x1000000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx-mask-style : (-> Any Mtx-Indices Mtx-Mask-Style)
  (lambda [dat indices]
    (make-mtx-mask-style #:stroke-width 1.0
                         #:fill-paint (if (and (index? dat) (> dat 0)) dat 'WhiteSmoke))))

(define 2d-array
  (parameterize ([default-mtx-col-header-font-paint 'green]
                 [default-mtx-row-header-font-paint 'blue])
    ((inst dia-matrix Nonnegative-Fixnum) #:row-desc (λ [[r : Index]] (format "第 ~a 行\n行索引[~a]" r (sub1 r)))
                                          #:col-desc (λ [[c : Index]] (format "第 ~a 列\n列索引[~a]" c (sub1 c)))
                                          #:col-header-rotate -1.57 #:gap 8.0 #:mask? #true
                                          #:λmask-style mtx-mask-style
                                          mtx 48 48)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  2d-array)
