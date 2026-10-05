#lang typed/racket

(require exprfun/matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx : (Vectorof (Vectorof Nonnegative-Fixnum))
  (build-vector 8 (λ [[r : Index]] ((inst make-vector Nonnegative-Fixnum) 8 0))))

(for ([idx (in-range 4)])
  (define row (vector-ref mtx (random (vector-length mtx))))
  (vector-set! row (random (vector-length row))
               (random #x1000000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mtx-entry-style : Mtx-Entry-Theme-Adjuster
  (lambda [style id dat indices]
    (when (index? dat)
      (remake-expr-slot-style style
                              ;#:font-paint 'transparent
                              #:fill-paint (if (> dat 0) dat 'WhiteSmoke)
                              #:opacity (/ dat #x1618034)))))

(define 2d-array
  (parameterize ([default-mtx-col-header-style (make-mtx-col-header-style #:font-paint 'green)]
                 [default-mtx-row-header-style (make-mtx-row-header-style #:font-paint 'blue)])
    ((inst $matrix Nonnegative-Fixnum) #:row-desc (λ [[r : Index]] (format "第 ~a 行\n行索引[~a]" r (sub1 r)))
                                       #:col-desc (λ [[c : Index]] (format "第 ~a 列\n列索引[~a]" c (sub1 c)))
                                       #:gap 8.0 #:hole? zero?
                                       #:desc (λ [dat style indices] #false) #:λstyle mtx-entry-style
                                       mtx 48 48)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  2d-array)
