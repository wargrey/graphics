#lang typed/racket/base

(provide (all-defined-out))

(require racket/list)

(require "../tick/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-minor-grid-values : (-> Plot-Ticks (U Index (-> Real Real Index)) (-> Flonum Flonum) (Listof Flonum))
  (lambda [ticks grid-minor-count val->pos]
    (if (and (pair? ticks) (pair? (cdr ticks)))
        (for/fold ([ts : (Listof Flonum) null])
                  ([ptick (in-list ticks)]
                   [ctick (in-list (cdr ticks))])
          (define prev (plot-tick-value ptick))
          (define val (plot-tick-value ctick))
          (define mc (if (index? grid-minor-count) grid-minor-count (grid-minor-count prev val)))
          
          (if (> mc 0)
              (append ts
                      (let ([step (/ (- val prev) (+ mc 1))])
                        (for/list : (Listof Flonum) ([v (in-range (+ prev step) val step)])
                          (val->pos (real->double-flonum v)))))
              ts))
        null)))

(define plot-grid-values : (-> Plot-Ticks (U Index (-> Real Real Index)) (U Index (-> Real Real Index)) (-> Flonum Flonum)
                               (Values (Listof Flonum) (Listof Flonum)))
  (lambda [ticks minor-count grid-minor-count val->pos]
    (define-values (majors minors) (partition plot-tick-major? ticks))
    
    (values (for/list : (Listof Flonum) ([t (in-list majors)])
              (val->pos (real->double-flonum (plot-tick-value t))))
            
            (if (eq? grid-minor-count minor-count)
                (for/list : (Listof Flonum) ([t (in-list minors)])
                  (val->pos (real->double-flonum (plot-tick-value t))))
                (plot-minor-grid-values majors grid-minor-count val->pos)))))
