#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Point2D (U Complex (Pairof Real Real) (List Real Real)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~point2d : (-> Point2D Float-Complex)
  (lambda [dt]
    (define-values (x y) (point2d-values dt))
    (make-rectangular x y)))

(define ~point2ds : (->* ((Listof Point2D)) (Real Real Point2D) (Values (Listof (Pairof Char Float-Complex)) Flonum Flonum Flonum Flonum))
  (lambda [raws [dx 0.0] [dy 0.0] [scale 1.0]]
    (define xoff : Flonum (real->double-flonum dx))
    (define yoff : Flonum (real->double-flonum dy))
    (define-values (fx fy) (point2d-scale-values scale))
    (define-values (afx afy) (values (abs fx) (abs fy)))
    (define-values (xflip? yflip?) (values (< fx 0.0) (< fy 0.0)))
    
    (let normalize ([dots : (Listof Point2D) raws]
                    [stod : (Listof (Pairof Char Float-Complex)) null]
                    [lx : Flonum +inf.0]
                    [ty : Flonum +inf.0]
                    [rx : Flonum -inf.0]
                    [by : Flonum -inf.0])
      (cond [(pair? dots)
             (let*-values ([(self rest) (values (car dots) (cdr dots))]
                           [(x0 y0) (point2d-values self)]
                           [(x y) (values (+ (* x0 afx) xoff) (+ (* y0 afy) yoff))])
               (normalize rest
                          (cons (cons #\L (make-rectangular x y)) stod)
                          (min lx x) (min ty y)
                          (max rx x) (max by y)))]
            [(or xflip? yflip?)
             (let flip ([stod : (Listof (Pairof Char Float-Complex)) stod]
                        [dots : (Listof (Pairof Char Float-Complex)) null])
               (if (pair? stod)
                   (flip (cdr stod)
                         (let* ([op+dot (car stod)]
                                [x (real-part (cdr op+dot))]
                                [y (imag-part (cdr op+dot))])
                           (cons (cons (car op+dot)
                                       (make-rectangular (if xflip? (- rx (- x lx)) x)
                                                         (if yflip? (- by (- y ty)) y)))
                                 dots)))
                   (values dots lx ty rx by)))]
            [else (values (reverse stod) lx ty rx by)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define point2d-values : (-> Point2D (Values Flonum Flonum))
  (lambda [dt]
    (cond [(real? dt) (values (real->double-flonum dt) 0.0)]
          [(list? dt) (values (real->double-flonum (car dt)) (real->double-flonum (cadr dt)))]
          [(pair? dt) (values (real->double-flonum (car dt)) (real->double-flonum (cdr dt)))]
          [else (values (real->double-flonum (real-part dt)) (real->double-flonum (imag-part dt)))])))

(define point2d-scale-values : (-> Point2D (Values Flonum Flonum))
  (lambda [st]
    (define-values (sx0 sy0) (point2d-values st))
    (define sx (if (= sx0 0.0) 1.0 sx0))
    (define sy (if (= sy0 0.0) sx sy0))

    (values sx sy)))

(define point2d->window : (-> Point2D Flonum Flonum Flonum Flonum (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [hint lx ty rx by]
    (define-values (w h) (point2d-values hint))

    (define-values (xoff width)
      (cond [(= w 0.0) (values 0.0 (max rx 0.0))]
            [(> w 0.0) (values 0.0 w)]
            [else (values (- lx) (max (- rx lx) 0.0))]))
    
    (define-values (yoff height)
      (cond [(= h 0.0) (values 0.0 (max by 0.0))]
            [(> h 0.0) (values 0.0 h)]
            [else (values (- ty) (max (- by ty) 0.0))]))

    (values xoff yoff width height)))
