#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Point2D (U Complex (Pairof Real Real) (List Real Real)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~point2d : (-> Point2D Float-Complex)
  (lambda [dt]
    (define-values (x y) (point2d-values dt))
    (make-rectangular x y)))

(define ~point2ds : (->* ((Listof Point2D)) (Real Real Point2D) (Values (Listof Float-Complex) Flonum Flonum Flonum Flonum))
  (lambda [raws [dx 0.0] [dy 0.0] [scale 1.0]]
    (define xoff : Flonum (real->double-flonum dx))
    (define yoff : Flonum (real->double-flonum dy))
    (define-values (fx fy) (point2d-scale-values scale))
    (define-values (afx afy) (values (abs fx) (abs fy)))
    (define-values (xflip? yflip?) (values (< fx 0.0) (< fy 0.0)))
    
    (let normalize ([dots : (Listof Point2D) raws]
                    [stod : (Listof Float-Complex) null]
                    [lx : Flonum +inf.0]
                    [ty : Flonum +inf.0]
                    [rx : Flonum -inf.0]
                    [by : Flonum -inf.0])
      (cond [(pair? dots)
             (let*-values ([(self rest) (values (car dots) (cdr dots))]
                           [(x0 y0) (point2d-values self)]
                           [(x y) (values (+ (* x0 afx) xoff) (+ (* y0 afy) yoff))])
               (normalize rest
                          (cons (make-rectangular x y) stod)
                          (min lx x) (min ty y)
                          (max rx x) (max by y)))]
            [(or xflip? yflip?)
             (let flip ([stod : (Listof Float-Complex) stod]
                        [dots : (Listof Float-Complex) null])
               (if (pair? stod)
                   (flip (cdr stod)
                         (let ([x (real-part (car stod))]
                               [y (imag-part (car stod))])
                           (cons (make-rectangular (if xflip? (- rx (- x lx)) x)
                                                   (if yflip? (- by (- y ty)) y))
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

(define point2d->window : (-> Point2D Flonum Flonum Flonum Flonum (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Boolean Boolean))
  (lambda [hint lx ty rx by]
    (define-values (w h) (point2d-values hint))

    (define-values (xoff width x-stroke?)
      (cond [(= w 0.0) (values 0.0 (max rx 0.0) #true)]
            [(> w 0.0) (values 0.0 w #false)]
            [(< w 0.0) (values (- w) (max (- rx w) 0.0) #false)]
            [else (values (- lx) (max (- rx lx) 0.0) #true)]))
    
    (define-values (yoff height y-stroke?)
      (cond [(= h 0.0) (values 0.0 (max by 0.0) #true)]
            [(> h 0.0) (values 0.0 h #false)]
            [(< h 0.0) (values (- h) (max (- by h) 0.0) #false)]
            [else (values (- ty) (max (- by ty) 0.0) #true)]))

    (values xoff yoff width height x-stroke? y-stroke?)))

(define path-window-adjust : (-> Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum Boolean Boolean
                                 (Values Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [dx dy flw flh thickness x-stroke? y-stroke?]
    (if (and (or x-stroke? y-stroke?) (> thickness 0.0))
        (let ([inset (* thickness 0.5)])
          (values (if (and x-stroke?) (+ dx inset) dx)
                  (if (and y-stroke?) (+ dy inset) dy)
                  (if (and x-stroke?) (+ flw thickness) flw)
                  (if (and y-stroke?) (+ flh thickness) flh)))
        (values dx dy flw flh))))
