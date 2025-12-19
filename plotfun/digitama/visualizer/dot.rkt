#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/geometry/dot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ~y-bounds : (case-> [(-> Real (Option Number)) (Listof Real) -> (Pairof Real Real)]
                            [(Listof Complex) Real Real -> (Pairof Real Real)])
  (case-lambda
    [(f xs)
     (for/fold ([ymin : Real +inf.0]
                [ymax : Real -inf.0]
                #:result (cons ymin ymax))
               ([x (in-list xs)])
       (define y (f x))
       (if (rational? y)
           (values (min ymin y) (max ymax y))
           (values ymin         ymax)))]
    [(pts xmin xmax)
     (let bound ([ymin : Real +inf.0]
                 [ymax : Real -inf.0]
                 [pts : (Listof Complex) pts])
       (if (pair? pts)
           (let*-values ([(pt rest) (values (car pts) (cdr pts))]
                         [(flx y) (values (real-part pt) (imag-part pt))])
             (cond [(> flx xmax)  (cons ymin ymax)]
                   [(< flx xmin)  (bound ymin ymax rest)]
                   [(rational? y) (bound (min ymin y) (max ymax y) rest)]
                   [else          (bound ymin         ymax rest)]))
           (cons ymin ymax)))]))

(define ~cartesian2ds : (case-> [(-> Real (Option Number)) (Listof Real) Real Real (-> Flonum Flonum Float-Complex)
                                                           -> (Values (Listof Float-Complex) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum)]
                                [(Listof Complex) Real Real Real Real (-> Flonum Flonum Float-Complex)
                                                  -> (Values (Listof Float-Complex) Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum)])
  (case-lambda
    [(f xs ymin ymax transform)
     (let ([flmin (real->double-flonum ymin)]
           [flmax (real->double-flonum ymax)])
       (let normalize ([xs : (Listof Real) xs]
                       [stod : (Listof Float-Complex) null]
                       [lx : Flonum +inf.0]
                       [ty : Flonum +inf.0]
                       [rx : Flonum -inf.0]
                       [by : Flonum -inf.0])
         (if (pair? xs)
             (let*-values ([(flx rest) (values (car xs) (cdr xs))]
                           [(y) (f flx)]
                           [(fly) (if (rational? y) (real->double-flonum y) +nan.0)])
               (if (<= flmin fly flmax)
                   (let*-values ([(dot) (transform (real->double-flonum flx) fly)]
                                 [(scr-x scr-y) (values (real-part dot) (imag-part dot))])
                     (normalize rest (cons dot stod)
                                (min lx scr-x) (min ty scr-y)
                                (max rx scr-x) (max by scr-y)))
                   (normalize rest (point2ds-nan-cons stod) lx ty rx by)))
             (point2ds-rational-reverse stod lx ty rx by))))]
    [(pts xmin xmax ymin ymax transform)
     (let ([xflmin (real->double-flonum xmin)]
           [xflmax (real->double-flonum xmax)]
           [yflmin (real->double-flonum ymin)]
           [yflmax (real->double-flonum ymax)])
       (let normalize ([pts : (Listof Complex) pts]
                       [stod : (Listof Float-Complex) null]
                       [lx : Flonum +inf.0]
                       [ty : Flonum +inf.0]
                       [rx : Flonum -inf.0]
                       [by : Flonum -inf.0])
         (if (pair? pts)
             (let*-values ([(self rest) (values (car pts) (cdr pts))]
                           [(flx) (real->double-flonum (real-part self))]
                           [(fly) (real->double-flonum (imag-part self))])
               (cond [(or (< flx xflmin) (> flx xflmax))
                      (normalize rest stod lx ty rx by)]
                     [(<= yflmin fly yflmax)
                      (let*-values ([(dot) (transform flx fly)]
                                    [(scr-x scr-y) (values (real-part dot) (imag-part dot))])
                        (normalize rest (cons dot stod)
                                   (min lx scr-x) (min ty scr-y)
                                   (max rx scr-x) (max by scr-y)))]
                     [else (normalize rest (point2ds-nan-cons stod) lx ty rx by)]))
               (point2ds-rational-reverse stod lx ty rx by))))]))
