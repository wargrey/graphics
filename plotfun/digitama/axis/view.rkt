#lang typed/racket/base

(provide (all-defined-out))

(require "self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-lt-position : (case-> [Plot-Position-Transform (Pairof Real Real) (Pairof Real Real) Boolean -> Float-Complex]
                                   [Plot-Position-Transform Real Real Real Real Boolean -> Float-Complex])
  (case-lambda
    [(transform xmin ymin xmax ymax screen?)
     (if (not screen?)
         (transform (real->double-flonum xmin) (real->double-flonum ymax))
         (transform (real->double-flonum xmin) (real->double-flonum ymin)))]
    [(transform xview yview screen?)
     (if (not screen?)
         (transform (real->double-flonum (car xview)) (real->double-flonum (cdr yview)))
         (transform (real->double-flonum (car xview)) (real->double-flonum (car yview))))]))

(define plot-rb-position : (case-> [Plot-Position-Transform (Pairof Real Real) (Pairof Real Real) Boolean -> Float-Complex]
                                   [Plot-Position-Transform Real Real Real Real Boolean -> Float-Complex])
  (case-lambda
    [(transform xmin ymin xmax ymax screen?)
     (transform (real->double-flonum xmax) (real->double-flonum ymin))]
    [(transform xview yview screen?)
     (transform (real->double-flonum (cdr xview)) (real->double-flonum (car yview)))]))

(define plot-diagonal : (case-> [Plot-Position-Transform (Pairof Real Real) (Pairof Real Real) Boolean -> Float-Complex]
                                [Plot-Position-Transform Real Real Real Real Boolean -> Float-Complex])
  (case-lambda
    [(transform xmin ymin xmax ymax screen?)
     (- (plot-rb-position transform xmin ymin xmax ymax screen?)
        (plot-lt-position transform xmin ymin xmax ymax screen?))]
    [(transform xview yview screen?)
     (- (plot-rb-position transform xview yview screen?)
        (plot-lt-position transform xview yview screen?))]))

(define plot-vxunit : (case-> [Plot-Position-Transform -> Float-Complex]
                              [Plot-Position-Transform Real -> Float-Complex])
  (case-lambda
    [(transform) (plot-vxunit transform 0.0)]
    [(transform x0)
     (define flx (real->double-flonum x0))
     (- (transform (+ flx 1.0) 0.0)
        (transform flx 0.0))]))

(define plot-xunit : (case-> [Plot-Position-Transform -> Nonnegative-Flonum]
                             [Plot-Position-Transform Real -> Nonnegative-Flonum])
  (case-lambda
    [(transform) (magnitude (plot-vxunit transform 0.0))]
    [(transform x0) (magnitude (plot-vxunit transform x0))]))

(define plot-vyunit : (case-> [Plot-Position-Transform -> Float-Complex]
                              [Plot-Position-Transform Real -> Float-Complex])
  (case-lambda
    [(transform) (plot-vyunit transform 0.0)]
    [(transform y0)
     (define flx (real->double-flonum y0))
     (- (transform (+ flx 1.0) 0.0)
        (transform flx 0.0))]))

(define plot-yunit : (case-> [Plot-Position-Transform -> Nonnegative-Flonum]
                             [Plot-Position-Transform Real -> Nonnegative-Flonum])
  (case-lambda
    [(transform) (magnitude (plot-vyunit transform 0.0))]
    [(transform y0) (magnitude (plot-vyunit transform y0))]))
