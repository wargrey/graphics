#lang typed/racket/base

(provide (all-defined-out))

(require "interface.rkt")
(require "../marker/self.rkt")
(require "../marker/guard.rkt")

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (plot-function-mark-guard stx)
  (syntax-case stx []
    [(_ fx label [argl ...])
     (syntax/loc stx
       (cond [(plot:mark? label)
              (let* ([origin (plot:mark-point label)]
                     [dot (plot-mark-point-guard origin fx argl ...)])
                (cond [(not dot) #false]
                      [(eq? origin dot) label]
                      [(pair? dot) (remake-plot:mark label #:point (car dot) #:parameter (cdr dot))]
                      [else (remake-plot:mark label #:point dot)]))]
             [(or label)
              (let ([dot (plot-mark-point-guard +nan.0 fx argl ...)])
                (and dot (let ([body (if (eq? label 'name) (format "~a" (object-name fx)) label)])
                           (cond [(complex? dot) (plot-label body #:at dot)]
                                 [else (plot-label body #:at (car dot) #:parameter (cdr dot))]))))]
             [else #false]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plot-function-pin-angle : (-> (U Real (-> Real (Option Real))) (U Real (-> Real (Option Real)))
                                      Plot-Visualizer-Label-Placement
                                      (-> Real (Option Real)))
  (lambda [df/dx ddf/dxx placement]
    (define (c [x : Real]) : Real
      (cond [(real? ddf/dxx) ddf/dxx]
            [else (or (ddf/dxx x) 0)]))
    
    (λ [[param : Real]]
      (let ([+k (if (real? df/dx) df/dx (df/dx param))])
        (and (rational? +k)
             (let ([-k (- +k)])
               ;;; NOTE
               ; the facility that draws the pin line has no knownledge about the cartesian system,
               ; so that the resulting angle should be symmetrical about the x-axis
               ; if the angle is computed mathematically.
               (- (cond [(eq? placement 'auto) (if (>= (c param) 0) (atan -1 +k) (atan +1 -k))]
                        [(eq? placement 'flip) (if (>= (c param) 0) (atan +1 -k) (atan -1 +k))]
                        [(eq? placement 'left) (atan -1 +k)]
                        [else                  (atan +1 -k)]))))))))
