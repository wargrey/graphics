#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require "../misc.rkt")

(define rgb->hue : (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum Flonum))
  (lambda [red green blue]
    (define-values (M m) (values (max red green blue) (min red green blue)))
    (define chroma : Flonum (- M m))
    (define hue : Flonum
      (cond [(zero? chroma) +nan.0]
            [(= M green)    (* 60.0 (+ (/ (- blue red)   chroma) 2.0))]
            [(= M blue)     (* 60.0 (+ (/ (- red green)  chroma) 4.0))]
            [(< green blue) (* 60.0 (+ (/ (- green blue) chroma) 6.0))]
            [else           (* 60.0      (/ (- green blue) chroma))]))
    (values M m chroma hue)))

(define hue->rgb : (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum))
  (lambda [hue chroma m]
    (define hue/60 : Flonum (if (nan? hue) 6.0 (/ hue 60.0)))
    (define hue. : Integer (exact-floor hue/60))
    ; X = C(1-|H' mod 2 - 1|)
    (define x : Flonum (* chroma (- 1.0 (abs (- (exact->inexact (remainder hue. 2)) (- (exact->inexact hue.) hue/60) 1.0)))))
    (define-values (red green blue)
      (case hue.
        [(0) (values chroma x 0.0)]
        [(1) (values x chroma 0.0)]
        [(2) (values 0.0 chroma x)]
        [(3) (values 0.0 x chroma)]
        [(4) (values x 0.0 chroma)]
        [(5) (values chroma 0.0 x)]
        [else (values 0.0 0.0 0.0)]))
    (values (+ red m) (+ green m) (+ blue m))))

(define hsi-sector->rgb : (-> Flonum Flonum Flonum (U 'red 'green 'blue) (Values Flonum Flonum Flonum))
  (lambda [hue saturation intensity color]
    (define flcosH/cos60-H : Flonum
      (cond [(or (zero? hue) (= hue 120.0)) 2.0]
            [else (let ([H (* hue (/ pi 180.0))])
                    (/ (cos H) (cos (- (/ pi 3.0) H))))]))
    (define major : Flonum (* intensity (+ 1.0 (* saturation flcosH/cos60-H))))
    (define midor : Flonum (* intensity (- 1.0 saturation)))
    (define minor : Flonum (- (* 3.0 intensity) (+ major midor)))
    (case color
      [(red)   (values major minor midor)]
      [(green) (values midor major minor)]
      [else    (values minor midor major)])))
