#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/flonum)

(require "../misc.rkt")

(define rgb->hue : (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum Flonum))
  (lambda [red green blue]
    (define-values (M m) (values (max red green blue) (min red green blue)))
    (define chroma : Flonum (- M m))
    (define hue : Flonum
      (cond [(zero? chroma)   +nan.0]
            [(fl= M green)    (fl* 60.0 (fl+ (fl/ (fl- blue red)   chroma) 2.0))]
            [(fl= M blue)     (fl* 60.0 (fl+ (fl/ (fl- red green)  chroma) 4.0))]
            [(fl< green blue) (fl* 60.0 (fl+ (fl/ (fl- green blue) chroma) 6.0))]
            [else             (fl* 60.0      (fl/ (fl- green blue) chroma))]))
    (values M m chroma hue)))

(define hue->rgb : (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum))
  (lambda [hue chroma m]
    (define hue/60 : Flonum (if (nan? hue) 6.0 (fl/ hue 60.0)))
    (define hue. : Integer (exact-floor hue/60))
    (define x : Flonum (* chroma (- 1 (abs (- (remainder hue. 2) (- hue. hue/60) 1))))) ; X = C(1-|H' mod 2 - 1|)
    (define-values (red green blue)
      (case hue.
        [(0) (values chroma x 0.0)]
        [(1) (values x chroma 0.0)]
        [(2) (values 0.0 chroma x)]
        [(3) (values 0.0 x chroma)]
        [(4) (values x 0.0 chroma)]
        [(5) (values chroma 0.0 x)]
        [else (values 0.0 0.0 0.0)]))
    (values (fl+ red m) (fl+ green m) (fl+ blue m))))

(define hsi-sector->rgb : (-> Flonum Flonum Flonum (U 'red 'green 'blue) (Values Flonum Flonum Flonum))
  (lambda [hue saturation intensity color]
    (define flcosH/cos60-H : Flonum
      (cond [(or (zero? hue) (fl= hue 120.0)) 2.0]
            [else (let ([H (fl* hue (fl/ pi 180.0))])
                    (fl/ (flcos H) (flcos (fl- (fl/ pi 3.0) H))))]))
    (define major : Flonum (fl* intensity (fl+ 1.0 (fl* saturation flcosH/cos60-H))))
    (define midor : Flonum (fl* intensity (fl- 1.0 saturation)))
    (define minor : Flonum (fl- (fl* 3.0 intensity) (fl+ major midor)))
    (case color
      [(red)   (values major minor midor)]
      [(green) (values midor major minor)]
      [else    (values minor midor major)])))
