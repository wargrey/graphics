#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-RGB-normalize : (-> Flonum Flonum Flonum (Values Flonum Flonum Flonum))
  (lambda [r g b]
    ; Yes, negative colors are acceptable
    (define L (max r g b))
    (values (/ r L) (/ g L) (/ b L))))

(define CIE-XYZ-normalize : (-> Flonum Flonum Flonum (Immutable-Vector Flonum Flonum Flonum) (Values Flonum Flonum Flonum))
  (lambda [X Y Z XYZn]
    (values (/ X (vector-ref XYZn 0))
            (/ Y (vector-ref XYZn 1))
            (/ Z (vector-ref XYZn 2)))))

;;; the f(t) used in the LAB RGB convertor 
(define CIE-lightness-transformation : (-> Flonum Flonum)
  (let* ([δ      6/29]
         [δ³     (* δ δ δ)]
         [δ⁻²/3  (/ (expt δ -2) 3)]
         [c      16/116])
    (lambda [t]
      (real->double-flonum
       (if (> t δ³)
           (expt t 1/3)
           (+ (* δ⁻²/3 t) c))))))

;;; the f⁻¹(t) used in the LAB RGB convertor
(define CIE-lightness-transformation⁻¹ : (-> Flonum Flonum)
  (let* ([δ   6/29]
         [3δ² (* 3 δ δ)]
         [c   4/29])
    (lambda [t]
      (real->double-flonum
       (if (> t δ)
           (expt t 3)
           (* 3δ² (- t c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-XYZ->xyY : (-> Flonum Flonum Flonum (Values Flonum Flonum))
  (lambda [X Y Z]
    (define L (+ X Y Z))
    
    (values (/ X L)
            (/ Y L))))

(define CIE-xyY->XYZ : (->* (Flonum Flonum) (Flonum) (Values Flonum Flonum Flonum))
  (lambda [x y [luminance 1.0]]
    (define z (- 1.0 x y))
    
    (values (* (/ x y) luminance)
            luminance
            (* (/ z y) luminance))))


(define CIE-LCh->Lab : (-> Flonum Flonum (Values Flonum Flonum))
  (lambda [C h]
    (cond [(or (zero? C) (nan? h)) (values 0.0 0.0)]
          [else (let ([polar (make-polar C (degrees->radians h))])
                  (values (real-part polar) (imag-part polar)))])))

(define CIE-Lab->LCh : (-> Flonum Flonum Flonum (Values Flonum Flonum))
  (λ [a b epsilon]
    (define polar (make-rectangular a b))
    (define chroma (magnitude polar))
    (define hue
      (let ([deg (radians->degrees (angle polar))])
        (cond [(<= chroma epsilon) +nan.0]
              [(< deg 0.0) (+ deg 360.0)]
              [else deg])))
    
    (values chroma hue)))
