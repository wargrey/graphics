#lang typed/racket/base

(provide (all-defined-out))

(require racket/flonum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ∫Yλdλ : Flonum 106.856895)

(define spectrum-sample-average : (-> FlVector FlVector Flonum Flonum Flonum)
  (lambda [λs vs λstart λend]
    (define n : Index (min (flvector-length λs) (flvector-length vs)))
    
    (define (interp [λself : Flonum] [i : Index]) : Flonum
      (define λ0 (flvector-ref λs i))
      (lerp (real->double-flonum (/ (- λself λ0)
                                    (- (flvector-ref λs (add1 i)) λ0)))
            (flvector-ref vs i) (flvector-ref vs (add1 i))))
    
    (cond [(= n 0) 0.0]
          [(= n 1) (flvector-ref vs 0)]
          [(<= λend (flvector-ref λs 0)) (flvector-ref vs 0)]
          [else (let locate-segment0 ([i : Nonnegative-Fixnum 0]
                                      [n-1 : Index (sub1 n)])
                  (cond [(>= i n) (flvector-ref vs n-1)]
                        [(> λstart (flvector-ref λs (add1 i))) (locate-segment0 (add1 i) n-1)]
                        [else (let avg ([sum : Flonum (+ (if (< λstart (flvector-ref λs 0)) (* (flvector-ref vs 0) (- (flvector-ref λs 0) λstart)) 0.0)
                                                         (if (> λend (flvector-ref λs n-1)) (* (flvector-ref vs n-1) (- λend (flvector-ref λs n-1))) 0.0))]
                                        [idx : Nonnegative-Fixnum i])
                                (if (and (< idx n-1) (>= λend (flvector-ref λs idx)))
                                    (let* ([idx+1 (add1 idx)]
                                           [seg-sλ (max λstart (flvector-ref λs idx))]
                                           [seg-eλ (min λend (flvector-ref λs idx+1))])
                                      (avg (+ sum (* 0.5
                                                     (+ (interp seg-sλ idx) (interp seg-eλ idx))
                                                     (- seg-eλ seg-sλ)))
                                           idx+1))
                                    (/ sum (- λend λstart))))]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lerp : (-> Flonum Flonum Flonum Flonum)
  (lambda [t lv rv]
    (+ (* lv (- 1.0 t))
       (* rv t))))
