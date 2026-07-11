#lang typed/racket/base

(provide (all-defined-out))

;;; NOTE
; All matrice here are based on experience.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CMYK-Ink-Matrix
  (Immutable-Vector Flonum Flonum Flonum
                    Flonum Flonum Flonum
                    Flonum Flonum Flonum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; US SWOP
;; the ink looks dirty and full of impurities
;; resulting colors look much more darken. 
(define cmyk-matrix-swop : CMYK-Ink-Matrix
  (vector-immutable 1.00 0.15 0.00
                    0.42 1.00 0.06
                    0.14 0.57 1.00))

;;; Euroscale / FOGRA
;; Medium
(define cmyk-matrix-euroscale : CMYK-Ink-Matrix
  (vector-immutable 1.00 0.13 0.00
                    0.33 1.00 0.05
                    0.11 0.48 1.00))

;;; Japan
;; Brightest
(define cmyk-matrix-japan : CMYK-Ink-Matrix
  (vector-immutable 1.00 0.10 0.00
                    0.30 1.00 0.05
                    0.10 0.45 1.00))

